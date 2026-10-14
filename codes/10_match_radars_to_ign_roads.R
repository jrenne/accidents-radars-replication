#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(curl)
  library(data.table)
  library(jsonlite)
})

root <- normalizePath(".")
processed <- file.path(root, "data", "processed")
out_dir <- file.path(root, "results", "current")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- file.path(out_dir, "radar_ign_road_matches.csv")
diagnostic_file <- file.path(out_dir, "radar_ign_road_match_diagnostics.csv")
max_radars <- as.integer(Sys.getenv("MAX_RADARS", "999999"))
search_radius_m <- 350
maximum_match_distance_m <- 150

normalize_route <- function(x) {
  x <- toupper(gsub("[^A-Z0-9]", "", fifelse(is.na(x), "", x)))
  x <- sub("^RD", "D", x)
  x <- sub("^RN", "N", x)
  x
}

point_to_line_m <- function(latitude, longitude, coordinates) {
  xy <- t(vapply(
    coordinates,
    function(point) as.numeric(unlist(point))[1:2],
    numeric(2)
  ))
  if (nrow(xy) < 2) return(Inf)
  x <- (xy[, 1] - longitude) * 111320 * cos(latitude * pi / 180)
  y <- (xy[, 2] - latitude) * 110574
  ax <- x[-length(x)]
  ay <- y[-length(y)]
  bx <- x[-1]
  by <- y[-1]
  dx <- bx - ax
  dy <- by - ay
  denominator <- dx^2 + dy^2
  t <- ifelse(denominator > 0, pmax(0, pmin(1, -(ax * dx + ay * dy) / denominator)), 0)
  min(sqrt((ax + t * dx)^2 + (ay + t * dy)^2))
}

fetch_features <- function(latitude, longitude) {
  latitude_delta <- search_radius_m / 110574
  longitude_delta <- search_radius_m / (111320 * cos(latitude * pi / 180))
  bbox <- paste(
    longitude - longitude_delta, latitude - latitude_delta,
    longitude + longitude_delta, latitude + latitude_delta,
    "CRS:84", sep = ","
  )
  query <- paste0(
    "https://data.geopf.fr/wfs/ows?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature",
    "&TYPENAMES=BDTOPO_V3%3Atroncon_de_route",
    "&OUTPUTFORMAT=application%2Fjson&SRSNAME=CRS%3A84&COUNT=1000&BBOX=",
    URLencode(bbox, reserved = TRUE)
  )
  last_error <- NULL
  for (attempt in 1:3) {
    response <- tryCatch(curl_fetch_memory(query), error = function(e) e)
    if (!inherits(response, "error") && response$status_code == 200L) {
      return(fromJSON(rawToChar(response$content), simplifyVector = FALSE)$features)
    }
    last_error <- if (inherits(response, "error")) conditionMessage(response) else
      paste("HTTP", response$status_code)
    Sys.sleep(attempt)
  }
  stop(last_error)
}

extract_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x)) return(default)
  as.character(x[[1]])
}

message("Préparation des radars ponctuels 2012-2024...")
radars <- fread(file.path(processed, "radars.csv.gz"))[
  !is.na(latitude) & !is.na(longitude)
]
radars[, `:=`(
  date_2025 = as.IDate(date_mise_en_service),
  date_2018 = as.IDate(date_installation_2018),
  current_point = type_radar_code %chin% c("ETF", "ETD", "ETT", "ETU"),
  old_point = type_radar_libelle_2018 %chin% c("Radar fixe", "Radar discriminant")
)]
radars[, point_speed := current_point | (actif_liste_2025 == 0L & old_point)]
radars <- radars[point_speed == TRUE]
radars[, installation_date := as.IDate(NA)]
radars[current_point == TRUE, installation_date := date_2025]
radars[current_point == TRUE & old_point == TRUE & !is.na(date_2018),
       installation_date := fifelse(is.na(date_2025), date_2018, pmin(date_2025, date_2018))]
radars[actif_liste_2025 == 0L & old_point == TRUE, installation_date := date_2018]
radars <- radars[!is.na(installation_date)]
radars[, installation_year := as.integer(format(installation_date, "%Y"))]
radars <- radars[installation_year %between% c(2012L, 2024L)]
radars[, route_normalisee := normalize_route(route_2018)]
setorder(radars, cle_radar)

existing <- if (file.exists(output_file)) fread(output_file) else data.table()
done <- if (nrow(existing)) existing$cle_radar else character()
todo <- radars[!cle_radar %chin% done]
if (nrow(todo) > max_radars) todo <- todo[seq_len(max_radars)]

message("Radars à interroger : ", nrow(todo), " (déjà en cache : ", length(done), ")")
new_results <- vector("list", nrow(todo))

for (i in seq_len(nrow(todo))) {
  radar <- todo[i]
  features <- tryCatch(fetch_features(radar$latitude, radar$longitude), error = function(e) e)
  if (inherits(features, "error")) {
    new_results[[i]] <- data.table(
      cle_radar = radar$cle_radar, installation_year = radar$installation_year,
      latitude = radar$latitude, longitude = radar$longitude,
      route_2018 = radar$route_2018, route_normalisee = radar$route_normalisee,
      match_status = "wfs_error", match_method = NA_character_, road_number = NA_character_,
      road_id = NA_character_, distance_to_road_m = NA_real_, road_importance = NA_character_,
      road_urban = NA, road_speed_mean = NA_real_, error_message = conditionMessage(features)
    )
  } else {
    feature_rows <- rbindlist(lapply(features, function(feature) {
      if (is.null(feature$geometry) || feature$geometry$type != "LineString") return(NULL)
      properties <- feature$properties
      data.table(
        road_number = normalize_route(extract_scalar(properties$cpx_numero, "")),
        road_id = extract_scalar(properties$cleabs),
        distance_to_road_m = point_to_line_m(
          radar$latitude, radar$longitude, feature$geometry$coordinates
        ),
        road_importance = extract_scalar(properties$importance),
        road_urban = as.logical(extract_scalar(properties$urbain)),
        road_speed_mean = as.numeric(extract_scalar(properties$vitesse_moyenne_vl)),
        coordinates = list(feature$geometry$coordinates)
      )
    }), fill = TRUE)

    numbered <- feature_rows[nzchar(road_number)]
    exact <- if (nzchar(radar$route_normalisee))
      numbered[road_number == radar$route_normalisee] else numbered[0]
    pool <- if (nrow(exact)) exact else numbered
    method <- if (nrow(exact)) "exact_route_number" else "nearest_numbered_road"

    if (!nrow(pool)) {
      new_results[[i]] <- data.table(
        cle_radar = radar$cle_radar, installation_year = radar$installation_year,
        latitude = radar$latitude, longitude = radar$longitude,
        route_2018 = radar$route_2018, route_normalisee = radar$route_normalisee,
        match_status = "no_numbered_road", match_method = NA_character_,
        road_number = NA_character_, road_id = NA_character_, distance_to_road_m = NA_real_,
        road_importance = NA_character_, road_urban = NA, road_speed_mean = NA_real_,
        error_message = NA_character_
      )
    } else {
      best <- pool[which.min(distance_to_road_m)]
      status <- if (best$distance_to_road_m <= maximum_match_distance_m) "matched" else "too_far"
      new_results[[i]] <- data.table(
        cle_radar = radar$cle_radar, installation_year = radar$installation_year,
        latitude = radar$latitude, longitude = radar$longitude,
        route_2018 = radar$route_2018, route_normalisee = radar$route_normalisee,
        match_status = status, match_method = method,
        road_number = best$road_number, road_id = best$road_id,
        distance_to_road_m = best$distance_to_road_m,
        road_importance = best$road_importance, road_urban = best$road_urban,
        road_speed_mean = best$road_speed_mean, error_message = NA_character_
      )
    }
  }

  if (i %% 25L == 0L || i == nrow(todo)) {
    partial <- rbindlist(new_results[seq_len(i)], fill = TRUE)
    fwrite(rbindlist(list(existing, partial), fill = TRUE), output_file)
    message("Appariements traités : ", i, "/", nrow(todo))
  }
}

matches <- fread(output_file)
diagnostics <- matches[, .(
  radars = .N,
  matched = sum(match_status == "matched"),
  exact_route_number = sum(match_status == "matched" & match_method == "exact_route_number"),
  nearest_numbered_road = sum(match_status == "matched" & match_method == "nearest_numbered_road"),
  median_distance_m = median(distance_to_road_m[match_status == "matched"], na.rm = TRUE),
  p90_distance_m = quantile(distance_to_road_m[match_status == "matched"], 0.9, na.rm = TRUE),
  within_30m = sum(match_status == "matched" & distance_to_road_m <= 30),
  within_50m = sum(match_status == "matched" & distance_to_road_m <= 50),
  wfs_errors = sum(match_status == "wfs_error")
)]
fwrite(diagnostics, diagnostic_file)
print(diagnostics)
