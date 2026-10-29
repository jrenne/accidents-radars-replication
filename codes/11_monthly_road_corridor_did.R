#!/usr/bin/env Rscript

# Construct monthly crash counts. Estimation is handled by script 13.

suppressPackageStartupMessages({
  library(curl)
  library(data.table)
  library(igraph)
  library(jsonlite)
})

root <- normalizePath(".")
processed <- file.path(root, "data", "processed")
out_dir <- file.path(root, "results", "current")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

radii_km <- c(1, 2, 3)
corridor_half_width_m <- as.numeric(Sys.getenv("CORRIDOR_HALF_WIDTH_M", "50"))
spatial_method <- match.arg(
  Sys.getenv("CORRIDOR_METHOD", "network"),
  c("network", "euclidean")
)
algorithm_version <- if (spatial_method == "network") "network-v2" else "euclidean-v2"
exclude_large_cities <- identical(Sys.getenv("EXCLUDE_LARGE_CITIES", "1"), "1")
width_suffix <- if (corridor_half_width_m == 50) "" else
  paste0("_", as.integer(corridor_half_width_m), "m")
method_suffix <- if (spatial_method == "network") "_network" else ""
city_suffix <- if (exclude_large_cities) "" else "_with_large_cities"
analysis_suffix <- paste0(method_suffix, width_suffix, city_suffix)
output_path <- function(stem, extension = ".csv")
  file.path(out_dir, paste0(stem, analysis_suffix, extension))
maximum_radius_km <- max(radii_km)
large_city_population_thousands <- 100
large_city_buffer_km <- 10
counts_file <- output_path("road_corridor_monthly_counts", ".csv.gz")
geometry_diagnostic_file <- output_path("road_corridor_geometry_diagnostics")
max_corridors <- as.integer(Sys.getenv("MAX_CORRIDORS", "999999"))
geometry_cache_dir <- file.path(root, "data", "roads", "ign_road_geometry_cache")
dir.create(geometry_cache_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(root, "R", "network_corridor_helpers.R"))

haversine_km <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 +
    cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  6371.0088 * 2 * atan2(sqrt(a), sqrt(pmax(0, 1 - a)))
}

fetch_road_geometry <- function(latitude, longitude, road_number) {
  radius <- maximum_radius_km + 0.5
  lat_delta <- radius / 110.574
  lon_delta <- radius / (111.320 * cos(latitude * pi / 180))
  cql <- sprintf(
    "cpx_numero='%s' AND BBOX(geometrie,%.7f,%.7f,%.7f,%.7f,'CRS:84')",
    gsub("'", "''", road_number),
    longitude - lon_delta, latitude - lat_delta,
    longitude + lon_delta, latitude + lat_delta
  )
  url <- paste0(
    "https://data.geopf.fr/wfs/ows?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature",
    "&TYPENAMES=BDTOPO_V3%3Atroncon_de_route",
    "&OUTPUTFORMAT=application%2Fjson&SRSNAME=CRS%3A84&COUNT=2000&CQL_FILTER=",
    URLencode(cql, reserved = TRUE)
  )
  last_error <- NULL
  for (attempt in 1:3) {
    response <- tryCatch(curl_fetch_memory(url), error = function(e) e)
    if (!inherits(response, "error") && response$status_code == 200L) {
      parsed <- tryCatch(
        fromJSON(rawToChar(response$content), simplifyVector = FALSE),
        error = function(e) e
      )
      if (!inherits(parsed, "error")) return(parsed$features)
      last_error <- conditionMessage(parsed)
    } else {
      last_error <- if (inherits(response, "error")) conditionMessage(response) else
        paste("HTTP", response$status_code)
    }
    Sys.sleep(attempt)
  }
  stop(last_error)
}

features_to_segments <- function(features, latitude, longitude) {
  segments <- lapply(features, function(feature) {
    if (is.null(feature$geometry) || feature$geometry$type != "LineString") return(NULL)
    xy <- t(vapply(
      feature$geometry$coordinates,
      function(point) as.numeric(unlist(point))[1:2],
      numeric(2)
    ))
    if (nrow(xy) < 2) return(NULL)
    x <- (xy[, 1] - longitude) * 111320 * cos(latitude * pi / 180)
    y <- (xy[, 2] - latitude) * 110574
    data.table(x1 = x[-length(x)], y1 = y[-length(y)], x2 = x[-1], y2 = y[-1])
  })
  rbindlist(segments)
}

minimum_segment_distance_m <- function(x, y, segments) {
  minimum <- rep(Inf, length(x))
  for (j in seq_len(nrow(segments))) {
    dx <- segments$x2[j] - segments$x1[j]
    dy <- segments$y2[j] - segments$y1[j]
    denominator <- dx^2 + dy^2
    t <- if (denominator > 0) {
      pmax(0, pmin(1, ((x - segments$x1[j]) * dx + (y - segments$y1[j]) * dy) / denominator))
    } else rep(0, length(x))
    distance <- sqrt(
      (x - (segments$x1[j] + t * dx))^2 +
        (y - (segments$y1[j] + t * dy))^2
    )
    minimum <- pmin(minimum, distance)
  }
  minimum
}

message("Reconstruction de l'échantillon de radars...")
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
radars[, `:=`(
  installation_year = as.integer(format(installation_date, "%Y")),
  installation_month = as.integer(format(installation_date, "%Y")) * 12L +
    as.integer(format(installation_date, "%m")) - 1L
)]

matches <- fread(file.path(out_dir, "radar_ign_road_matches.csv"))[match_status == "matched"]
radars <- merge(
  radars, matches[, .(cle_radar, road_number, distance_to_road_m, match_method)],
  by = "cle_radar", all.x = TRUE
)
radars[, near_large_city := FALSE]
if (exclude_large_cities) {
  city_environment <- new.env(parent = emptyenv())
  load(file.path(root, "data", "auxiliary", "result.radar.commune.rda"), envir = city_environment)
  city_centres <- unique(as.data.table(city_environment$radars)[
    Population >= large_city_population_thousands & !is.na(lat.commune) & !is.na(long.commune),
    .(INSEE_COM, latitude = lat.commune, longitude = long.commune)
  ], by = "INSEE_COM")
  for (i in seq_len(nrow(radars))) {
    radars$near_large_city[i] <- any(haversine_km(
      radars$latitude[i], radars$longitude[i], city_centres$latitude, city_centres$longitude
    ) <= large_city_buffer_km)
  }
}

base_candidates <- radars[
  installation_year %between% c(2012L, 2024L) &
    !is.na(road_number) &
    (!exclude_large_cities | near_large_city == FALSE)
]
message("Lecture des accidents et des décès...")
accidents <- fread(
  file.path(processed, "accidents.csv.gz"),
  select = c("id_accident", "annee", "mois", "latitude", "longitude", "geolocalisation_valide")
)[
  annee %between% c(2010L, 2024L) & geolocalisation_valide == 1L &
    !is.na(latitude) & !is.na(longitude) & !is.na(mois)
]
deaths <- fread(
  file.path(processed, "usagers.csv.gz"),
  select = c("id_accident", "gravite", "source_annee")
)[source_annee %between% c(2010L, 2024L),
  .(killed = sum(gravite == 2L, na.rm = TRUE)), by = id_accident]
accidents <- deaths[accidents, on = "id_accident"]
accidents[is.na(killed), killed := 0L]
accidents[, month_index := annee * 12L + mois - 1L]
all_months <- seq(2010L * 12L, 2024L * 12L + 11L)

existing_counts <- if (file.exists(counts_file)) fread(counts_file) else data.table()
existing_diagnostics <- if (file.exists(geometry_diagnostic_file))
  fread(geometry_diagnostic_file) else data.table()
done <- if (nrow(existing_diagnostics)) {
  valid_algorithm <- existing_diagnostics$algorithm_version == algorithm_version
  existing_diagnostics$status[is.na(existing_diagnostics$status)] <- ""
  existing_diagnostics$cle_radar[
    existing_diagnostics$status != "wfs_error" & valid_algorithm
  ]
} else character()
todo <- base_candidates[!cle_radar %chin% done]
if (nrow(todo) > max_corridors) todo <- todo[seq_len(max_corridors)]
message("Corridors à construire : ", nrow(todo), " (déjà en cache : ", length(done), ")")

new_counts <- vector("list", nrow(todo))
new_diagnostics <- vector("list", nrow(todo))
for (i in seq_len(nrow(todo))) {
  radar <- todo[i]
  cache_token <- paste(radar$cle_radar, radar$road_number, sep = "__")
  geometry_cache_file <- file.path(
    geometry_cache_dir,
    paste0(gsub("[^A-Za-z0-9_-]", "_", cache_token), ".rds")
  )
  legacy_cache_file <- file.path(
    geometry_cache_dir,
    paste0(gsub("[^A-Za-z0-9_-]", "_", radar$cle_radar), ".rds")
  )
  features <- if (file.exists(geometry_cache_file)) {
    tryCatch(readRDS(geometry_cache_file), error = function(e) e)
  } else if (file.exists(legacy_cache_file)) {
    cached <- tryCatch(readRDS(legacy_cache_file), error = function(e) e)
    if (!inherits(cached, "error")) saveRDS(cached, geometry_cache_file)
    cached
  } else {
    fetched <- tryCatch(
      fetch_road_geometry(radar$latitude, radar$longitude, radar$road_number),
      error = function(e) e
    )
    if (!inherits(fetched, "error")) saveRDS(fetched, geometry_cache_file)
    fetched
  }
  if (inherits(features, "error")) {
    new_diagnostics[[i]] <- data.table(
      cle_radar = radar$cle_radar, status = "wfs_error", road_number = radar$road_number,
      road_features = NA_integer_, line_segments = NA_integer_, nearby_accidents = NA_integer_,
      corridor_accidents = NA_integer_, spatial_method = spatial_method,
      algorithm_version = algorithm_version, error_message = conditionMessage(features)
    )
  } else {
    segments <- if (spatial_method == "network") {
      features_to_network_segments(features, radar$latitude, radar$longitude)
    } else {
      features_to_segments(features, radar$latitude, radar$longitude)
    }
    if (!nrow(segments)) {
      new_diagnostics[[i]] <- data.table(
        cle_radar = radar$cle_radar, status = "no_geometry", road_number = radar$road_number,
        road_features = length(features), line_segments = 0L, nearby_accidents = 0L,
        corridor_accidents = 0L, spatial_method = spatial_method,
        algorithm_version = algorithm_version, error_message = NA_character_
      )
    } else {
      lat_delta <- (maximum_radius_km + 0.1) / 110.574
      lon_delta <- (maximum_radius_km + 0.1) /
        (111.320 * cos(radar$latitude * pi / 180))
      nearby <- accidents[
        abs(latitude - radar$latitude) <= lat_delta &
          abs(longitude - radar$longitude) <= lon_delta
      ]
      if (nrow(nearby)) {
        nearby[, `:=`(
          x_m = (longitude - radar$longitude) * 111320 * cos(radar$latitude * pi / 180),
          y_m = (latitude - radar$latitude) * 110574
        )]
      }

      grid <- CJ(radius_km = radii_km, month_index = all_months)
      snap_to_network_m <- NA_real_
      network_nodes <- NA_integer_
      reachable_length_m <- NA_real_
      reachable_length_1km_m <- NA_real_
      reachable_length_2km_m <- NA_real_
      reachable_length_3km_m <- NA_real_
      corridor_accidents <- 0L
      if (spatial_method == "network") {
        snapped <- project_origin_to_segments(segments)
        node_distances <- network_node_distances(segments, snapped)
        reachable <- stats::setNames(lapply(radii_km, function(radius) {
          clip_network_segments(segments, node_distances, radius * 1000)
        }), radii_km)
        observed <- rbindlist(lapply(radii_km, function(radius) {
          if (!nrow(nearby)) return(data.table())
          road_reach <- reachable[[as.character(radius)]]
          lateral_m <- minimum_segment_distance_m(nearby$x_m, nearby$y_m, road_reach)
          selected <- nearby[lateral_m <= corridor_half_width_m]
          if (radius == maximum_radius_km) corridor_accidents <<- nrow(selected)
          selected[, .(accidents = .N, killed = sum(killed)), by = month_index][
            , radius_km := radius]
        }), fill = TRUE)
        snap_to_network_m <- snapped$distance_m
        network_nodes <- length(node_distances)
        reachable_length_m <- sum(
          reachable[[as.character(maximum_radius_km)]]$length_m
        )
        reachable_length_1km_m <- sum(reachable[["1"]]$length_m)
        reachable_length_2km_m <- sum(reachable[["2"]]$length_m)
        reachable_length_3km_m <- sum(reachable[["3"]]$length_m)
      } else {
        if (nrow(nearby)) {
          nearby[, radial_km := haversine_km(
            radar$latitude, radar$longitude, latitude, longitude
          )]
          nearby[, lateral_m := minimum_segment_distance_m(x_m, y_m, segments)]
          corridor <- nearby[
            lateral_m <= corridor_half_width_m & radial_km <= maximum_radius_km
          ]
        } else {
          corridor <- copy(nearby)
          corridor[, `:=`(radial_km = numeric(), lateral_m = numeric())]
        }
        corridor_accidents <- nrow(corridor)
        observed <- rbindlist(lapply(radii_km, function(radius) {
          corridor[radial_km <= radius,
                   .(accidents = .N, killed = sum(killed)), by = month_index][
                     , radius_km := radius]
        }), fill = TRUE)
      }
      if (nrow(observed)) {
        grid <- observed[grid, on = .(radius_km, month_index)]
      }
      if (!"accidents" %in% names(grid)) grid[, accidents := NA_integer_]
      if (!"killed" %in% names(grid)) grid[, killed := NA_integer_]
      grid[is.na(accidents), accidents := 0L]
      grid[is.na(killed), killed := 0L]
      grid[, cle_radar := radar$cle_radar]
      new_counts[[i]] <- grid
      new_diagnostics[[i]] <- data.table(
        cle_radar = radar$cle_radar, status = "ok", road_number = radar$road_number,
        road_features = length(features), line_segments = nrow(segments),
        nearby_accidents = nrow(nearby), corridor_accidents = corridor_accidents,
        snap_to_network_m = snap_to_network_m, network_nodes = network_nodes,
        reachable_length_m = reachable_length_m, spatial_method = spatial_method,
        reachable_length_1km_m = reachable_length_1km_m,
        reachable_length_2km_m = reachable_length_2km_m,
        reachable_length_3km_m = reachable_length_3km_m,
        algorithm_version = algorithm_version,
        error_message = NA_character_
      )
    }
  }

  if (i %% 25L == 0L || i == nrow(todo)) {
    partial_counts <- rbindlist(new_counts[seq_len(i)], fill = TRUE)
    partial_diagnostics <- rbindlist(new_diagnostics[seq_len(i)], fill = TRUE)
    combined_counts <- rbindlist(list(existing_counts, partial_counts), fill = TRUE)
    if (nrow(combined_counts)) {
      combined_counts <- unique(combined_counts, by = c("cle_radar", "radius_km", "month_index"),
                                fromLast = TRUE)
      fwrite(combined_counts, counts_file)
    }
    combined_diagnostics <- rbindlist(
      list(existing_diagnostics, partial_diagnostics), fill = TRUE
    )
    combined_diagnostics <- unique(combined_diagnostics, by = "cle_radar", fromLast = TRUE)
    fwrite(combined_diagnostics, geometry_diagnostic_file)
    message("Corridors traités : ", i, "/", nrow(todo))
  }
}

message("Monthly corridor counts saved in results/current.")
