#!/usr/bin/env Rscript

# Match baseline camera locations to official annual traffic sections on the
# French national road network. Matches use the same normalized road number
# and the distance from the camera to the chord joining section endpoints.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")
traffic_dir <- file.path(root, "data", "traffic")

wgs84_to_lambert93 <- function(latitude, longitude) {
  eccentricity <- 0.081819191042816
  exponent <- 0.725607765053267
  constant <- 11754255.426096
  p <- pi / 180
  phi <- latitude * p
  radius <- constant * exp(
    -exponent * (atanh(sin(phi)) - eccentricity * atanh(eccentricity * sin(phi)))
  )
  angle <- exponent * (longitude * p - 3 * p)
  list(
    x = 700000 + radius * sin(angle),
    y = 12655612.049876 - radius * cos(angle)
  )
}

point_segment_distance <- function(px, py, x1, y1, x2, y2) {
  dx <- x2 - x1
  dy <- y2 - y1
  denominator <- dx^2 + dy^2
  fraction <- fifelse(
    denominator > 0,
    ((px - x1) * dx + (py - y1) * dy) / denominator,
    0
  )
  fraction <- pmax(0, pmin(1, fraction))
  sqrt((px - x1 - fraction * dx)^2 + (py - y1 - fraction * dy)^2)
}

normalize_national_route <- function(value) {
  value <- toupper(gsub("[^A-Z0-9]", "", value))
  matched <- regexec("^(?:RN|N|A)0*([0-9]{1,4})", value)
  parts <- regmatches(value, matched)
  vapply(parts, function(x) {
    if (length(x) < 2L) return(NA_character_)
    prefix <- if (grepl("^A", x[1])) "A" else "N"
    paste0(prefix, sprintf("%04d", as.integer(x[2])))
  }, character(1))
}

read_traffic <- function(year) {
  path <- file.path(traffic_dir, sprintf("tmja_%d.csv", year))
  if (!file.exists(path)) stop("Missing official TMJA file: ", path)
  x <- fread(path, dec = ",", na.strings = c("", "NA"))
  required <- c("route", "longueur", "xD", "yD", "xF", "yF", "TMJA")
  if (!all(required %chin% names(x))) {
    stop("Unexpected TMJA schema in ", path)
  }
  x[, `:=`(
    normalized_route = normalize_national_route(route),
    traffic_year = year,
    traffic_section_row = .I
  )]
  x <- x[
    !is.na(normalized_route) & is.finite(TMJA) & TMJA > 0 &
      is.finite(xD) & is.finite(yD) & is.finite(xF) & is.finite(yF)
  ]
  x[, chord_m := sqrt((xF - xD)^2 + (yF - yD)^2)]
  x[, chord_quality :=
      is.finite(longueur) & longueur > 0 & longueur <= 5000 &
      chord_m / longueur >= 0.5]
  x
}

membership <- fread(file.path(out_dir, "baseline_radar_sample_membership.csv"))
roads <- fread(file.path(out_dir, "radar_ign_road_matches.csv"))[
  match_status == "matched",
  .(cle_radar, road_number, road_match_method = match_method)
]
sites <- merge(membership, roads, by = "cle_radar", all.x = TRUE)
sites[, normalized_route := normalize_national_route(road_number)]
coordinates <- wgs84_to_lambert93(sites$latitude, sites$longitude)
sites[, `:=`(x = coordinates$x, y = coordinates$y)]
sites <- sites[
  (baseline_treated == TRUE | baseline_control == TRUE) &
    !is.na(normalized_route)
]

traffic <- setNames(lapply(2011:2019, read_traffic), as.character(2011:2019))
matches <- vector("list", length = nrow(sites) * length(traffic))
cursor <- 1L
for (year in 2011:2019) {
  sections <- traffic[[as.character(year)]]
  for (i in seq_len(nrow(sites))) {
    candidate_sections <- sections[
      normalized_route == sites$normalized_route[i]
    ]
    if (!nrow(candidate_sections)) next
    distance <- point_segment_distance(
      sites$x[i], sites$y[i],
      candidate_sections$xD, candidate_sections$yD,
      candidate_sections$xF, candidate_sections$yF
    )
    nearest <- which.min(distance)
    row <- candidate_sections[nearest]
    matches[[cursor]] <- data.table(
      cle_radar = sites$cle_radar[i],
      traffic_year = year,
      normalized_route = sites$normalized_route[i],
      tmja = row$TMJA,
      tmja_distance_m = distance[nearest],
      chord_quality = row$chord_quality,
      traffic_section_row = row$traffic_section_row,
      traffic_route_raw = row$route
    )
    cursor <- cursor + 1L
  }
}
matches <- rbindlist(matches[seq_len(cursor - 1L)], fill = TRUE)
matches <- merge(
  matches,
  sites[, .(
    cle_radar, installation_year, baseline_treated, baseline_control,
    road_number, road_match_method
  )],
  by = "cle_radar", all.x = TRUE
)
matches[, pre_installation_year := traffic_year == installation_year - 1L]
matches[, within_100m := tmja_distance_m <= 100]
matches[, within_300m := tmja_distance_m <= 300]
matches[, within_500m := tmja_distance_m <= 500]

fwrite(matches, file.path(out_dir, "baseline_tmja_site_year_matches.csv"))

national_sites <- unique(sites[, .(
  cle_radar, installation_year, baseline_treated, baseline_control
)])
summary <- rbindlist(lapply(c(100, 300, 500, 1000, 2000), function(threshold) {
  pre <- matches[
    pre_installation_year == TRUE & tmja_distance_m <= threshold
  ]
  data.table(
    maximum_distance_m = threshold,
    treated_national_sites = national_sites[baseline_treated == TRUE, uniqueN(cle_radar)],
    treated_with_pre_tmja = pre[baseline_treated == TRUE, uniqueN(cle_radar)],
    control_national_sites = national_sites[baseline_control == TRUE, uniqueN(cle_radar)],
    controls_with_own_pre_tmja = pre[baseline_control == TRUE, uniqueN(cle_radar)],
    treated_high_quality_chord = pre[
      baseline_treated == TRUE & chord_quality == TRUE, uniqueN(cle_radar)
    ],
    site_year_matches = matches[tmja_distance_m <= threshold, .N]
  )
}))
fwrite(summary, file.path(out_dir, "baseline_tmja_coverage_summary.csv"))

print(summary)
