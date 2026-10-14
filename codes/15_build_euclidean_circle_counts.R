#!/usr/bin/env Rscript

# Monthly crash counts in conventional Euclidean circles around the same
# camera sites used by the road-network analysis.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
processed <- file.path(root, "data", "processed")
out_dir <- file.path(root, "results", "current")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

radii_km <- c(1, 2, 3)
maximum_radius_km <- max(radii_km)
output_file <- file.path(out_dir, "circle_monthly_counts.csv.gz")
diagnostic_file <- file.path(out_dir, "circle_monthly_count_diagnostics.csv")

haversine_km <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 +
    cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  6371.0088 * 2 * atan2(sqrt(a), sqrt(pmax(0, 1 - a)))
}

message("Reading the camera sites used by the network design...")
network_diagnostics <- fread(file.path(
  out_dir, "road_corridor_geometry_diagnostics_network_100m.csv"
))[status == "ok"]
site_ids <- network_diagnostics$cle_radar

radars <- fread(file.path(processed, "radars.csv.gz"))[
  cle_radar %chin% site_ids & !is.na(latitude) & !is.na(longitude),
  .(cle_radar, latitude, longitude)
]
setorder(radars, cle_radar)
stopifnot(uniqueN(radars$cle_radar) == length(site_ids))

message("Reading geocoded crashes and fatalities...")
accidents <- fread(
  file.path(processed, "accidents.csv.gz"),
  select = c("id_accident", "annee", "mois", "latitude", "longitude",
             "geolocalisation_valide")
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

results <- vector("list", nrow(radars))
diagnostics <- vector("list", nrow(radars))
for (i in seq_len(nrow(radars))) {
  radar <- radars[i]
  lat_delta <- (maximum_radius_km + 0.05) / 110.574
  lon_delta <- (maximum_radius_km + 0.05) /
    (111.320 * cos(radar$latitude * pi / 180))
  nearby <- accidents[
    abs(latitude - radar$latitude) <= lat_delta &
      abs(longitude - radar$longitude) <= lon_delta
  ]
  if (nrow(nearby)) {
    nearby[, distance_km := haversine_km(
      radar$latitude, radar$longitude, latitude, longitude
    )]
  } else {
    nearby[, distance_km := numeric()]
  }
  grid <- CJ(radius_km = radii_km, month_index = all_months)
  observed <- rbindlist(lapply(radii_km, function(radius) {
    nearby[distance_km <= radius,
           .(accidents = .N, killed = sum(killed)), by = month_index][
             , radius_km := radius]
  }), use.names = TRUE, fill = TRUE)
  if (nrow(observed)) grid <- observed[grid, on = .(radius_km, month_index)]
  if (!"accidents" %in% names(grid)) grid[, accidents := NA_integer_]
  if (!"killed" %in% names(grid)) grid[, killed := NA_integer_]
  grid[is.na(accidents), accidents := 0L]
  grid[is.na(killed), killed := 0L]
  grid[, cle_radar := radar$cle_radar]
  results[[i]] <- grid
  diagnostics[[i]] <- data.table(
    cle_radar = radar$cle_radar,
    nearby_accidents_3km = sum(nearby$distance_km <= maximum_radius_km)
  )
  if (i %% 100L == 0L || i == nrow(radars)) {
    message("Circle counts: ", i, "/", nrow(radars))
  }
}

counts <- rbindlist(results, use.names = TRUE, fill = TRUE)
setorder(counts, cle_radar, radius_km, month_index)
fwrite(counts, output_file)
fwrite(rbindlist(diagnostics), diagnostic_file)

message("Euclidean-circle totals:")
print(counts[, .(
  sites = uniqueN(cle_radar), accidents = sum(accidents), killed = sum(killed)
), by = radius_km])
