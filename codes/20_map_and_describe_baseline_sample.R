#!/usr/bin/env Rscript

# Reconstruct the preferred two-kilometre estimation sample without fitting the
# nuisance models. The script documents the number of crash assignments used by
# the estimator and produces the deployment/sample map used in the paper.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
})

root <- normalizePath(".")
processed <- file.path(root, "data", "processed")
out_dir <- file.path(root, "results", "current")

haversine_km <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 +
    cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  6371.0088 * 2 * atan2(sqrt(a), sqrt(pmax(0, 1 - a)))
}

road_class <- function(route) {
  route <- toupper(gsub("[^A-Z0-9]", "", route))
  fifelse(
    grepl("^A[0-9]", route), "autoroute",
    fifelse(
      grepl("^N[0-9]", route), "nationale",
      fifelse(grepl("^D[0-9]", route), "departementale", "other")
    )
  )
}

# Direct reader for polygon shapefiles. This keeps the figure reproducible with
# base R plus the packages already used by the project; no GIS package is
# required merely to draw the departmental outline.
read_polygon_shapefile <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con))
  seek(con, where = 100, origin = "start")
  pieces <- list()
  group <- 0L
  repeat {
    record_number <- readBin(con, integer(), n = 1L, size = 4L, endian = "big")
    if (!length(record_number)) break
    content_words <- readBin(con, integer(), n = 1L, size = 4L, endian = "big")
    payload <- readBin(con, raw(), n = 2L * content_words)
    rc <- rawConnection(payload, open = "rb")
    shape_type <- readBin(rc, integer(), n = 1L, size = 4L, endian = "little")
    if (shape_type %in% c(5L, 15L, 25L)) {
      readBin(rc, numeric(), n = 4L, size = 8L, endian = "little")
      n_parts <- readBin(rc, integer(), n = 1L, size = 4L, endian = "little")
      n_points <- readBin(rc, integer(), n = 1L, size = 4L, endian = "little")
      starts <- readBin(rc, integer(), n = n_parts, size = 4L, endian = "little")
      coordinates <- matrix(
        readBin(rc, numeric(), n = 2L * n_points, size = 8L, endian = "little"),
        ncol = 2L, byrow = TRUE
      )
      ends <- c(starts[-1L], n_points) - 1L
      for (part in seq_len(n_parts)) {
        group <- group + 1L
        rows <- (starts[part] + 1L):(ends[part] + 1L)
        pieces[[group]] <- data.table(
          x = coordinates[rows, 1L], y = coordinates[rows, 2L], group = group
        )
      }
    }
    close(rc)
  }
  rbindlist(pieces)
}

# Longitude/latitude (RGF93/WGS84) to Lambert-93 (EPSG:2154).
to_lambert93 <- function(longitude, latitude) {
  rad <- pi / 180
  a <- 6378137
  e <- 0.0818191910428158
  lon0 <- 3 * rad
  lat0 <- 46.5 * rad
  lat1 <- 44 * rad
  lat2 <- 49 * rad
  x0 <- 700000
  y0 <- 6600000
  iso_lat <- function(phi) {
    log(tan(pi / 4 + phi / 2) *
          ((1 - e * sin(phi)) / (1 + e * sin(phi)))^(e / 2))
  }
  m <- function(phi) cos(phi) / sqrt(1 - e^2 * sin(phi)^2)
  n <- (log(m(lat1)) - log(m(lat2))) / (iso_lat(lat2) - iso_lat(lat1))
  cst <- m(lat1) * exp(n * iso_lat(lat1)) / n
  radius <- a * cst * exp(-n * iso_lat(latitude * rad))
  radius0 <- a * cst * exp(-n * iso_lat(lat0))
  angle <- n * (longitude * rad - lon0)
  data.table(
    x = x0 + radius * sin(angle),
    y = y0 + radius0 - radius * cos(angle)
  )
}

message("Reconstructing dated point-camera sites...")
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
       installation_date := fifelse(
         is.na(date_2025), date_2018, pmin(date_2025, date_2018)
       )]
radars[actif_liste_2025 == 0L & old_point == TRUE,
       installation_date := date_2018]
radars <- radars[!is.na(installation_date)]
radars[, `:=`(
  installation_year = as.integer(format(installation_date, "%Y")),
  installation_month = as.integer(format(installation_date, "%Y")) * 12L +
    as.integer(format(installation_date, "%m")) - 1L
)]
exposure_radars <- copy(radars)

audit <- fread(file.path(out_dir, "radar_treatment_date_audit.csv"))[, .(
  cle_radar, date_confidence
)]
radars <- merge(radars, audit, by = "cle_radar", all.x = TRUE)

matches <- fread(file.path(out_dir, "radar_ign_road_matches.csv"))[
  match_status == "matched",
  .(cle_radar, road_number, road_importance, road_urban, road_speed_mean)
]
radars <- merge(radars, matches, by = "cle_radar", all.x = TRUE)

radars[, near_large_city := FALSE]
city_environment <- new.env(parent = emptyenv())
load(file.path(root, "data", "auxiliary", "result.radar.commune.rda"), envir = city_environment)
city_centres <- unique(as.data.table(city_environment$radars)[
  Population >= 100 & !is.na(lat.commune) & !is.na(long.commune),
  .(INSEE_COM, latitude = lat.commune, longitude = long.commune)
], by = "INSEE_COM")
for (i in seq_len(nrow(radars))) {
  radars$near_large_city[i] <- any(haversine_km(
    radars$latitude[i], radars$longitude[i],
    city_centres$latitude, city_centres$longitude
  ) <= 10)
}

counts <- fread(file.path(out_dir, "road_corridor_monthly_counts_network_100m.csv.gz"))
usable_ids <- unique(counts$cle_radar)
geometry <- fread(file.path(
  out_dir, "road_corridor_geometry_diagnostics_network_100m.csv"
))
reach_columns <- c(
  "cle_radar", "reachable_length_1km_m", "reachable_length_2km_m",
  "reachable_length_3km_m"
)
radars <- merge(radars, geometry[, ..reach_columns], by = "cle_radar", all.x = TRUE)

base_candidates <- radars[
  installation_year %between% c(2012L, 2024L) &
    !is.na(road_number) & near_large_city == FALSE & cle_radar %chin% usable_ids
]
base_candidates[, matching_stratum := paste(
  road_class(road_number),
  fifelse(is.na(vma_kmh), "vma_missing", paste0("vma_", vma_kmh)),
  sep = "__"
)]

all_months <- seq(2010L * 12L, 2024L * 12L + 11L)

build_first_year_sample <- function(radius, design_radius = radius) {
  candidates <- copy(base_candidates)
  reach_column <- paste0("reachable_length_", as.integer(design_radius), "km_m")
  candidates <- candidates[
    is.finite(get(reach_column)) &
      get(reach_column) >= 0.75 * 2 * design_radius * 1000
  ]

  keep <- logical(nrow(candidates))
  next_nearby <- rep(Inf, nrow(candidates))
  for (i in seq_len(nrow(candidates))) {
    distance <- haversine_km(
      candidates$latitude[i], candidates$longitude[i],
      exposure_radars$latitude, exposure_radars$longitude
    )
    other <- exposure_radars$cle_radar != candidates$cle_radar[i]
    keep[i] <- !any(
      distance < design_radius & other &
        exposure_radars$installation_month <= candidates$installation_month[i],
      na.rm = TRUE
    )
    later <- exposure_radars$installation_month[
      distance < design_radius & other &
        exposure_radars$installation_month > candidates$installation_month[i]
    ]
    if (length(later)) next_nearby[i] <- min(later)
  }
  candidates[, next_nearby_installation_month := next_nearby]
  candidates <- candidates[keep]
  setorder(candidates, cle_radar)

  available <- unique(counts[radius_km == radius, cle_radar])
  candidates <- candidates[cle_radar %chin% available]
  candidates[, site_index := .I]
  assignments <- list()
  cursor <- 1L
  for (treated_index in candidates[installation_year <= 2022L, site_index]) {
    anchor <- candidates$installation_month[treated_index]
    needed <- c(anchor + (-24L:-1L), anchor + (1L:12L))
    if (min(needed) < min(all_months) || max(needed) > max(all_months)) next
    if (candidates$next_nearby_installation_month[treated_index] <= anchor + 12L) next
    controls <- candidates[
      installation_month > anchor + 12L &
        matching_stratum == candidates$matching_stratum[treated_index],
      site_index
    ]
    if (length(controls)) {
      distance <- haversine_km(
        candidates$latitude[treated_index], candidates$longitude[treated_index],
        candidates$latitude[controls], candidates$longitude[controls]
      )
      controls <- controls[distance > 2 * design_radius]
    }
    if (length(controls) < 10L) next
    assignments[[cursor]] <- data.table(
      stack_id = cursor,
      anchor_month = anchor,
      cle_radar = candidates$cle_radar[c(treated_index, controls)],
      role = c("treated", rep("control", length(controls)))
    )
    cursor <- cursor + 1L
  }
  rbindlist(assignments)
}

message("Reconstructing the baseline and common-radius risk sets...")
baseline <- build_first_year_sample(radius = 2, design_radius = 2)
common <- build_first_year_sample(radius = 2, design_radius = 3)
stopifnot(
  baseline[role == "treated", uniqueN(cle_radar)] == 406L,
  baseline[role == "control", uniqueN(cle_radar)] == 477L,
  common[role == "treated", uniqueN(cle_radar)] == 327L,
  all(common[role == "treated", unique(cle_radar)] %chin%
        baseline[role == "treated", unique(cle_radar)])
)

baseline_treated <- unique(baseline[role == "treated", cle_radar])
baseline_controls <- unique(baseline[role == "control", cle_radar])
common_treated <- unique(common[role == "treated", cle_radar])

membership <- radars[, .(
  cle_radar, latitude, longitude, installation_date, installation_year,
  date_confidence
)]
membership[, `:=`(
  baseline_treated = cle_radar %chin% baseline_treated,
  baseline_control = cle_radar %chin% baseline_controls,
  common_radius_treated = cle_radar %chin% common_treated
)]
membership[, estimation_role := fcase(
  common_radius_treated, "Treated: all three radii",
  baseline_treated, "Treated: 2 km sample only",
  baseline_control, "Control only",
  default = "Not used in baseline"
)]
membership[, deployment_cohort := cut(
  installation_year,
  breaks = c(-Inf, 2007, 2011, 2015, 2019, Inf),
  labels = c("Before 2008", "2008--2011", "2012--2015", "2016--2019", "2020--2024")
)]
membership[, metropolitan_france :=
  longitude >= -6 & longitude <= 11 & latitude >= 41 & latitude <= 52]
fwrite(membership, file.path(out_dir, "baseline_radar_sample_membership.csv"))

message("Counting crash assignments in the observations used by the estimator...")
count_matrix <- dcast(
  counts[radius_km == 2], cle_radar ~ month_index,
  value.var = "accidents"
)
month_columns <- as.integer(names(count_matrix)[-1L])
row_lookup <- setNames(seq_len(nrow(count_matrix)), count_matrix$cle_radar)
count_values <- as.matrix(count_matrix[, -"cle_radar"])

used_cells <- baseline[, {
  months <- anchor_month + c(-24L:-1L, 1L:12L)
  .(month_index = months)
}, by = .(cle_radar, stack_id, role)]
used_cells <- unique(used_cells, by = c("cle_radar", "month_index"))
used_cells[, accidents := {
  row <- unname(row_lookup[cle_radar])
  column <- match(month_index, month_columns)
  as.numeric(count_values[cbind(row, column)])
}]

treated_anchors <- unique(baseline[role == "treated", .(cle_radar, anchor_month)])
treated_cells <- treated_anchors[, {
  months <- anchor_month + c(-24L:-1L, 1L:12L)
  .(
    period = rep(c("Months -24 to -1", "Months +1 to +12"), c(24L, 12L)),
    month_index = months
  )
}, by = cle_radar]
treated_cells[, accidents := {
  row <- unname(row_lookup[cle_radar])
  column <- match(month_index, month_columns)
  as.numeric(count_values[cbind(row, column)])
}]
reference_cells <- treated_anchors[, {
  months <- anchor_month + (-12L:-1L)
  .(month_index = months)
}, by = cle_radar]
reference_cells[, accidents := {
  row <- unname(row_lookup[cle_radar])
  column <- match(month_index, month_columns)
  as.numeric(count_values[cbind(row, column)])
}]

summarise_cells <- function(x, label) {
  data.table(
    sample = label,
    unique_sites = uniqueN(x$cle_radar),
    site_months = nrow(x),
    accident_assignments = sum(x$accidents),
    nonzero_site_months = sum(x$accidents > 0),
    sites_with_any_accident = uniqueN(x[accidents > 0, cle_radar]),
    zero_site_month_share = mean(x$accidents == 0)
  )
}
sample_counts <- rbindlist(list(
  summarise_cells(
    treated_cells[period == "Months -24 to -1"],
    "Treated sites: months -24 to -1"
  ),
  summarise_cells(reference_cells, "Treated sites: months -12 to -1"),
  summarise_cells(
    treated_cells[period == "Months +1 to +12"],
    "Treated sites: months +1 to +12"
  ),
  summarise_cells(
    used_cells,
    "Distinct treated and control observations used"
  )
))
fwrite(sample_counts, file.path(out_dir, "baseline_accident_count_summary.csv"))

message("Drawing the metropolitan-France map...")
outline <- read_polygon_shapefile(file.path(
  root, "data", "auxiliary", "DEPARTEMENT", "DEPARTEMENT.shp"
))
map_membership <- copy(membership[metropolitan_france == TRUE])
projected <- to_lambert93(map_membership$longitude, map_membership$latitude)
map_membership[, `:=`(x = projected$x, y = projected$y)]

cohort_levels <- levels(membership$deployment_cohort)
cohort_colours <- c(
  "Before 2008" = "#440154", "2008--2011" = "#3B528B",
  "2012--2015" = "#21918C", "2016--2019" = "#5DC863",
  "2020--2024" = "#FDE725"
)
role_levels <- c(
  "Not used in baseline", "Control only",
  "Treated: 2 km sample only", "Treated: all three radii"
)
map_membership[, estimation_role := factor(estimation_role, levels = role_levels)]

map_theme <- theme_void(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12.5),
    legend.key.width = grid::unit(1.1, "lines"),
    legend.key.height = grid::unit(1.0, "lines"),
    legend.spacing.y = grid::unit(0.15, "lines"),
    plot.margin = margin(8, 10, 6, 10)
  )

p_deployment <- ggplot() +
  geom_path(
    data = outline, aes(x, y, group = group),
    colour = "#B8B8B8", linewidth = 0.22
  ) +
  geom_point(
    data = map_membership[order(installation_year)],
    aes(x, y, colour = deployment_cohort),
    size = 1.0, alpha = 0.78
  ) +
  scale_colour_manual(values = cohort_colours, breaks = cohort_levels) +
  coord_equal(xlim = c(0, 1300000), ylim = c(6000000, 7200000), expand = FALSE) +
  labs(title = "(a) Deployment cohorts") +
  guides(colour = guide_legend(
    nrow = 3, byrow = TRUE,
    override.aes = list(size = 3.8, alpha = 1)
  )) +
  map_theme

role_shapes <- setNames(c(20, 0, 16, 17), role_levels)
role_colours <- setNames(
  c("#C7C7C7", "#555555", "#2166AC", "#B2182B"), role_levels
)
p_sample <- ggplot() +
  geom_path(
    data = outline, aes(x, y, group = group),
    colour = "#B8B8B8", linewidth = 0.22
  ) +
  geom_point(
    data = map_membership[estimation_role == "Not used in baseline"],
    aes(x, y, shape = estimation_role, colour = estimation_role),
    size = 0.82, alpha = 0.72
  ) +
  geom_point(
    data = map_membership[estimation_role != "Not used in baseline"],
    aes(x, y, shape = estimation_role, colour = estimation_role),
    size = 1.28, alpha = 0.9, stroke = 0.65
  ) +
  scale_shape_manual(values = role_shapes, breaks = role_levels, drop = FALSE) +
  scale_colour_manual(values = role_colours, breaks = role_levels, drop = FALSE) +
  coord_equal(xlim = c(0, 1300000), ylim = c(6000000, 7200000), expand = FALSE) +
  labs(title = "(b) Role in the estimation sample") +
  guides(
    colour = guide_legend(
      nrow = 4, byrow = TRUE,
      override.aes = list(
        shape = unname(role_shapes[role_levels]), size = 3.8, alpha = 1
      )
    ),
    shape = "none"
  ) +
  map_theme

figure <- p_deployment + p_sample + plot_layout(guides = "keep")
ggsave(
  file.path(out_dir, "radar_deployment_and_estimation_sample.pdf"),
  figure, width = 11.8, height = 8.0, units = "in", device = grDevices::pdf,
  useDingbats = FALSE, bg = "white"
)
ggsave(
  file.path(out_dir, "radar_deployment_and_estimation_sample.png"),
  figure, width = 11.8, height = 8.0, units = "in", dpi = 300, bg = "white"
)

message("Baseline sample membership:")
print(membership[, .N, by = estimation_role][order(estimation_role)])
message("Crash assignments:")
print(sample_counts)
