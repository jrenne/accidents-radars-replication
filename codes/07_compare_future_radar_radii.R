#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
script <- file.path(root, "codes", "06_monthly_future_radar_controls.R")
out_dir <- file.path(root, "results", "current")

specifications <- data.table(
  radius_km = c(0.25, 0.5, 1.0, 2.0, 3.0, 5.0),
  suffix = c(
    "_radius_025km", "_radius_050km", "_radius_100km",
    "_radius_200km", "_radius_300km", "_radius_500km"
  )
)

exclude_large_cities <- Sys.getenv("EXCLUDE_LARGE_CITIES", "0") == "1"
analysis_suffix <- if (exclude_large_cities) "_no_large_cities" else ""
specifications[, output_suffix := paste0(analysis_suffix, suffix)]

requested_radii <- Sys.getenv("RUN_RADII", "")
run_specifications <- if (nzchar(requested_radii)) {
  requested <- as.numeric(strsplit(requested_radii, ",", fixed = TRUE)[[1]])
  specifications[radius_km %in% requested]
} else {
  specifications
}
combine_specifications <- if (nzchar(requested_radii)) run_specifications else specifications

for (i in seq_len(nrow(run_specifications))) {
  status <- system2(
    "Rscript",
    script,
    env = c(
      paste0("OUTCOME_RADIUS_KM=", run_specifications$radius_km[i]),
      paste0("OUTPUT_SUFFIX=", run_specifications$output_suffix[i]),
      paste0("EXCLUDE_LARGE_CITIES=", as.integer(exclude_large_cities))
    )
  )
  if (status != 0L) stop("Échec de l'estimation au rayon ", run_specifications$radius_km[i], " km")
}

event_results <- rbindlist(lapply(combine_specifications$output_suffix, function(suffix) {
  fread(file.path(out_dir, paste0(
    "event_study_monthly_future_radar_controls", suffix, ".csv"
  )))
}))

average_results <- rbindlist(lapply(combine_specifications$output_suffix, function(suffix) {
  fread(file.path(out_dir, paste0(
    "estimates_monthly_future_radar_controls", suffix, ".csv"
  )))
}))

fwrite(
  event_results,
  file.path(out_dir, paste0(
    "event_study_monthly_future_radar_controls_by_radius", analysis_suffix, ".csv"
  ))
)
fwrite(
  average_results,
  file.path(out_dir, paste0(
    "estimates_monthly_future_radar_controls_by_radius", analysis_suffix, ".csv"
  ))
)

print(average_results)
