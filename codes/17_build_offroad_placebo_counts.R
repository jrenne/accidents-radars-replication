#!/usr/bin/env Rscript

# Crashes inside the Euclidean circle but outside the 100 m corridor around the
# monitored road. This is a spatial negative-control outcome.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")

circle <- fread(file.path(out_dir, "circle_monthly_counts.csv.gz"))
road <- fread(file.path(out_dir, "road_corridor_monthly_counts_100m.csv.gz"))
setkey(circle, cle_radar, radius_km, month_index)
setkey(road, cle_radar, radius_km, month_index)

placebo <- road[circle]
stopifnot(nrow(placebo) == nrow(circle))
placebo[, `:=`(
  accidents = i.accidents - accidents,
  killed = i.killed - killed
)]
if (placebo[, any(accidents < 0 | killed < 0)]) {
  stop("Road-corridor counts are not nested inside circle counts.")
}
placebo <- placebo[, .(radius_km, month_index, accidents, killed, cle_radar)]
setorder(placebo, cle_radar, radius_km, month_index)
fwrite(placebo, file.path(out_dir, "offroad_circle_monthly_counts_100m.csv.gz"))

message("Off-road placebo totals:")
print(placebo[, .(
  sites = uniqueN(cle_radar), accidents = sum(accidents), killed = sum(killed)
), by = radius_km])
