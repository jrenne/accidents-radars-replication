#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(igraph)
})

source(file.path("R", "network_corridor_helpers.R"))

# A synthetic L-shaped road: the camera is 400 m along the horizontal arm.
segments <- data.table(
  segment_id = 1:2,
  x1 = c(0, 1000), y1 = c(0, 0),
  x2 = c(1000, 1000), y2 = c(0, 1000),
  from_key = c("a", "b"), to_key = c("b", "c")
)
segments[, length_m := sqrt((x2 - x1)^2 + (y2 - y1)^2)]
snapped <- list(segment_id = 1L, fraction = 0.4, distance_m = 0, x = 400, y = 0)
node_distances <- network_node_distances(segments, snapped)
stopifnot(all.equal(unname(node_distances[c("a", "b", "c")]), c(400, 600, 1600)))

reach_500 <- clip_network_segments(segments, node_distances, 500)
stopifnot(abs(sum(reach_500$length_m) - 900) < 1e-8)
stopifnot(nrow(reach_500) == 1L)

reach_700 <- clip_network_segments(segments, node_distances, 700)
stopifnot(abs(sum(reach_700$length_m) - 1100) < 1e-8)
stopifnot(minimum_segment_distance_m(1000, 50, reach_700) < 1e-8)
stopifnot(abs(minimum_segment_distance_m(1000, 200, reach_700) - 100) < 1e-8)

message("Network-corridor helper tests passed.")
