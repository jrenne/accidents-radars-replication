# Helpers for constructing road-network corridors around a speed camera.
# Coordinates are expressed in a local metric system centred on the camera.

geometry_lines <- function(geometry) {
  if (is.null(geometry) || is.null(geometry$type)) return(list())
  if (identical(geometry$type, "LineString")) {
    return(list(geometry$coordinates))
  }
  if (identical(geometry$type, "MultiLineString")) {
    return(geometry$coordinates)
  }
  list()
}

features_to_network_segments <- function(features, latitude, longitude,
                                         node_snap_tolerance_m = 2) {
  rows <- list()
  cursor <- 1L
  for (feature_index in seq_along(features)) {
    lines <- geometry_lines(features[[feature_index]]$geometry)
    for (line_index in seq_along(lines)) {
      coordinates <- lines[[line_index]]
      xy <- t(vapply(
        coordinates,
        function(point) as.numeric(unlist(point))[1:2],
        numeric(2)
      ))
      if (nrow(xy) < 2L) next
      x <- (xy[, 1] - longitude) * 111320 * cos(latitude * pi / 180)
      y <- (xy[, 2] - latitude) * 110574
      block <- data.table::data.table(
        feature_index = feature_index,
        line_index = line_index,
        vertex_index = seq_len(length(x) - 1L),
        x1 = x[-length(x)], y1 = y[-length(y)],
        x2 = x[-1L], y2 = y[-1L]
      )
      block[, length_m := sqrt((x2 - x1)^2 + (y2 - y1)^2)]
      block <- block[is.finite(length_m) & length_m > 0.05]
      if (nrow(block)) {
        rows[[cursor]] <- block
        cursor <- cursor + 1L
      }
    }
  }
  if (!length(rows)) return(data.table::data.table())
  segments <- data.table::rbindlist(rows)
  node_key <- function(x, y) paste(
    round(x / node_snap_tolerance_m),
    round(y / node_snap_tolerance_m),
    sep = ":"
  )
  segments[, from_key := node_key(x1, y1)]
  segments[, to_key := node_key(x2, y2)]
  segments[, segment_id := .I]
  segments
}

project_origin_to_segments <- function(segments) {
  dx <- segments$x2 - segments$x1
  dy <- segments$y2 - segments$y1
  denominator <- dx^2 + dy^2
  fraction <- ifelse(
    denominator > 0,
    pmax(0, pmin(1, -(segments$x1 * dx + segments$y1 * dy) / denominator)),
    0
  )
  projected_x <- segments$x1 + fraction * dx
  projected_y <- segments$y1 + fraction * dy
  distance_m <- sqrt(projected_x^2 + projected_y^2)
  best <- which.min(distance_m)
  list(
    segment_id = segments$segment_id[best],
    fraction = fraction[best],
    distance_m = distance_m[best],
    x = projected_x[best],
    y = projected_y[best]
  )
}

network_node_distances <- function(segments, snapped) {
  nodes <- unique(c(segments$from_key, segments$to_key))
  edges <- data.frame(
    from = segments$from_key,
    to = segments$to_key,
    length_m = segments$length_m,
    stringsAsFactors = FALSE
  )
  graph <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  source_edge <- segments[segment_id == snapped$segment_id]
  source_nodes <- c(source_edge$from_key, source_edge$to_key)
  source_offsets <- c(
    snapped$fraction * source_edge$length_m,
    (1 - snapped$fraction) * source_edge$length_m
  )
  distance_matrix <- igraph::distances(
    graph,
    v = source_nodes,
    to = igraph::V(graph),
    weights = igraph::E(graph)$length_m
  )
  distances <- pmin(
    source_offsets[1] + distance_matrix[1, ],
    source_offsets[2] + distance_matrix[2, ]
  )
  result <- stats::setNames(as.numeric(distances), colnames(distance_matrix))
  attr(result, "source_segment_id") <- snapped$segment_id
  attr(result, "source_fraction") <- snapped$fraction
  result
}

clip_network_segments <- function(segments, node_distances, radius_m) {
  segments <- data.table::copy(segments)
  source_segment_id <- attr(node_distances, "source_segment_id")
  source_fraction <- attr(node_distances, "source_fraction")
  segments[, `:=`(
    distance_from = node_distances[from_key],
    distance_to = node_distances[to_key]
  )]
  pieces <- vector("list", 2L * nrow(segments))
  cursor <- 1L
  for (i in seq_len(nrow(segments))) {
    edge <- segments[i]
    length_m <- edge$length_m
    left_end <- if (is.finite(edge$distance_from)) {
      pmax(0, pmin(1, (radius_m - edge$distance_from) / length_m))
    } else 0
    right_start <- if (is.finite(edge$distance_to)) {
      pmax(0, pmin(1, 1 - (radius_m - edge$distance_to) / length_m))
    } else 1
    intervals <- list()
    if (left_end > 0) intervals[[length(intervals) + 1L]] <- c(0, left_end)
    if (right_start < 1) intervals[[length(intervals) + 1L]] <- c(right_start, 1)
    if (length(source_segment_id) && edge$segment_id == source_segment_id) {
      intervals[[length(intervals) + 1L]] <- c(
        pmax(0, source_fraction - radius_m / length_m),
        pmin(1, source_fraction + radius_m / length_m)
      )
    }
    if (!length(intervals)) next
    intervals <- intervals[order(vapply(intervals, `[[`, numeric(1), 1L))]
    merged <- list(intervals[[1]])
    if (length(intervals) > 1L) {
      for (j in 2:length(intervals)) {
        last <- length(merged)
        if (intervals[[j]][1] <= merged[[last]][2]) {
          merged[[last]][2] <- max(merged[[last]][2], intervals[[j]][2])
        } else {
          merged[[length(merged) + 1L]] <- intervals[[j]]
        }
      }
    }
    intervals <- merged
    dx <- edge$x2 - edge$x1
    dy <- edge$y2 - edge$y1
    for (interval in intervals) {
      if (interval[2] - interval[1] <= 1e-10) next
      pieces[[cursor]] <- data.table::data.table(
        source_segment_id = edge$segment_id,
        x1 = edge$x1 + interval[1] * dx,
        y1 = edge$y1 + interval[1] * dy,
        x2 = edge$x1 + interval[2] * dx,
        y2 = edge$y1 + interval[2] * dy
      )
      cursor <- cursor + 1L
    }
  }
  pieces <- pieces[seq_len(cursor - 1L)]
  if (!length(pieces)) return(data.table::data.table())
  result <- data.table::rbindlist(pieces)
  result[, length_m := sqrt((x2 - x1)^2 + (y2 - y1)^2)]
  result
}

minimum_segment_distance_m <- function(x, y, segments) {
  minimum <- rep(Inf, length(x))
  if (!nrow(segments)) return(minimum)
  for (j in seq_len(nrow(segments))) {
    dx <- segments$x2[j] - segments$x1[j]
    dy <- segments$y2[j] - segments$y1[j]
    denominator <- dx^2 + dy^2
    fraction <- if (denominator > 0) {
      pmax(0, pmin(
        1,
        ((x - segments$x1[j]) * dx + (y - segments$y1[j]) * dy) / denominator
      ))
    } else rep(0, length(x))
    distance <- sqrt(
      (x - (segments$x1[j] + fraction * dx))^2 +
        (y - (segments$y1[j] + fraction * dy))^2
    )
    minimum <- pmin(minimum, distance)
  }
  minimum
}
