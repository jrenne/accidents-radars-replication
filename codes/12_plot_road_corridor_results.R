#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")
files <- data.table(
  width = c(50, 100),
  event_file = c(
    "event_study_monthly_road_corridor.csv",
    "event_study_monthly_road_corridor_100m.csv"
  ),
  average_file = c(
    "estimates_monthly_road_corridor.csv",
    "estimates_monthly_road_corridor_100m.csv"
  ),
  exact_file = c(
    "estimates_monthly_road_corridor_exact_matches.csv",
    "estimates_monthly_road_corridor_exact_matches_100m.csv"
  )
)

events <- rbindlist(lapply(seq_len(nrow(files)), function(i) {
  x <- fread(file.path(out_dir, files$event_file[i]))
  x[, corridor_width := files$width[i]]
  x
}))
averages <- rbindlist(lapply(seq_len(nrow(files)), function(i) {
  x <- fread(file.path(out_dir, files$average_file[i]))
  x[, `:=`(corridor_width = files$width[i], sample = "all_matches")]
  x
}))
exact <- rbindlist(lapply(seq_len(nrow(files)), function(i) {
  x <- fread(file.path(out_dir, files$exact_file[i]))
  x[, `:=`(corridor_width = files$width[i], sample = "exact_route_matches")]
  x
}))
fwrite(
  rbindlist(list(averages, exact), fill = TRUE),
  file.path(out_dir, "road_corridor_robustness_summary.csv")
)

events <- events[outcome == "accidents"]
window_order <- c("-24:-19", "-18:-13", "-12:-7", "+1:+6", "+7:+12", "+13:+18", "+19:+24")
events[, x := match(event_window_months, window_order)]
common_ylim <- range(c(events$conf_low_95, events$conf_high_95, 0))
colors <- c(`50` = "#35698f", `100` = "#e07a3f")

png(
  file.path(out_dir, "event_study_road_corridors.png"),
  width = 3000, height = 1100, res = 180
)
par(
  mfrow = c(1, 3),
  mar = c(5.8, 3.6, 3.4, 1.0),
  oma = c(3.2, 4.6, 0.2, 0.2),
  mgp = c(2.7, 0.8, 0),
  tcl = -0.3
)
for (radius in c(1, 2, 3)) {
  d <- events[radius_km == radius]
  plot(
    NA, NA, xlim = c(0.5, 7.5), ylim = common_ylim, xaxt = "n",
    xlab = "", ylab = "",
    main = paste0("Radius: ", radius, " km"),
    cex.main = 2.0, cex.axis = 1.5
  )
  abline(h = 0, col = "#999999", lty = 2, lwd = 1.4)
  abline(v = 3.5, col = "#d9534f", lty = 3, lwd = 1.4)
  for (width in c(50, 100)) {
    z <- d[corridor_width == width][order(x)]
    shift <- if (width == 50) -0.08 else 0.08
    segments(z$x + shift, z$conf_low_95, z$x + shift, z$conf_high_95,
             col = colors[as.character(width)], lwd = 3)
    points(z$x + shift, z$estimate, pch = if (width == 50) 19 else 17,
           col = colors[as.character(width)], cex = 1.35)
  }
  axis(
    1, at = seq_along(window_order), labels = window_order,
    las = 2, cex.axis = 1.5
  )
  if (radius == 1) legend(
    "bottomleft", legend = c("±50 m", "±100 m"),
    col = colors, pch = c(19, 17), bty = "n",
    cex = 1.5, pt.cex = 1.35
  )
}
mtext(
  "Months relative to camera installation",
  outer = TRUE, side = 1, line = 1.3, cex = 1.75
)
mtext(
  "Monthly difference in injury crashes",
  outer = TRUE, side = 2, line = 2.7, cex = 1.75
)
dev.off()
