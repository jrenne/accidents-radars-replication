#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")
events <- fread(file.path(
  out_dir, "event_study_monthly_future_radar_controls_by_radius.csv"
))[outcome == "accidents"]

window_order <- c("-24:-19", "-18:-13", "-12:-7", "+1:+6", "+7:+12", "+13:+18", "+19:+24")
events[, x := match(event_window_months, window_order)]
small_events <- events[radius_km %in% c(0.25, 0.5, 1.0)]
common_ylim <- range(c(small_events$conf_low_95, small_events$conf_high_95, 0))

png(
  file.path(out_dir, "event_study_future_radars_by_radius.png"),
  width = 2400, height = 850, res = 150
)
par(mfrow = c(1, 3), mar = c(7, 5, 4, 1), oma = c(0, 0, 3, 0))

for (radius in c(0.25, 0.5, 1.0)) {
  d <- events[radius_km == radius][order(x)]
  plot(
    d$x, d$estimate,
    ylim = common_ylim, xlim = c(0.5, 7.5), xaxt = "n",
    pch = 19, col = "#35698f", xlab = "Mois relativement à l'installation",
    ylab = "Différence mensuelle d'accidents",
    main = paste0("Rayon de ", format(radius, decimal.mark = ","), " km")
  )
  segments(d$x, d$conf_low_95, d$x, d$conf_high_95, col = "#35698f", lwd = 2)
  abline(h = 0, col = "#999999", lty = 2)
  abline(v = 3.5, col = "#e45756", lty = 3)
  axis(1, at = seq_along(window_order), labels = window_order, las = 2, cex.axis = 0.8)
  text(3.6, common_ylim[2] * 0.92, "installation", col = "#e45756", pos = 4, cex = 0.8)
}

mtext(
  "Diff-in-diff avec futurs radars témoins — sensibilité au rayon",
  outer = TRUE, side = 3, line = 1, cex = 1.4, font = 2
)
dev.off()

large_events <- events[radius_km %in% c(2.0, 3.0, 5.0)]
large_ylim <- range(c(large_events$conf_low_95, large_events$conf_high_95, 0))
png(
  file.path(out_dir, "event_study_future_radars_large_radii.png"),
  width = 2400, height = 850, res = 150
)
par(mfrow = c(1, 3), mar = c(7, 5, 4, 1), oma = c(0, 0, 3, 0))
for (radius in c(2.0, 3.0, 5.0)) {
  d <- large_events[radius_km == radius][order(x)]
  plot(
    d$x, d$estimate,
    ylim = large_ylim, xlim = c(0.5, 7.5), xaxt = "n",
    pch = 19, col = "#35698f", xlab = "Mois relativement à l'installation",
    ylab = "Différence mensuelle d'accidents",
    main = paste0("Rayon de ", format(radius, decimal.mark = ","), " km")
  )
  segments(d$x, d$conf_low_95, d$x, d$conf_high_95, col = "#35698f", lwd = 2)
  abline(h = 0, col = "#999999", lty = 2)
  abline(v = 3.5, col = "#e45756", lty = 3)
  axis(1, at = seq_along(window_order), labels = window_order, las = 2, cex.axis = 0.8)
  text(3.6, large_ylim[2] * 0.92, "installation", col = "#e45756", pos = 4, cex = 0.8)
}
mtext(
  "Futurs radars témoins — grands rayons (échantillons isolés)",
  outer = TRUE, side = 3, line = 1, cex = 1.4, font = 2
)
dev.off()
