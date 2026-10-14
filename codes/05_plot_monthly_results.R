#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")

local <- fread(file.path(out_dir, "event_study_monthly_local_control.csv"))[
  outcome == "accidents_local_difference"
]
matched <- fread(file.path(out_dir, "event_study_monthly_matched_traffic.csv"))[
  outcome == "accidents_difference"
]
future <- fread(file.path(out_dir, "event_study_monthly_future_radar_controls.csv"))[
  outcome == "accidents" & radius_km == 0.5
]

positions <- c(-21.5, -15.5, -9.5, 3.5, 9.5, 15.5, 21.5)
labels <- c("-24 à -19", "-18 à -13", "-12 à -7",
            "+1 à +6", "+7 à +12", "+13 à +18", "+19 à +24")

draw_panel <- function(x, title, ylim) {
  plot(
    positions, x$estimate,
    type = "n", xlim = c(-25, 25), ylim = ylim,
    xaxt = "n", xlab = "",
    ylab = "Différence mensuelle d'accidents",
    main = title, cex.main = 0.95
  )
  abline(h = 0, col = "grey55", lty = 2)
  abline(v = 0, col = "firebrick3", lty = 3)
  segments(positions, x$conf_low_95, positions, x$conf_high_95,
           col = "steelblue4", lwd = 2)
  points(positions, x$estimate, pch = 19, col = "steelblue4", cex = 0.9)
  axis(1, at = positions, labels = labels, las = 2, cex.axis = 0.78)
  mtext("Mois relativement à l'installation", side = 1, line = 4.7, cex = 0.85)
  text(0.8, ylim[2] * 0.92, "installation", col = "firebrick3",
       adj = 0, cex = 0.8)
}

png(
  file.path(out_dir, "event_study_monthly_accidents.png"),
  width = 2400, height = 850, res = 180
)
par(mfrow = c(1, 3), mar = c(6.5, 4.5, 3.5, 1.5), oma = c(0, 0, 1, 0))
draw_panel(
  local,
  "Contrôle local (282 radars)",
  range(c(local$conf_low_95, local$conf_high_95, 0))
)
draw_panel(
  future,
  "Futurs radars témoins (372–634 traités)",
  range(c(future$conf_low_95, future$conf_high_95, 0))
)
draw_panel(
  matched,
  "Matching TMJA pré-traitement (23 paires)",
  range(c(matched$conf_low_95, matched$conf_high_95, 0))
)
mtext("Études d'événement mensuelles — accidents corporels", outer = TRUE,
      font = 2, cex = 1.05)
dev.off()
