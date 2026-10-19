#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")

result_files <- data.table(
  corridor_half_width_m = c(50, 100),
  event_file = c(
    "event_study_monthly_road_corridor_network_balanced12_reach75.csv",
    "event_study_monthly_road_corridor_network_100m_balanced12_reach75.csv"
  ),
  bootstrap_file = c(
    "bootstrap_monthly_road_corridor_network_balanced12_reach75.csv",
    "bootstrap_monthly_road_corridor_network_100m_balanced12_reach75.csv"
  ),
  average_file = c(
    "estimates_monthly_road_corridor_network_balanced12_reach75.csv",
    "estimates_monthly_road_corridor_network_100m_balanced12_reach75.csv"
  ),
  pretrend_file = c(
    "pretrend_tests_monthly_road_corridor_network_balanced12_reach75.csv",
    "pretrend_tests_monthly_road_corridor_network_100m_balanced12_reach75.csv"
  )
)

events <- rbindlist(lapply(seq_len(nrow(result_files)), function(i) {
  x <- fread(file.path(out_dir, result_files$event_file[i]))
  x[, corridor_half_width_m := result_files$corridor_half_width_m[i]]
  x
}))

averages <- rbindlist(lapply(seq_len(nrow(result_files)), function(i) {
  x <- fread(file.path(out_dir, result_files$average_file[i]))
  x[, corridor_half_width_m := result_files$corridor_half_width_m[i]]
  x
}))

pretrends <- rbindlist(lapply(seq_len(nrow(result_files)), function(i) {
  x <- fread(file.path(out_dir, result_files$pretrend_file[i]))
  x[, corridor_half_width_m := result_files$corridor_half_width_m[i]]
  x
}))

summary <- merge(
  averages[outcome == "accidents" & window == "months_1_12"],
  pretrends[outcome == "accidents", .(
    corridor_half_width_m, radius_km, pretrend_p_value
  )],
  by = c("corridor_half_width_m", "radius_km"),
  all.x = TRUE
)
setorder(summary, corridor_half_width_m, radius_km)
fwrite(summary, file.path(out_dir, "network_baseline_summary.csv"))

events <- events[outcome == "accidents" & radius_km == 2]
window_order <- c("-24:-19", "-18:-13", "+1:+6", "+7:+12")
events <- events[window %chin% window_order]
events[, x := match(window, window_order)]
setorder(events, corridor_half_width_m, x)

# Simultaneous bands refer only to the four bins displayed here.
for (width in c(50, 100)) {
  rows <- which(events$corridor_half_width_m == width)
  boot <- fread(file.path(
    out_dir, result_files[corridor_half_width_m == width, bootstrap_file]
  ))[outcome == "accidents" & radius_km == 2 &
       window_type == "event" & window %chin% window_order]
  boot <- dcast(boot, bootstrap_draw ~ window, value.var = "estimate")
  setcolorder(boot, c("bootstrap_draw", window_order))
  deviations <- sweep(as.matrix(boot[, -"bootstrap_draw"]), 2,
                      events$estimate[rows], "-")
  t_draws <- sweep(deviations, 2, events$std_error_bootstrap[rows], "/")
  critical <- unname(quantile(apply(abs(t_draws), 1, max), 0.95))
  events$conf_low_simultaneous_95[rows] <-
    events$estimate[rows] - critical * events$std_error_bootstrap[rows]
  events$conf_high_simultaneous_95[rows] <-
    events$estimate[rows] + critical * events$std_error_bootstrap[rows]
}

colors <- c(`50` = "#2B6F9F", `100` = "#D65F3C")
shifts <- c(`50` = -0.07, `100` = 0.07)
ylim <- range(c(
  events$conf_low_simultaneous_95,
  events$conf_high_simultaneous_95,
  0
), finite = TRUE)

png(
  file.path(out_dir, "event_study_road_corridors.png"),
  width = 2200, height = 1350, res = 220
)
par(mar = c(5.8, 6.8, 1.0, 1.0), mgp = c(3.6, 1.0, 0), tcl = -0.3)
plot(
  NA, NA, xlim = c(0.5, 4.5), ylim = ylim, xaxt = "n",
  xlab = "Months relative to camera installation",
  ylab = "Monthly difference in injury crashes",
  cex.axis = 1.35, cex.lab = 1.55
)
abline(h = 0, col = "#777777", lty = 2, lwd = 1.5)
abline(v = 2.5, col = "#A9A9A9", lty = 3, lwd = 1.5)
for (width in c(50, 100)) {
  z <- events[corridor_half_width_m == width]
  x <- z$x + shifts[as.character(width)]
  segments(x, z$conf_low_simultaneous_95, x, z$conf_high_simultaneous_95,
           col = colors[as.character(width)], lwd = 3)
  points(x, z$estimate, pch = if (width == 50) 19 else 17,
         col = colors[as.character(width)], cex = 1.45)
}
axis(1, at = seq_along(window_order), labels = window_order,
     las = 1, cex.axis = 1.15)
legend(
  "bottomleft", legend = c("50 m half-width", "100 m half-width"),
  col = colors, pch = c(19, 17), bty = "n", cex = 1.3, pt.cex = 1.35
)
dev.off()

print(summary[, .(
  corridor_half_width_m, radius_km, estimate, conf_low_95, conf_high_95,
  p_value_normal, pretrend_p_value, treated_radars
)])
