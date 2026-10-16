#!/usr/bin/env Rscript

# Baseline overlap and covariate-balance diagnostics for the preferred
# 2-km, 100-m half-width, first-year specification.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")

balance_file <- file.path(
  out_dir,
  "balance_monthly_road_corridor_network_100m_balanced12_reach75.csv"
)
diagnostic_file <- file.path(
  out_dir,
  "diagnostics_monthly_road_corridor_network_100m_balanced12_reach75.csv"
)
if (!file.exists(balance_file)) stop("Missing balance output: ", balance_file)
if (!file.exists(diagnostic_file)) stop("Missing diagnostic output: ", diagnostic_file)

labels <- c(
  pre_24_13 = "Crashes, months -24 to -13",
  pre_12_7 = "Crashes, months -12 to -7",
  pre_6_1 = "Crashes, months -6 to -1",
  road_importance_num = "Road importance",
  road_urban_num = "Urban road",
  road_speed_mean = "Mean mapped-road speed"
)

balance <- fread(balance_file)[
  outcome == "accidents" & radius_km == 2 & window == "months_1_12"
]
balance[, variable_label := labels[variable]]
balance[, display_order := match(variable, names(labels))]
setorder(balance, display_order)

diagnostics <- fread(diagnostic_file)[
  outcome == "accidents" & radius_km == 2 & window == "months_1_12"
]
overlap <- diagnostics[, .(
  radius_km, corridor_half_width_m, stacks, unique_treated_sites,
  unique_control_sites, median_raw_controls, median_effective_controls,
  propensity_min_raw, propensity_max_raw, propensity_share_trimmed
)]

fwrite(balance, file.path(out_dir, "baseline_balance_summary.csv"))
fwrite(overlap, file.path(out_dir, "baseline_overlap_summary.csv"))

draw_plot <- function() {
  y <- rev(seq_len(nrow(balance)))
  x_unweighted <- balance$standardized_difference_unweighted
  x_weighted <- balance$standardized_difference_weighted
  x_limit <- max(0.22, max(abs(c(x_unweighted, x_weighted)), na.rm = TRUE) + 0.03)
  par(mar = c(4.8, 12.5, 0.8, 0.8), mgp = c(2.8, 0.8, 0), tcl = -0.25)
  plot(
    NA, NA, xlim = c(-x_limit, x_limit), ylim = c(0.5, nrow(balance) + 0.5),
    yaxt = "n", xlab = "Standardized difference: treated minus controls",
    ylab = "", cex.axis = 1.15, cex.lab = 1.25
  )
  abline(v = 0, col = "#777777", lwd = 1.2)
  abline(v = c(-0.1, 0.1), col = "#B6B6B6", lty = 2, lwd = 1.2)
  segments(x_unweighted, y, x_weighted, y, col = "#A8A8A8", lwd = 2)
  points(x_unweighted, y, pch = 1, cex = 1.35, lwd = 2, col = "#666666")
  points(x_weighted, y, pch = 19, cex = 1.15, col = "#2B6F9F")
  axis(2, at = y, labels = balance$variable_label, las = 1, tick = FALSE,
       cex.axis = 1.05)
  legend(
    "bottomleft", inset = c(0, -0.02), bty = "n", horiz = TRUE,
    legend = c("Unweighted controls", "Propensity-weighted controls"),
    pch = c(1, 19), col = c("#666666", "#2B6F9F"),
    pt.cex = c(1.25, 1.05), cex = 1.0
  )
}

png(
  file.path(out_dir, "covariate_balance_2km_100m.png"),
  width = 2100, height = 1250, res = 220
)
draw_plot()
dev.off()

pdf(
  file.path(out_dir, "covariate_balance_2km_100m.pdf"),
  width = 9.5, height = 5.7, useDingbats = FALSE
)
draw_plot()
dev.off()

print(balance[, .(
  variable, standardized_difference_unweighted,
  standardized_difference_weighted
)])
print(overlap)
