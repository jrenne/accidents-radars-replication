#!/usr/bin/env Rscript

# Sensitivity of the preferred 2-km, 100-m first-year estimate to the minimum
# share of nominal two-sided road-network reach required at each camera site.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")

thresholds <- c(50L, 75L, 90L)
files <- file.path(
  out_dir,
  sprintf(
    "estimates_monthly_road_corridor_dr_network_100m_balanced12_reach%d.csv",
    thresholds
  )
)
missing <- files[!file.exists(files)]
if (length(missing)) stop("Missing completeness-sensitivity input(s): ", paste(missing, collapse = ", "))

results <- rbindlist(lapply(seq_along(files), function(i) {
  x <- fread(files[i])[
    outcome == "accidents" & radius_km == 2 & window == "months_1_12"
  ]
  if (nrow(x) != 1L) stop("Expected one preferred estimate in ", files[i])
  pretrend_file <- file.path(
    out_dir,
    sprintf(
      "pretrend_tests_monthly_road_corridor_dr_network_100m_balanced12_reach%d.csv",
      thresholds[i]
    )
  )
  if (!file.exists(pretrend_file)) stop("Missing pretrend input: ", pretrend_file)
  pretrend <- fread(pretrend_file)[outcome == "accidents" & radius_km == 2]
  if (nrow(pretrend) != 1L) stop("Expected one pretrend test in ", pretrend_file)
  x[, pretrend_p_value := pretrend$pretrend_p_value]
  x[, completeness_threshold := thresholds[i] / 100]
  x
}), fill = TRUE)

setorder(results, completeness_threshold)
keep <- c(
  "completeness_threshold", "treated_radars", "estimate",
  "std_error_bootstrap", "conf_low_95", "conf_high_95",
  "p_value_normal", "mean_monthly_reference", "pretrend_p_value"
)
fwrite(
  results[, ..keep],
  file.path(out_dir, "baseline_graph_completeness_sensitivity.csv")
)

draw_plot <- function() {
  y_lim <- range(c(results$conf_low_95, results$conf_high_95, 0))
  y_lim <- y_lim + c(-0.002, 0.002)
  par(mar = c(5.0, 6.3, 0.8, 0.8), mgp = c(3.4, 0.9, 0), tcl = -0.25)
  plot(
    100 * results$completeness_threshold, results$estimate,
    pch = 19, type = "b", lwd = 2.2, cex = 1.25, col = "#2B6F9F",
    xlab = "Minimum share of nominal network reach (%)",
    ylab = "First-year effect on monthly injury crashes",
    xaxt = "n", ylim = y_lim, cex.axis = 1.15, cex.lab = 1.25
  )
  axis(1, at = 100 * results$completeness_threshold,
       labels = 100 * results$completeness_threshold, cex.axis = 1.15)
  arrows(
    100 * results$completeness_threshold, results$conf_low_95,
    100 * results$completeness_threshold, results$conf_high_95,
    angle = 90, code = 3, length = 0.07, lwd = 1.8, col = "#2B6F9F"
  )
  abline(h = 0, col = "#777777", lty = 3, lwd = 1.2)
  text(
    100 * results$completeness_threshold,
    results$conf_low_95 - 0.0012,
    labels = paste0("N = ", results$treated_radars),
    cex = 1.0, col = "#444444"
  )
}

png(
  file.path(out_dir, "graph_completeness_sensitivity_2km_100m.png"),
  width = 1900, height = 1200, res = 220
)
draw_plot()
dev.off()

pdf(
  file.path(out_dir, "graph_completeness_sensitivity_2km_100m.pdf"),
  width = 8.6, height = 5.5, useDingbats = FALSE
)
draw_plot()
dev.off()

print(results[, ..keep])
