#!/usr/bin/env Rscript

# Influence of installation cohorts on the preferred 2-km, 100-m estimate.
# Nuisance predictions and within-stack weights remain fixed at their
# full-sample cross-fitted values; each cohort is removed from the final
# equal-stack aggregation in turn.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")
input_file <- file.path(
  out_dir,
  "leave_one_cohort_out_monthly_road_corridor_network_100m_balanced12_reach75.csv"
)
if (!file.exists(input_file)) stop("Missing cohort diagnostic: ", input_file)

x <- fread(input_file)[
  outcome == "accidents" & radius_km == 2 & window == "months_1_12"
]
baseline <- x[is.na(excluded_cohort), estimate]
cohorts <- x[!is.na(excluded_cohort)]
setorder(cohorts, excluded_cohort)
fwrite(cohorts, file.path(out_dir, "baseline_leave_one_cohort_out.csv"))

draw_plot <- function() {
  ylim <- range(c(cohorts$estimate, baseline, 0))
  ylim <- ylim + c(-0.003, 0.003)
  par(mar = c(5.0, 6.5, 0.8, 0.8), mgp = c(3.4, 0.9, 0), tcl = -0.25)
  plot(
    cohorts$excluded_cohort, cohorts$estimate,
    type = "b", pch = 19, lwd = 2.2, cex = 1.2, col = "#2B6F9F",
    xlab = "Excluded installation cohort",
    ylab = "First-year effect on monthly injury crashes",
    xaxt = "n", ylim = ylim, cex.axis = 1.15, cex.lab = 1.25
  )
  axis(1, at = cohorts$excluded_cohort, labels = cohorts$excluded_cohort,
       las = 2, cex.axis = 1.0)
  abline(h = 0, col = "#777777", lty = 3, lwd = 1.2)
  abline(h = baseline, col = "#D65F3C", lty = 2, lwd = 2)
  legend(
    "topright", bty = "n",
    legend = c("Leave-one-cohort-out", sprintf("Full sample: %.4f", baseline)),
    col = c("#2B6F9F", "#D65F3C"), pch = c(19, NA), lty = c(1, 2),
    lwd = c(2.2, 2), cex = 1.05
  )
}

png(
  file.path(out_dir, "leave_one_cohort_out_2km_100m.png"),
  width = 1900, height = 1200, res = 220
)
draw_plot()
dev.off()

pdf(
  file.path(out_dir, "leave_one_cohort_out_2km_100m.pdf"),
  width = 8.6, height = 5.5, useDingbats = FALSE
)
draw_plot()
dev.off()

print(cohorts[, .(excluded_cohort, estimate, treated_stacks)])
