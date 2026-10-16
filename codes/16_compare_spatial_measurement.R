#!/usr/bin/env Rscript

# Apples-to-apples comparison of three spatial outcome definitions. All three
# designs use the same sites, risk sets, nuisance specification, and bootstrap
# draws; only the assignment of crashes to a camera changes.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")

designs <- data.table(
  design = c("circle", "offroad", "road_euclidean", "road_network"),
  result_file = c(
    "estimates_monthly_road_corridor_circle_100m_balanced12_reach75.csv",
    "estimates_monthly_road_corridor_offroad_100m_balanced12_reach75.csv",
    "estimates_monthly_road_corridor_100m_balanced12_reach75.csv",
    "estimates_monthly_road_corridor_network_100m_balanced12_reach75.csv"
  ),
  bootstrap_file = c(
    "bootstrap_monthly_road_corridor_circle_100m_balanced12_reach75.csv",
    "bootstrap_monthly_road_corridor_offroad_100m_balanced12_reach75.csv",
    "bootstrap_monthly_road_corridor_100m_balanced12_reach75.csv",
    "bootstrap_monthly_road_corridor_network_100m_balanced12_reach75.csv"
  )
)

summary <- rbindlist(lapply(seq_len(nrow(designs)), function(i) {
  x <- fread(file.path(out_dir, designs$result_file[i]))[
    outcome == "accidents" & radius_km == 2 & window == "months_1_12"
  ]
  x[, design := designs$design[i]]
  x
}), fill = TRUE)

summary[, `:=`(
  standard_error_ratio_to_network =
    std_error_bootstrap / std_error_bootstrap[design == "road_network"],
  reference_ratio_to_network =
    mean_monthly_reference / mean_monthly_reference[design == "road_network"]
)]
setcolorder(summary, c(
  "design", "estimate", "std_error_bootstrap", "conf_low_95", "conf_high_95",
  "p_value_normal", "mean_monthly_reference", "effect_percent_of_reference",
  "standard_error_ratio_to_network", "reference_ratio_to_network"
))

boot <- rbindlist(lapply(seq_len(nrow(designs)), function(i) {
  x <- fread(file.path(out_dir, designs$bootstrap_file[i]))[
    outcome == "accidents" & radius_km == 2 & window == "months_1_12"
  ]
  x[, design := designs$design[i]]
  x
}), fill = TRUE)
boot_wide <- dcast(boot, bootstrap_draw ~ design, value.var = "estimate")

pairs <- list(
  c("road_network", "circle"),
  c("road_euclidean", "circle"),
  c("road_network", "road_euclidean"),
  c("road_euclidean", "offroad"),
  c("road_network", "offroad")
)
paired <- rbindlist(lapply(pairs, function(pair) {
  design_a <- pair[1]
  design_b <- pair[2]
  point_difference <- summary[design == design_a, estimate] -
    summary[design == design_b, estimate]
  bootstrap_difference <- boot_wide[[design_a]] - boot_wide[[design_b]]
  paired_se <- sd(bootstrap_difference)
  data.table(
    design_a = design_a,
    design_b = design_b,
    estimate_difference = point_difference,
    paired_bootstrap_se = paired_se,
    conf_low_95 = point_difference - qnorm(0.975) * paired_se,
    conf_high_95 = point_difference + qnorm(0.975) * paired_se,
    p_value_normal = 2 * pnorm(-abs(point_difference / paired_se)),
    bootstrap_correlation = cor(boot_wide[[design_a]], boot_wide[[design_b]]),
    bootstrap_repetitions = length(bootstrap_difference)
  )
}))

fwrite(summary, file.path(out_dir, "spatial_measurement_comparison.csv"))
fwrite(paired, file.path(out_dir, "spatial_measurement_paired_differences.csv"))

message("Spatial outcome comparison:")
print(summary[, .(
  design, estimate, std_error_bootstrap, conf_low_95, conf_high_95,
  p_value_normal, mean_monthly_reference, standard_error_ratio_to_network
)])
message("Paired differences:")
print(paired)
