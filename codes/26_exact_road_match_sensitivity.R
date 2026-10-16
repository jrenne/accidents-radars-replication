#!/usr/bin/env Rscript

# Compare the preferred first-year estimate with the subset for which the
# normalized historical road number exactly matches the BD TOPO road number.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")

variants <- data.table(
  road_match_rule = c("All matched numbered roads", "Exact historical road number"),
  suffix = c("", "_road_exact")
)

results <- rbindlist(lapply(seq_len(nrow(variants)), function(i) {
  suffix <- variants$suffix[i]
  stem <- paste0("_network_100m_balanced12_reach75", suffix)
  estimate_file <- file.path(
    out_dir, paste0("estimates_monthly_road_corridor", stem, ".csv")
  )
  pretrend_file <- file.path(
    out_dir, paste0("pretrend_tests_monthly_road_corridor", stem, ".csv")
  )
  if (!file.exists(estimate_file)) stop("Missing estimate input: ", estimate_file)
  if (!file.exists(pretrend_file)) stop("Missing pretrend input: ", pretrend_file)
  x <- fread(estimate_file)[
    outcome == "accidents" & radius_km == 2 & window == "months_1_12"
  ]
  p <- fread(pretrend_file)[outcome == "accidents" & radius_km == 2]
  if (nrow(x) != 1L || nrow(p) != 1L) stop("Unexpected row count for ", suffix)
  x[, `:=`(
    road_match_rule = variants$road_match_rule[i],
    pretrend_p_value = p$pretrend_p_value
  )]
  x
}), fill = TRUE)

keep <- c(
  "road_match_rule", "treated_radars", "estimate", "std_error_bootstrap",
  "conf_low_95", "conf_high_95", "p_value_normal",
  "mean_monthly_reference", "effect_percent_of_reference",
  "pretrend_p_value"
)
fwrite(results[, ..keep], file.path(out_dir, "baseline_exact_road_match_sensitivity.csv"))
print(results[, ..keep])
