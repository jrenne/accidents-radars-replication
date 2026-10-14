#!/usr/bin/env Rscript

# Compare the preferred 2-km, 100-m first-year estimate when camera sites
# within 10 km of municipalities of at least 100,000 inhabitants are excluded
# (baseline) or retained. Each row comes from a separately constructed set of
# road-network corridors and a full nuisance-model/bootstrap reestimation.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")

variants <- data.table(
  large_city_rule = c("Exclude large-city environments", "Include large-city environments"),
  suffix = c("", "_with_large_cities")
)

results <- rbindlist(lapply(seq_len(nrow(variants)), function(i) {
  suffix <- variants$suffix[i]
  specification_suffix <- if (nzchar(suffix)) {
    paste0("_network_100m", suffix, "_balanced12_reach75")
  } else {
    "_network_100m_balanced12_reach75"
  }
  estimate_file <- file.path(
    out_dir,
    paste0("estimates_monthly_road_corridor_dr", specification_suffix, ".csv")
  )
  pretrend_file <- file.path(
    out_dir,
    paste0("pretrend_tests_monthly_road_corridor_dr", specification_suffix, ".csv")
  )
  if (!file.exists(estimate_file)) stop("Missing estimate input: ", estimate_file)
  if (!file.exists(pretrend_file)) stop("Missing pretrend input: ", pretrend_file)
  x <- fread(estimate_file)[
    outcome == "accidents" & radius_km == 2 & window == "months_1_12"
  ]
  p <- fread(pretrend_file)[outcome == "accidents" & radius_km == 2]
  if (nrow(x) != 1L || nrow(p) != 1L) stop("Unexpected row count for ", suffix)
  x[, `:=`(
    large_city_rule = variants$large_city_rule[i],
    pretrend_p_value = p$pretrend_p_value
  )]
  x
}), fill = TRUE)

keep <- c(
  "large_city_rule", "treated_radars", "estimate", "std_error_bootstrap",
  "conf_low_95", "conf_high_95", "p_value_normal",
  "mean_monthly_reference", "effect_percent_of_reference",
  "pretrend_p_value"
)
fwrite(results[, ..keep], file.path(out_dir, "baseline_large_city_sensitivity.csv"))
print(results[, ..keep])
