#!/usr/bin/env Rscript

# Compare the preferred monthly crash count with an indicator for at least one
# injury crash in the site-month. The binary outcome checks whether the count
# result is driven by a small number of site-months containing multiple crashes.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")
baseline_file <- file.path(
  out_dir,
  "estimates_monthly_road_corridor_dr_network_100m_balanced12_reach75.csv"
)
binary_file <- file.path(
  out_dir,
  paste0(
    "estimates_monthly_road_corridor_dr_network_100m_balanced12_reach75",
    "_outcome_binary.csv"
  )
)

if (!file.exists(baseline_file) || !file.exists(binary_file)) {
  stop("Missing count or binary estimation output. Run script 13 first.")
}

extract_first_year <- function(path, scale, description) {
  x <- fread(path)
  x <- x[
    outcome == "accidents" & radius_km == 2 & window == "months_1_12"
  ]
  if (nrow(x) != 1L) stop("Expected one first-year row in ", path)
  x[, .(
    outcome_scale = scale,
    outcome_description = description,
    estimate,
    std_error_bootstrap,
    conf_low_95,
    conf_high_95,
    p_value_normal,
    mean_monthly_reference,
    effect_percent_of_reference,
    treated_radars,
    bootstrap_repetitions
  )]
}

summary <- rbindlist(list(
  extract_first_year(
    baseline_file, "count", "Number of injury crashes in the site-month"
  ),
  extract_first_year(
    binary_file, "binary", "At least one injury crash in the site-month"
  )
))

fwrite(summary, file.path(out_dir, "baseline_binary_outcome_sensitivity.csv"))
print(summary)
