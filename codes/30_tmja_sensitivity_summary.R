#!/usr/bin/env Rscript

# Assemble the preferred estimate and geometry-based TMJA sensitivities.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")
stem <- "monthly_road_corridor_network_100m_balanced12_reach75"

extract_row <- function(label, suffix = "") {
  estimates <- fread(file.path(out_dir, paste0("estimates_", stem, suffix, ".csv")))[
    outcome == "accidents" & radius_km == 2 & window == "months_1_12"
  ]
  pretrend <- fread(file.path(out_dir, paste0("pretrend_tests_", stem, suffix, ".csv")))[
    outcome == "accidents" & radius_km == 2
  ]
  diagnostics <- fread(file.path(out_dir, paste0("diagnostics_", stem, suffix, ".csv")))[
    outcome == "accidents" & radius_km == 2 & window == "months_1_12"
  ]
  if (nrow(estimates) != 1L || nrow(pretrend) != 1L || nrow(diagnostics) != 1L) {
    stop("Unexpected number of rows for suffix: ", suffix)
  }
  data.table(
    specification = label,
    traffic_match_distance_m = estimates$traffic_match_distance_m,
    estimate = estimates$estimate,
    std_error_bootstrap = estimates$std_error_bootstrap,
    conf_low_95 = estimates$conf_low_95,
    conf_high_95 = estimates$conf_high_95,
    p_value_normal = estimates$p_value_normal,
    mean_monthly_reference = estimates$mean_monthly_reference,
    effect_percent_of_reference = estimates$effect_percent_of_reference,
    treated_radars = estimates$treated_radars,
    unique_control_sites = diagnostics$unique_control_sites,
    median_raw_controls = diagnostics$median_raw_controls,
    plain_estimate_same_stacks = diagnostics$plain_estimate_same_stacks,
    pretrend_p_value = pretrend$pretrend_p_value
  )
}

summary <- rbindlist(list(
  extract_row("Preferred baseline"),
  extract_row("TMJA observed within 50 m", "_traffic50m"),
  extract_row("TMJA observed within 100 m", "_traffic100m")
), fill = TRUE)

fwrite(summary, file.path(out_dir, "baseline_tmja_sensitivity.csv"))
print(summary)
