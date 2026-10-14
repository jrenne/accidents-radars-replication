#!/usr/bin/env Rscript

# Assemble the two placebo diagnostics used in the paper:
# 1. pre-treatment event-time coefficients on the baseline risk set;
# 2. the first-year off-road outcome inside the 2-km circle but outside the
#    monitored-road corridor.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")

event_file <- file.path(
  out_dir,
  "event_study_monthly_road_corridor_dr_network_100m_balanced12_reach75.csv"
)
pretrend_file <- file.path(
  out_dir,
  "pretrend_tests_monthly_road_corridor_dr_network_100m_balanced12_reach75.csv"
)
offroad_file <- file.path(
  out_dir,
  "estimates_monthly_road_corridor_dr_offroad_100m_balanced12_reach75.csv"
)
paired_file <- file.path(out_dir, "spatial_measurement_paired_differences.csv")
needed <- c(event_file, pretrend_file, offroad_file, paired_file)
if (any(!file.exists(needed))) stop("Missing placebo input(s): ", paste(needed[!file.exists(needed)], collapse = ", "))

leads <- fread(event_file)[
  outcome == "accidents" & radius_km == 2 &
    window %chin% c("-24:-19", "-18:-13", "-12:-7")
]
leads[, `:=`(
  placebo_type = "Temporal lead",
  placebo_label = window,
  joint_pretrend_p_value = fread(pretrend_file)[
    outcome == "accidents" & radius_km == 2, pretrend_p_value
  ],
  paired_network_difference = NA_real_,
  paired_difference_p_value = NA_real_
)]

offroad <- fread(offroad_file)[
  outcome == "accidents" & radius_km == 2 & window == "months_1_12"
]
paired <- fread(paired_file)[design_a == "road_network" & design_b == "offroad"]
offroad[, `:=`(
  placebo_type = "Spatial negative-control outcome",
  placebo_label = "Inside circle, outside monitored-road corridor",
  joint_pretrend_p_value = NA_real_,
  paired_network_difference = paired$estimate_difference,
  paired_difference_p_value = paired$p_value_normal
)]

keep <- c(
  "placebo_type", "placebo_label", "treated_radars", "estimate",
  "std_error_bootstrap", "conf_low_95", "conf_high_95", "p_value_normal",
  "conf_low_simultaneous_95", "conf_high_simultaneous_95",
  "p_value_simultaneous", "joint_pretrend_p_value",
  "paired_network_difference", "paired_difference_p_value"
)
summary <- rbindlist(list(leads, offroad), fill = TRUE)[, ..keep]
fwrite(summary, file.path(out_dir, "baseline_placebo_summary.csv"))
print(summary)
