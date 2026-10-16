#!/usr/bin/env Rscript

# Multiplicity adjustment for the pre-declared primary family:
# injury crashes, months 1--12, network corridors, radii 1/2/3 km, and
# half-widths 50/100 m. Holm controls the family-wise error rate under
# arbitrary dependence. Bonferroni p-values and simultaneous intervals are
# reported as a transparent conservative benchmark.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")

inputs <- data.table(
  corridor_half_width_m = c(50L, 100L),
  file = c(
    "estimates_monthly_road_corridor_network_balanced12_reach75.csv",
    "estimates_monthly_road_corridor_network_100m_balanced12_reach75.csv"
  )
)

family <- rbindlist(lapply(seq_len(nrow(inputs)), function(i) {
  path <- file.path(out_dir, inputs$file[i])
  if (!file.exists(path)) stop("Missing primary-family input: ", path)
  x <- fread(path)
  x <- x[
    outcome == "accidents" & window == "months_1_12" &
      radius_km %in% c(1, 2, 3)
  ]
  x[, corridor_half_width_m := inputs$corridor_half_width_m[i]]
  x
}), fill = TRUE)

setorder(family, radius_km, corridor_half_width_m)
if (nrow(family) != 6L) {
  stop("The primary family must contain exactly six estimates; found ",
       nrow(family), ".")
}

alpha <- 0.05
m <- nrow(family)
critical_bonferroni <- qnorm(1 - alpha / (2 * m))

family[, `:=`(
  family = "Injury crashes, months 1--12, network corridors",
  p_value_holm = p.adjust(p_value_normal, method = "holm"),
  p_value_bonferroni = p.adjust(p_value_normal, method = "bonferroni"),
  conf_low_bonferroni_95 = estimate -
    critical_bonferroni * std_error_bootstrap,
  conf_high_bonferroni_95 = estimate +
    critical_bonferroni * std_error_bootstrap
)]
family[, `:=`(
  reject_holm_5pct = p_value_holm < alpha,
  reject_bonferroni_5pct = p_value_bonferroni < alpha,
  bonferroni_critical_95 = critical_bonferroni,
  number_of_tests = m
)]

keep <- c(
  "family", "radius_km", "corridor_half_width_m", "treated_radars",
  "estimate", "std_error_bootstrap", "conf_low_95", "conf_high_95",
  "p_value_normal", "p_value_holm", "p_value_bonferroni",
  "conf_low_bonferroni_95", "conf_high_bonferroni_95",
  "reject_holm_5pct", "reject_bonferroni_5pct",
  "bonferroni_critical_95", "number_of_tests"
)
fwrite(
  family[, ..keep],
  file.path(out_dir, "primary_family_multiplicity.csv")
)

print(family[, .(
  radius_km, corridor_half_width_m, estimate, std_error_bootstrap,
  p_value_normal, p_value_holm, p_value_bonferroni,
  conf_low_bonferroni_95, conf_high_bonferroni_95,
  reject_holm_5pct
)])
