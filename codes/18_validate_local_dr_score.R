#!/usr/bin/env Rscript

# Monte Carlo validation of the local-stack AIPW score used in script 13.
# The experiment isolates the score's double-robustness property. Treatment is
# selected by a correctly specified logit propensity, untreated changes follow
# a nonlinear conditional mean, and the ATT is known. We evaluate four cases:
# both nuisance functions correct, outcome regression only correct, propensity
# only correct, and both functions misspecified.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
out_dir <- file.path(root, "results", "current")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

monte_carlo_repetitions <- as.integer(Sys.getenv("DR_SIM_REPETITIONS", "1000"))
sample_size <- as.integer(Sys.getenv("DR_SIM_SAMPLE_SIZE", "2000"))
true_att <- -0.20

estimate_att <- function(d, y, p_hat, m_hat) {
  treated_residual <- y[d == 1L] - m_hat[d == 1L]
  control_residual <- y[d == 0L] - m_hat[d == 0L]
  control_odds <- p_hat[d == 0L] / (1 - p_hat[d == 0L])
  mean(treated_residual) - weighted.mean(control_residual, control_odds)
}

set.seed(20261013L)
draws <- vector("list", monte_carlo_repetitions)
for (b in seq_len(monte_carlo_repetitions)) {
  x <- rnorm(sample_size)
  p_true <- plogis(-0.6 + 0.8 * x)
  d <- rbinom(sample_size, 1L, p_true)
  m_true <- 0.2 + 0.7 * x + 0.5 * x^2
  y <- m_true + true_att * d + rnorm(sample_size)
  p_wrong <- rep(mean(d), sample_size)
  m_wrong <- rep(mean(y[d == 0L]), sample_size)
  draws[[b]] <- data.table(
    repetition = b,
    both_correct = estimate_att(d, y, p_true, m_true),
    outcome_only = estimate_att(d, y, p_wrong, m_true),
    propensity_only = estimate_att(d, y, p_true, m_wrong),
    both_wrong = estimate_att(d, y, p_wrong, m_wrong)
  )
}

draws <- rbindlist(draws)
long <- melt(draws, id.vars = "repetition", variable.name = "specification",
             value.name = "estimate")
summary <- long[, .(
  true_att = true_att,
  mean_estimate = mean(estimate),
  bias = mean(estimate) - true_att,
  monte_carlo_sd = sd(estimate),
  rmse = sqrt(mean((estimate - true_att)^2)),
  repetitions = .N
), by = specification]

fwrite(summary, file.path(out_dir, "simulation_local_dr_score.csv"))
print(summary)

validated <- summary[
  specification %chin% c("both_correct", "outcome_only", "propensity_only"),
  all(abs(bias) < 0.02)
]
if (!validated) {
  stop("The local-stack DR score failed its Monte Carlo bias tolerance.")
}
message("Local-stack DR score validated under either correct nuisance function.")
