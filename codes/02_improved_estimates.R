#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(sandwich)
})

root <- normalizePath(".")
panel_file <- file.path(root, "results", "current", "panel_2013_2021.csv.gz")
out_dir <- file.path(root, "results", "current")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(panel_file)) {
  stop("Panel absent. Exécuter d'abord codes/01_reproduce_master.R depuis la racine du projet.")
}

panel <- fread(panel_file)
panel[, id_radar := factor(id_radar)]
panel[, event_time := annee - installation_year]

# L'année d'installation est déjà exclue du panel car elle ne correspond ni à
# une année entièrement non traitée, ni à une année entièrement traitée.
stopifnot(!any(panel$event_time == 0L))

cluster_tidy <- function(model, cluster, terms, outcome, specification, radius) {
  vc <- vcovCL(model, cluster = cluster, type = "HC1")
  cf <- coef(model)
  available <- intersect(terms, names(cf))
  data.table(
    outcome = outcome,
    specification = specification,
    radius_km = radius,
    term = available,
    estimate = unname(cf[available]),
    std_error_cluster = sqrt(diag(vc)[available]),
    p_value_cluster = 2 * pt(
      -abs(unname(cf[available]) / sqrt(diag(vc)[available])),
      df = length(unique(cluster)) - 1L
    ),
    n = nobs(model),
    clusters = length(unique(cluster))
  )
}

radii <- sort(unique(panel$radius_km))
outcomes <- c("accidents", "killed")

message("Effets fixes année + radar avec erreurs-types groupées...")
twfe <- rbindlist(lapply(radii, function(radius) {
  x <- panel[radius_km == radius]
  rbindlist(lapply(outcomes, function(y) {
    model <- lm(
      reformulate(c("installed", "factor(annee)", "id_radar"), response = y),
      data = x
    )
    ans <- cluster_tidy(
      model, x$id_radar, "installed", y, "year_radar_FE_clustered", radius
    )
    ans
  }))
}))
fwrite(twfe, file.path(out_dir, "estimates_clustered_twfe.csv"))

# Regroupement des extrémités pour éviter des coefficients reposant sur très
# peu de cohortes. La catégorie -1 est omise et sert de référence.
panel[, `:=`(
  event_le4 = as.integer(event_time <= -4L),
  event_le3 = as.integer(event_time == -3L),
  event_le2 = as.integer(event_time == -2L),
  event_ge1 = as.integer(event_time == 1L),
  event_ge2 = as.integer(event_time == 2L),
  event_ge3 = as.integer(event_time == 3L),
  event_ge4 = as.integer(event_time >= 4L)
)]
event_terms <- c(
  "event_le4", "event_le3", "event_le2",
  "event_ge1", "event_ge2", "event_ge3", "event_ge4"
)
event_labels <- c(
  event_le4 = "<= -4", event_le3 = "-3", event_le2 = "-2",
  event_ge1 = "+1", event_ge2 = "+2", event_ge3 = "+3", event_ge4 = ">= +4"
)

message("Études d'événement avec effets fixes et groupement par radar...")
event_results <- list()
diagnostics <- list()
idx <- 1L
for (radius in radii) {
  x <- panel[radius_km == radius]
  for (y in outcomes) {
    model <- lm(
      reformulate(c(event_terms, "factor(annee)", "id_radar"), response = y),
      data = x
    )
    tab <- cluster_tidy(
      model, x$id_radar, event_terms, y, "event_study_TWFE_clustered", radius
    )
    tab[, event := unname(event_labels[term])]
    setcolorder(tab, c(
      "outcome", "specification", "radius_km", "event", "term",
      "estimate", "std_error_cluster", "p_value_cluster", "n", "clusters"
    ))
    event_results[[idx]] <- tab

    vc <- vcovCL(model, cluster = x$id_radar, type = "HC1")
    lead_terms <- intersect(c("event_le4", "event_le3", "event_le2"), names(coef(model)))
    b <- coef(model)[lead_terms]
    v <- vc[lead_terms, lead_terms, drop = FALSE]
    stat <- as.numeric(t(b) %*% solve(v, b))
    diagnostics[[idx]] <- data.table(
      outcome = y,
      radius_km = radius,
      pretrend_terms = paste(lead_terms, collapse = ";"),
      pretrend_wald_chisq = stat,
      pretrend_df = length(lead_terms),
      pretrend_p_value = pchisq(stat, df = length(lead_terms), lower.tail = FALSE),
      reference_event = "-1",
      warning = "TWFE dynamique diagnostique; peut être biaisé si les effets diffèrent selon les cohortes"
    )
    idx <- idx + 1L
  }
}

event_results <- rbindlist(event_results)
diagnostics <- rbindlist(diagnostics)
fwrite(event_results, file.path(out_dir, "event_study_twfe.csv"))
fwrite(diagnostics, file.path(out_dir, "event_study_diagnostics.csv"))

# Estimateur de différences-de-différences par cohorte. Pour chaque cohorte g,
# la variation entre g-1 et g+e est comparée uniquement aux radars installés
# après la date évaluée. Il n'emploie donc jamais une cohorte déjà traitée comme
# témoin. L'inférence rééchantillonne les radars entiers.
make_did_cells <- function(x, outcome, event) {
  cells <- list()
  j <- 1L
  for (g in sort(unique(x$installation_year))) {
    reference_year <- g - 1L
    evaluation_year <- g + event
    cutoff <- if (event > 0L) evaluation_year else g
    control_ids <- unique(x[installation_year > cutoff, as.character(id_radar)])
    treated_ids <- unique(x[installation_year == g, as.character(id_radar)])
    if (!length(control_ids) || !length(treated_ids)) next

    changes <- merge(
      x[annee == reference_year, .(id = as.character(id_radar), y_ref = get(outcome))],
      x[annee == evaluation_year, .(id = as.character(id_radar), y_eval = get(outcome))],
      by = "id"
    )
    changes[, change := y_eval - y_ref]
    treated <- changes[id %in% treated_ids]
    controls <- changes[id %in% control_ids]
    if (!nrow(treated) || !nrow(controls)) next
    cells[[j]] <- list(g = g, treated = treated, controls = controls)
    j <- j + 1L
  }
  cells
}

aggregate_cells <- function(cells, bootstrap_weights = NULL, id_index = NULL) {
  numerator <- 0
  denominator <- 0
  for (cell in cells) {
    if (is.null(bootstrap_weights)) {
      nt <- nrow(cell$treated)
      att <- mean(cell$treated$change) - mean(cell$controls$change)
    } else {
      wt <- bootstrap_weights[id_index[cell$treated$id]]
      wc <- bootstrap_weights[id_index[cell$controls$id]]
      nt <- sum(wt)
      nc <- sum(wc)
      if (nt == 0 || nc == 0) next
      att <- weighted.mean(cell$treated$change, wt) -
        weighted.mean(cell$controls$change, wc)
    }
    numerator <- numerator + nt * att
    denominator <- denominator + nt
  }
  if (denominator == 0) return(NA_real_)
  numerator / denominator
}

message("Différences-de-différences avec témoins pas encore traités...")
set.seed(20261011)
bootstrap_repetitions <- 499L
not_yet_treated <- list()
j <- 1L
for (radius in radii) {
  x <- panel[radius_km == radius]
  all_ids <- unique(as.character(x$id_radar))
  id_index <- setNames(seq_along(all_ids), all_ids)
  for (y in outcomes) {
    for (event in c(-3L, -2L, 1L, 2L, 3L)) {
      cells <- make_did_cells(x, y, event)
      if (!length(cells)) next
      estimate <- aggregate_cells(cells)
      boot <- replicate(bootstrap_repetitions, {
        weights <- tabulate(
          sample.int(length(all_ids), length(all_ids), replace = TRUE),
          nbins = length(all_ids)
        )
        aggregate_cells(cells, weights, id_index)
      })
      boot <- boot[is.finite(boot)]
      se <- sd(boot)
      not_yet_treated[[j]] <- data.table(
        outcome = y,
        radius_km = radius,
        event = event,
        estimate = estimate,
        std_error_bootstrap = se,
        conf_low_95 = estimate - qnorm(0.975) * se,
        conf_high_95 = estimate + qnorm(0.975) * se,
        bootstrap_percentile_low_95 = unname(quantile(boot, 0.025)),
        bootstrap_percentile_high_95 = unname(quantile(boot, 0.975)),
        p_value_normal = 2 * pnorm(-abs(estimate / se)),
        contributing_cohorts = paste(vapply(cells, `[[`, integer(1), "g"), collapse = ";"),
        cohort_cells = length(cells),
        bootstrap_repetitions = length(boot),
        comparison = "cohorte traitee contre radars pas encore installes; reference g-1"
      )
      j <- j + 1L
    }
  }
}
not_yet_treated <- rbindlist(not_yet_treated)
fwrite(not_yet_treated, file.path(out_dir, "event_study_not_yet_treated.csv"))

message("Estimations améliorées terminées.")
print(twfe)
print(diagnostics)
print(not_yet_treated)
