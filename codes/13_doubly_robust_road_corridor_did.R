#!/usr/bin/env Rscript

# Stacked road-corridor difference-in-differences.
#
# Each treated camera defines a local stack anchored at its installation month.
# Eligible controls are future-treated camera locations in the same road-class
# and speed-limit stratum. The default estimator gives those controls equal
# weight. Setting PRIMARY_ESTIMATOR=doubly_robust adds nuisance models fitted
# on the pooled stacks with site-level cross-fitting:
#   1. a propensity model for being the treated site rather than a future site;
#   2. an outcome model for the untreated change in monthly crashes.
# The augmented inverse-probability score is then averaged over treated stacks.
# Site-cluster bootstrap weights are applied to both treated and control roles.
# Cross-fitting makes the fixed-nuisance bootstrap first-order valid under the
# usual orthogonality conditions: every site's outcomes are predicted by models
# estimated without any row in which that site appears.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
processed <- file.path(root, "data", "processed")
out_dir <- file.path(root, "results", "current")

parse_numeric_list <- function(x) {
  as.numeric(strsplit(x, ",", fixed = TRUE)[[1]])
}

radii_km <- parse_numeric_list(Sys.getenv("DR_RADII", "1,2,3"))
corridor_half_width_m <- as.numeric(Sys.getenv("CORRIDOR_HALF_WIDTH_M", "100"))
spatial_method <- match.arg(
  Sys.getenv("CORRIDOR_METHOD", "network"),
  c("network", "euclidean", "circle", "offroad")
)
bootstrap_repetitions <- as.integer(Sys.getenv("DR_BOOTSTRAP_REPETITIONS", "1999"))
primary_estimator <- match.arg(
  Sys.getenv("PRIMARY_ESTIMATOR", "plain"),
  c("plain", "doubly_robust")
)
crossfit_folds <- as.integer(Sys.getenv("DR_CROSSFIT_FOLDS", "5"))
nuisance_method <- match.arg(
  Sys.getenv("DR_NUISANCE_METHOD", "ridge"), c("ridge", "glm")
)
if (primary_estimator == "doubly_robust" && nuisance_method == "ridge" &&
    !requireNamespace("glmnet", quietly = TRUE)) {
  stop("DR_NUISANCE_METHOD=ridge requires the R package 'glmnet'.")
}
if (!is.finite(crossfit_folds) || crossfit_folds < 2L) {
  stop("DR_CROSSFIT_FOLDS must be an integer of at least 2.")
}
if (!is.finite(bootstrap_repetitions) || bootstrap_repetitions < 2L) {
  stop("DR_BOOTSTRAP_REPETITIONS must be an integer of at least 2.")
}
common_radius_sample <- identical(Sys.getenv("DR_COMMON_RADIUS_SAMPLE", "0"), "1")
balanced_event_horizon <- as.integer(Sys.getenv("DR_BALANCED_EVENT_HORIZON", "0"))
minimum_network_reach_ratio <- as.numeric(Sys.getenv("DR_MIN_NETWORK_REACH_RATIO", "0"))
exclude_large_cities <- identical(Sys.getenv("DR_EXCLUDE_LARGE_CITIES", "1"), "1")
date_confidence_filter <- match.arg(
  Sys.getenv("DR_DATE_CONFIDENCE", "all"),
  c("all", "high", "high_medium")
)
road_match_filter <- match.arg(
  Sys.getenv("DR_ROAD_MATCH", "all"),
  c("all", "exact")
)
outcome_scale <- match.arg(
  Sys.getenv("DR_OUTCOME_SCALE", "count"),
  c("count", "binary")
)
traffic_adjustment <- identical(Sys.getenv("DR_TRAFFIC_ADJUSTMENT", "0"), "1")
traffic_match_distance_m <- as.numeric(Sys.getenv("DR_TRAFFIC_MATCH_DISTANCE_M", "50"))
minimum_controls <- 10L
large_city_population_thousands <- 100
large_city_buffer_km <- 10
propensity_trim <- c(0.02, 0.98)
width_suffix <- if (corridor_half_width_m == 50) "" else
  paste0("_", as.integer(corridor_half_width_m), "m")
method_suffix <- switch(
  spatial_method,
  network = "_network",
  euclidean = "",
  circle = "_circle",
  offroad = "_offroad"
)
sample_suffix <- if (common_radius_sample) "_common_sample" else ""
balance_suffix <- if (balanced_event_horizon > 0L) {
  paste0("_balanced", balanced_event_horizon)
} else ""
reach_suffix <- if (minimum_network_reach_ratio > 0) {
  paste0("_reach", round(100 * minimum_network_reach_ratio))
} else ""
date_suffix <- switch(
  date_confidence_filter,
  all = "",
  high = "_date_high",
  high_medium = "_date_high_medium"
)
road_match_suffix <- if (road_match_filter == "exact") "_road_exact" else ""
outcome_suffix <- if (outcome_scale == "binary") "_outcome_binary" else ""
traffic_suffix <- if (traffic_adjustment) {
  paste0("_traffic", as.integer(traffic_match_distance_m), "m")
} else ""
nuisance_suffix <- if (nuisance_method == "ridge") "" else "_nuisance_glm"
city_suffix <- if (exclude_large_cities) "" else "_with_large_cities"
count_suffix <- paste0(method_suffix, width_suffix, city_suffix)
analysis_suffix <- paste0(
  count_suffix, sample_suffix, balance_suffix, reach_suffix, date_suffix,
  road_match_suffix, outcome_suffix, traffic_suffix,
  nuisance_suffix
)
output_path <- function(stem) file.path(out_dir, paste0(stem, analysis_suffix, ".csv"))
counts_file <- if (spatial_method == "circle") {
  file.path(out_dir, "circle_monthly_counts.csv.gz")
} else if (spatial_method == "offroad") {
  file.path(out_dir, "offroad_circle_monthly_counts_100m.csv.gz")
} else {
  file.path(out_dir, paste0("road_corridor_monthly_counts", count_suffix, ".csv.gz"))
}

if (!file.exists(counts_file)) {
  stop("Missing spatial counts: ", counts_file,
       ". Run the corresponding count-construction script first.")
}

haversine_km <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 +
    cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  6371.0088 * 2 * atan2(sqrt(a), sqrt(pmax(0, 1 - a)))
}

road_class <- function(route) {
  route <- toupper(gsub("[^A-Z0-9]", "", route))
  fifelse(grepl("^A[0-9]", route), "autoroute",
          fifelse(grepl("^N[0-9]", route), "nationale",
                  fifelse(grepl("^D[0-9]", route), "departementale", "autre")))
}

mean_window <- function(values, radar_indices, months, month_columns) {
  columns <- match(months, month_columns)
  if (anyNA(columns)) return(rep(NA_real_, length(radar_indices)))
  rowMeans(values[radar_indices, columns, drop = FALSE])
}

standardize <- function(x) {
  x <- as.numeric(x)
  centre <- mean(x, na.rm = TRUE)
  spread <- sd(x, na.rm = TRUE)
  if (!is.finite(spread) || spread < 1e-10) spread <- 1
  (x - centre) / spread
}

message("Reconstructing dated point-camera sites and road covariates...")
radars <- fread(file.path(processed, "radars.csv.gz"))[
  !is.na(latitude) & !is.na(longitude)
]
radars[, `:=`(
  date_2025 = as.IDate(date_mise_en_service),
  date_2018 = as.IDate(date_installation_2018),
  current_point = type_radar_code %chin% c("ETF", "ETD", "ETT", "ETU"),
  old_point = type_radar_libelle_2018 %chin% c("Radar fixe", "Radar discriminant")
)]
radars[, point_speed := current_point | (actif_liste_2025 == 0L & old_point)]
radars <- radars[point_speed == TRUE]
radars[, installation_date := as.IDate(NA)]
radars[current_point == TRUE, installation_date := date_2025]
radars[current_point == TRUE & old_point == TRUE & !is.na(date_2018),
       installation_date := fifelse(
         is.na(date_2025), date_2018, pmin(date_2025, date_2018)
       )]
radars[actif_liste_2025 == 0L & old_point == TRUE,
       installation_date := date_2018]
radars <- radars[!is.na(installation_date)]
date_audit_file <- file.path(out_dir, "radar_treatment_date_audit.csv")
if (!file.exists(date_audit_file)) {
  stop("Missing treatment-date audit. Run script 19 first: ", date_audit_file)
}
date_audit <- fread(date_audit_file)[, .(
  cle_radar, date_confidence, date_evidence, confidence_reason
)]
radars <- merge(radars, date_audit, by = "cle_radar", all.x = TRUE)
radars[, `:=`(
  installation_year = as.integer(format(installation_date, "%Y")),
  installation_month = as.integer(format(installation_date, "%Y")) * 12L +
    as.integer(format(installation_date, "%m")) - 1L
)]
# Keep every dated point-camera site when detecting prior or nearby exposure.
# The confidence filter applies only to treated and comparison candidates.
exposure_radars <- copy(radars)
if (date_confidence_filter == "high") {
  radars <- radars[date_confidence == "high"]
} else if (date_confidence_filter == "high_medium") {
  radars <- radars[date_confidence %chin% c("high", "medium")]
}

matches <- fread(file.path(out_dir, "radar_ign_road_matches.csv"))[
  match_status == "matched"
]
match_columns <- c(
  "cle_radar", "road_number", "distance_to_road_m", "match_method",
  "road_importance", "road_urban", "road_speed_mean"
)
radars <- merge(
  radars, matches[, ..match_columns], by = "cle_radar", all.x = TRUE
)

radars[, near_large_city := FALSE]
if (exclude_large_cities) {
  city_environment <- new.env(parent = emptyenv())
  load(file.path(root, "data", "auxiliary", "result.radar.commune.rda"), envir = city_environment)
  city_centres <- unique(as.data.table(city_environment$radars)[
    Population >= large_city_population_thousands &
      !is.na(lat.commune) & !is.na(long.commune),
    .(INSEE_COM, latitude = lat.commune, longitude = long.commune)
  ], by = "INSEE_COM")
  for (i in seq_len(nrow(radars))) {
    radars$near_large_city[i] <- any(haversine_km(
      radars$latitude[i], radars$longitude[i],
      city_centres$latitude, city_centres$longitude
    ) <= large_city_buffer_km)
  }
}

counts <- fread(counts_file)
usable_ids <- unique(counts$cle_radar)
if (minimum_network_reach_ratio > 0) {
  # The reach rule defines the common network-based risk set even when the
  # outcome is subsequently counted with a circle or Euclidean corridor.
  geometry_suffix <- paste0("_network", width_suffix, city_suffix)
  geometry_diagnostic_file <- file.path(
    out_dir, paste0("road_corridor_geometry_diagnostics", geometry_suffix, ".csv")
  )
  geometry_diagnostics <- fread(geometry_diagnostic_file)
  reach_columns <- c(
    "cle_radar", "reachable_length_1km_m", "reachable_length_2km_m",
    "reachable_length_3km_m"
  )
  radars <- merge(
    radars, geometry_diagnostics[, ..reach_columns], by = "cle_radar", all.x = TRUE
  )
}
base_candidates <- radars[
  installation_year %between% c(2012L, 2024L) &
    !is.na(road_number) &
    (!exclude_large_cities | near_large_city == FALSE) &
    cle_radar %chin% usable_ids
]
if (road_match_filter == "exact") {
  base_candidates <- base_candidates[match_method == "exact_route_number"]
}
base_candidates[, `:=`(
  route_class = road_class(road_number),
  vma_group = fifelse(is.na(vma_kmh), "vma_missing", paste0("vma_", vma_kmh)),
  matching_stratum = paste(
    road_class(road_number),
    fifelse(is.na(vma_kmh), "vma_missing", paste0("vma_", vma_kmh)),
    sep = "__"
  ),
  road_importance_missing = as.integer(is.na(road_importance)),
  road_speed_missing = as.integer(is.na(road_speed_mean)),
  road_urban_num = fifelse(is.na(road_urban), 0, as.integer(road_urban))
)]
base_candidates[, road_importance_num := as.numeric(road_importance)]
base_candidates[is.na(road_importance_num),
                road_importance_num := median(road_importance_num, na.rm = TRUE)]
base_candidates[is.na(road_speed_mean),
                road_speed_mean := median(road_speed_mean, na.rm = TRUE)]

traffic_matches <- NULL
if (traffic_adjustment) {
  traffic_file <- file.path(out_dir, "baseline_tmja_site_year_matches_geometry.csv")
  if (!file.exists(traffic_file)) {
    stop("Missing geometry-based TMJA matches. Run script 29b first: ", traffic_file)
  }
  traffic_matches <- fread(traffic_file)[
    is.finite(tmja) & tmja > 0 &
      is.finite(tmja_distance_m) & tmja_distance_m <= traffic_match_distance_m
  ]
}

event_windows <- list(
  `-24:-19` = c(-24L, -19L),
  `-18:-13` = c(-18L, -13L),
  `+1:+6` = c(1L, 6L),
  `+7:+12` = c(7L, 12L),
  `+13:+18` = c(13L, 18L),
  `+19:+24` = c(19L, 24L)
)
average_windows <- list(
  months_1_12 = c(1L, 12L),
  months_1_24 = c(1L, 24L)
)
all_months <- seq(2010L * 12L, 2024L * 12L + 11L)

nuisance_rhs_common <- paste(
  "road_importance_z + road_importance_missing + road_urban_num +",
  "road_speed_z + road_speed_missing +",
  "factor(anchor_year) + factor(anchor_month_of_year) + factor(matching_stratum)"
)
nuisance_rhs_post <- paste(
  "pre_24_13_z + pre_12_7_z + pre_6_1_z +",
  nuisance_rhs_common
)
if (traffic_adjustment) {
  nuisance_rhs_common <- paste("log_tmja_z +", nuisance_rhs_common)
  nuisance_rhs_post <- paste(
    "pre_24_13_z + pre_12_7_z + pre_6_1_z +",
    nuisance_rhs_common
  )
}

estimate_radius <- function(radius) {
  message("Radius ", radius, " km")
  design_radius <- if (common_radius_sample) max(radii_km) else radius
  candidates <- copy(base_candidates)
  if (minimum_network_reach_ratio > 0) {
    reach_column <- paste0("reachable_length_", as.integer(design_radius), "km_m")
    candidates <- candidates[
      is.finite(get(reach_column)) &
        get(reach_column) >= minimum_network_reach_ratio * 2 * design_radius * 1000
    ]
  }
  keep <- logical(nrow(candidates))
  next_nearby <- rep(Inf, nrow(candidates))
  for (i in seq_len(nrow(candidates))) {
    distance <- haversine_km(
      candidates$latitude[i], candidates$longitude[i],
      exposure_radars$latitude, exposure_radars$longitude
    )
    other <- exposure_radars$cle_radar != candidates$cle_radar[i]
    keep[i] <- !any(
      distance < design_radius & other &
        exposure_radars$installation_month <= candidates$installation_month[i],
      na.rm = TRUE
    )
    later <- exposure_radars$installation_month[
      distance < design_radius & other &
        exposure_radars$installation_month > candidates$installation_month[i]
    ]
    if (length(later)) next_nearby[i] <- min(later)
  }
  candidates[, next_nearby_installation_month := next_nearby]
  candidates <- candidates[keep]
  setorder(candidates, cle_radar)

  radius_counts <- counts[
    radius_km == radius & cle_radar %chin% candidates$cle_radar
  ]
  accident_matrix <- dcast(
    radius_counts, cle_radar ~ month_index, value.var = "accidents"
  )
  killed_matrix <- dcast(
    radius_counts, cle_radar ~ month_index, value.var = "killed"
  )
  common_ids <- intersect(accident_matrix$cle_radar, killed_matrix$cle_radar)
  candidates <- candidates[cle_radar %chin% common_ids]
  setorder(candidates, cle_radar)
  accident_matrix <- accident_matrix[match(candidates$cle_radar, cle_radar)]
  killed_matrix <- killed_matrix[match(candidates$cle_radar, cle_radar)]
  candidates[, `:=`(
    site_index = .I,
    eligible_treated = installation_year <= 2022L
  )]
  set.seed(20262800L + as.integer(radius * 100))
  candidates[, crossfit_fold := sample(rep(
    seq_len(crossfit_folds), length.out = .N
  ))]
  accident_values <- as.matrix(accident_matrix[, -"cle_radar"])
  killed_values <- as.matrix(killed_matrix[, -"cle_radar"])
  if (outcome_scale == "binary") {
    accident_values <- 1L * (accident_values > 0)
    killed_values <- 1L * (killed_values > 0)
  }
  month_columns <- as.integer(colnames(accident_values))
  set.seed(20262900L + as.integer(radius * 100))
  shared_boot_weights <- replicate(
    bootstrap_repetitions,
    tabulate(
      sample.int(nrow(candidates), nrow(candidates), replace = TRUE),
      nbins = nrow(candidates)
    )
  )

  build_stack_data <- function(values, event_start, event_end) {
    rows <- list()
    stacks <- list()
    row_cursor <- 1L
    stack_cursor <- 1L
    treated_indices <- candidates[eligible_treated == TRUE, site_index]
    for (treated_index in treated_indices) {
      anchor <- candidates$installation_month[treated_index]
      evaluation <- anchor + event_start:event_end
      reference <- anchor + (-12L:-1L)
      pre_24_13 <- anchor + (-24L:-13L)
      pre_12_7 <- anchor + (-12L:-7L)
      pre_6_1 <- anchor + (-6L:-1L)
      eligibility_end <- max(0L, event_end, balanced_event_horizon)
      needed <- c(
        evaluation, reference, pre_24_13, pre_12_7,
        anchor + eligibility_end
      )
      if (min(needed) < min(all_months) || max(needed) > max(all_months)) next
      if (eligibility_end > 0 &&
          candidates$next_nearby_installation_month[treated_index] <=
            anchor + eligibility_end) next

      controls <- candidates[
        installation_month > anchor + eligibility_end &
          matching_stratum == candidates$matching_stratum[treated_index],
        site_index
      ]
      if (length(controls)) {
        distance <- haversine_km(
          candidates$latitude[treated_index], candidates$longitude[treated_index],
          candidates$latitude[controls], candidates$longitude[controls]
        )
        controls <- controls[distance > 2 * design_radius]
      }
      anchor_year <- anchor %/% 12L
      traffic_values <- NULL
      if (traffic_adjustment) {
        traffic_at_anchor <- traffic_matches[traffic_year == anchor_year - 1L]
        traffic_values <- setNames(traffic_at_anchor$tmja, traffic_at_anchor$cle_radar)
        treated_id <- candidates$cle_radar[treated_index]
        if (!treated_id %chin% names(traffic_values)) next
        controls <- controls[
          candidates$cle_radar[controls] %chin% names(traffic_values)
        ]
      }
      if (length(controls) < minimum_controls) next

      site_indices <- c(treated_index, controls)
      reference_mean <- mean_window(
        values, site_indices, reference, month_columns
      )
      change <- mean_window(values, site_indices, evaluation, month_columns) -
        reference_mean
      pre_a <- mean_window(values, site_indices, pre_24_13, month_columns)
      pre_b <- mean_window(values, site_indices, pre_12_7, month_columns)
      pre_c <- mean_window(values, site_indices, pre_6_1, month_columns)
      if (any(!is.finite(c(change, pre_a, pre_b, pre_c)))) next

      n_controls <- length(controls)
      block <- candidates[site_indices, .(
        site_index, cle_radar, matching_stratum,
        road_importance_num, road_importance_missing,
        road_urban_num, road_speed_mean, road_speed_missing,
        crossfit_fold
      )]
      if (traffic_adjustment) {
        block[, tmja := as.numeric(traffic_values[cle_radar])]
      }
      block[, `:=`(
        stack_id = stack_cursor,
        treated = c(1L, rep(0L, n_controls)),
        change = change,
        pre_24_13 = pre_a,
        pre_12_7 = pre_b,
        pre_6_1 = pre_c,
        base_weight = c(1, rep(1 / n_controls, n_controls)),
        anchor_year = anchor_year,
        anchor_month_of_year = anchor %% 12L + 1L,
        treated_reference = reference_mean[1]
      )]
      rows[[row_cursor]] <- block
      stacks[[stack_cursor]] <- list(
        treated = treated_index,
        controls = controls,
        n_controls = n_controls
      )
      row_cursor <- row_cursor + 1L
      stack_cursor <- stack_cursor + 1L
    }
    list(data = rbindlist(rows, use.names = TRUE), stacks = stacks)
  }

  estimate_one <- function(values, event_start, event_end) {
    built <- build_stack_data(values, event_start, event_end)
    dat <- built$data
    stacks <- built$stacks
    if (!nrow(dat) || length(stacks) < 20L) {
      stop("Insufficient stacked comparisons at radius ", radius,
           " and window ", event_start, ":", event_end)
    }

    if (primary_estimator == "doubly_robust") {
      dat[, `:=`(
      pre_24_13_z = standardize(pre_24_13),
      pre_12_7_z = standardize(pre_12_7),
      pre_6_1_z = standardize(pre_6_1),
      road_importance_z = standardize(road_importance_num),
      road_speed_z = standardize(road_speed_mean)
    )]
    if (traffic_adjustment) {
      dat[, log_tmja_z := standardize(log(tmja))]
    }
    # A lead being tested must never also enter the nuisance model as a
    # predictor. Pre-treatment placebo windows therefore use road and calendar
    # covariates only; post-treatment estimates additionally use the three
    # pre-treatment outcome histories.
    nuisance_rhs <- if (event_end < 0L) nuisance_rhs_common else nuisance_rhs_post
    ps_formula <- as.formula(paste("treated ~", nuisance_rhs))
    outcome_formula <- as.formula(paste("change ~", nuisance_rhs))
    dat[, `:=`(propensity_raw = NA_real_, untreated_prediction = NA_real_)]
    if (nuisance_method == "ridge") {
      nuisance_matrix <- model.matrix(ps_formula, data = dat)
      nuisance_matrix <- nuisance_matrix[, colnames(nuisance_matrix) != "(Intercept)",
                                         drop = FALSE]
    }
    for (fold in seq_len(crossfit_folds)) {
      train_rows <- which(dat$crossfit_fold != fold)
      test_rows <- which(dat$crossfit_fold == fold)
      if (!length(test_rows)) next
      if (nuisance_method == "ridge") {
        inner_fold <- as.integer(factor(dat$crossfit_fold[train_rows]))
        ps_model <- glmnet::cv.glmnet(
          x = nuisance_matrix[train_rows, , drop = FALSE],
          y = dat$treated[train_rows],
          family = "binomial", alpha = 0,
          weights = dat$base_weight[train_rows], foldid = inner_fold,
          type.measure = "deviance"
        )
        control_train_rows <- train_rows[dat$treated[train_rows] == 0L]
        outcome_inner_fold <- as.integer(factor(
          dat$crossfit_fold[control_train_rows]
        ))
        outcome_model <- glmnet::cv.glmnet(
          x = nuisance_matrix[control_train_rows, , drop = FALSE],
          y = dat$change[control_train_rows],
          family = "gaussian", alpha = 0,
          weights = dat$base_weight[control_train_rows],
          foldid = outcome_inner_fold,
          type.measure = "mse"
        )
        dat$propensity_raw[test_rows] <- as.numeric(predict(
          ps_model, newx = nuisance_matrix[test_rows, , drop = FALSE],
          s = "lambda.1se", type = "response"
        ))
        dat$untreated_prediction[test_rows] <- as.numeric(predict(
          outcome_model, newx = nuisance_matrix[test_rows, , drop = FALSE],
          s = "lambda.1se", type = "response"
        ))
      } else {
        train <- dat[train_rows]
        ps_model <- glm(
          ps_formula, data = train, family = quasibinomial(), weights = base_weight
        )
        outcome_model <- lm(
          outcome_formula, data = train[treated == 0L], weights = base_weight
        )
        dat$propensity_raw[test_rows] <- as.numeric(predict(
          ps_model, newdata = dat[test_rows], type = "response"
        ))
        dat$untreated_prediction[test_rows] <- as.numeric(predict(
          outcome_model, newdata = dat[test_rows]
        ))
      }
    }
    if (any(!is.finite(dat$propensity_raw)) ||
        any(!is.finite(dat$untreated_prediction))) {
      stop("Cross-fitted nuisance prediction failed at radius ", radius,
           " and window ", event_start, ":", event_end)
    }
    dat[, propensity := pmax(
      propensity_trim[1], pmin(propensity_trim[2], propensity_raw)
    )]
      dat[, residual := change - untreated_prediction]
    } else {
      # The main specification is the transparent within-stack DiD. Equal
      # control weights correspond to a constant propensity score; no nuisance
      # model is estimated for this path.
      dat[, `:=`(
        propensity_raw = 0.5,
        propensity = 0.5,
        untreated_prediction = 0,
        residual = change
      )]
    }

    dat_by_stack <- split(dat, by = "stack_id", keep.by = FALSE)
    for (s in seq_along(stacks)) {
      block <- dat_by_stack[[s]]
      treated_row <- block[treated == 1L]
      control_rows <- block[treated == 0L]
      odds <- control_rows$propensity / (1 - control_rows$propensity)
      odds <- odds / sum(odds)
      stacks[[s]]$treated_residual <- treated_row$residual
      stacks[[s]]$control_residual <- control_rows$residual
      stacks[[s]]$treated_change <- treated_row$change
      stacks[[s]]$control_change <- control_rows$change
      stacks[[s]]$control_odds <- odds
      stacks[[s]]$treated_prediction <- treated_row$untreated_prediction
      stacks[[s]]$treated_year <- candidates$installation_year[
        stacks[[s]]$treated
      ]
    }

    local_scores <- data.table(
      stack_id = seq_along(stacks),
      treated_year = vapply(stacks, `[[`, integer(1), "treated_year"),
      score = vapply(stacks, function(stack) {
        if (primary_estimator == "plain") {
          stack$treated_change - mean(stack$control_change)
        } else {
          stack$treated_residual -
            weighted.mean(stack$control_residual, stack$control_odds)
        }
      }, numeric(1))
    )
    # Influence diagnostic: nuisance predictions and within-stack weights are
    # held at their full-sample cross-fitted values, then each installation
    # cohort is removed from the final equal-stack aggregation in turn.
    leave_one_cohort_out <- rbindlist(c(
      list(data.table(
        excluded_cohort = NA_integer_,
        estimate = mean(local_scores$score),
        treated_stacks = nrow(local_scores)
      )),
      lapply(sort(unique(local_scores$treated_year)), function(year) {
        retained <- local_scores[treated_year != year]
        data.table(
          excluded_cohort = year,
          estimate = mean(retained$score),
          treated_stacks = nrow(retained)
        )
      })
    ))

    # Balance is summarized at the local-stack level. Each treated site is
    # paired with the mean of its admissible controls, before and after the
    # propensity-odds weights. This respects the actual comparison design and
    # avoids treating repeated control appearances as independent records.
    balance_variables <- c(
      "pre_24_13", "pre_12_7", "pre_6_1", "road_importance_num",
      "road_urban_num", "road_speed_mean"
    )
    balance <- rbindlist(lapply(balance_variables, function(variable) {
      treated_value <- control_unweighted <- control_weighted <-
        numeric(length(stacks))
      for (s in seq_along(stacks)) {
        block <- dat_by_stack[[s]]
        treated_value[s] <- block[treated == 1L, get(variable)][1]
        control_value <- block[treated == 0L, get(variable)]
        control_unweighted[s] <- mean(control_value)
        control_weighted[s] <- weighted.mean(
          control_value, stacks[[s]]$control_odds
        )
      }
      pooled_sd_unweighted <- sqrt(
        (var(treated_value) + var(control_unweighted)) / 2
      )
      pooled_sd_weighted <- sqrt(
        (var(treated_value) + var(control_weighted)) / 2
      )
      data.table(
        variable = variable,
        treated_mean = mean(treated_value),
        control_mean_unweighted = mean(control_unweighted),
        control_mean_weighted = mean(control_weighted),
        mean_difference_unweighted = mean(treated_value - control_unweighted),
        mean_difference_weighted = mean(treated_value - control_weighted),
        standardized_difference_unweighted = fifelse(
          is.finite(pooled_sd_unweighted) & pooled_sd_unweighted > 0,
          mean(treated_value - control_unweighted) / pooled_sd_unweighted,
          NA_real_
        ),
        standardized_difference_weighted = fifelse(
          is.finite(pooled_sd_weighted) & pooled_sd_weighted > 0,
          mean(treated_value - control_weighted) / pooled_sd_weighted,
          NA_real_
        ),
        stacks = length(stacks)
      )
    }))

    aggregate_stacks <- function(site_weights = NULL, estimator = "dr") {
      if (is.null(site_weights)) site_weights <- rep(1, nrow(candidates))
      numerator <- denominator <- 0
      for (stack in stacks) {
        treated_weight <- site_weights[stack$treated]
        if (treated_weight <= 0) next
        control_weight <- site_weights[stack$controls]
        if (estimator %chin% c("dr", "ipw")) {
          control_weight <- control_weight * stack$control_odds
        }
        if (sum(control_weight) <= 0) next
        score <- switch(
          estimator,
          dr = stack$treated_residual -
            weighted.mean(stack$control_residual, control_weight),
          ipw = stack$treated_change -
            weighted.mean(stack$control_change, control_weight),
          plain = stack$treated_change -
            weighted.mean(stack$control_change, control_weight),
          or = stack$treated_change - stack$treated_prediction
        )
        numerator <- numerator + treated_weight * score
        denominator <- denominator + treated_weight
      }
      if (denominator == 0) NA_real_ else numerator / denominator
    }

    estimates <- c(
      plain = aggregate_stacks(estimator = "plain"),
      outcome_regression = aggregate_stacks(estimator = "or"),
      inverse_probability = aggregate_stacks(estimator = "ipw"),
      doubly_robust = aggregate_stacks(estimator = "dr")
    )
    estimator_code <- if (primary_estimator == "plain") "plain" else "dr"
    boot_primary <- apply(
      shared_boot_weights, 2, aggregate_stacks, estimator = estimator_code
    )
    boot_primary <- boot_primary[is.finite(boot_primary)]
    se <- sd(boot_primary)

    effective_controls <- vapply(stacks, function(x) {
      1 / sum(x$control_odds^2)
    }, numeric(1))
    treated_rows <- dat[treated == 1L]
    list(
      estimate = estimates[primary_estimator],
      se = se,
      boot = boot_primary,
      estimates = estimates,
      balance = balance,
      leave_one_cohort_out = leave_one_cohort_out,
      diagnostics = data.table(
        stacks = length(stacks),
        unique_treated_sites = uniqueN(treated_rows$site_index),
        unique_control_sites = uniqueN(dat[treated == 0L, site_index]),
        stacked_rows = nrow(dat),
        median_raw_controls = median(vapply(stacks, `[[`, integer(1), "n_controls")),
        median_effective_controls = median(effective_controls),
        propensity_min_raw = min(dat$propensity_raw),
        propensity_max_raw = max(dat$propensity_raw),
        propensity_share_trimmed = mean(
          dat$propensity_raw < propensity_trim[1] |
            dat$propensity_raw > propensity_trim[2]
        ),
        untreated_prediction_min = min(dat$untreated_prediction),
        untreated_prediction_p01 = unname(quantile(dat$untreated_prediction, 0.01)),
        untreated_prediction_p99 = unname(quantile(dat$untreated_prediction, 0.99)),
        untreated_prediction_max = max(dat$untreated_prediction),
        residual_min = min(dat$residual),
        residual_p01 = unname(quantile(dat$residual, 0.01)),
        residual_p99 = unname(quantile(dat$residual, 0.99)),
        residual_max = max(dat$residual),
        crossfit_folds = crossfit_folds,
        nuisance_method = nuisance_method,
        primary_estimator = primary_estimator,
        plain_estimate_same_stacks = estimates["plain"],
        outcome_regression_estimate = estimates["outcome_regression"],
        inverse_probability_estimate = estimates["inverse_probability"],
        doubly_robust_estimate = estimates["doubly_robust"],
        mean_monthly_reference = mean(treated_rows$treated_reference)
      )
    )
  }

  results <- list()
  diagnostics <- list()
  bootstrap_draws <- list()
  balance_diagnostics <- list()
  cohort_diagnostics <- list()
  pretrend_boot <- list(accidents = list(), killed = list())
  result_cursor <- diagnostic_cursor <- bootstrap_cursor <- balance_cursor <-
    cohort_cursor <- 1L
  all_windows <- c(event_windows, average_windows)
  window_types <- c(
    rep("event", length(event_windows)),
    rep("average", length(average_windows))
  )

  for (outcome in c("accidents", "killed")) {
    values <- if (outcome == "accidents") accident_values else killed_values
    for (j in seq_along(all_windows)) {
      label <- names(all_windows)[j]
      window <- all_windows[[j]]
      fit <- estimate_one(values, window[1], window[2])
      estimate <- as.numeric(fit$estimate)
      diag <- fit$diagnostics
      results[[result_cursor]] <- data.table(
        outcome = outcome,
        radius_km = radius,
        design_radius_km = design_radius,
        common_radius_sample = common_radius_sample,
        balanced_event_horizon = balanced_event_horizon,
        minimum_network_reach_ratio = minimum_network_reach_ratio,
        date_confidence_filter = date_confidence_filter,
        outcome_scale = outcome_scale,
        traffic_adjustment = traffic_adjustment,
        traffic_match_distance_m = if (traffic_adjustment) traffic_match_distance_m else NA_real_,
        corridor_half_width_m = corridor_half_width_m,
        spatial_method = spatial_method,
        estimator = primary_estimator,
        window_type = window_types[j],
        window = label,
        estimate = estimate,
        std_error_bootstrap = fit$se,
        conf_low_95 = estimate - qnorm(0.975) * fit$se,
        conf_high_95 = estimate + qnorm(0.975) * fit$se,
        conf_low_percentile_95 = unname(quantile(fit$boot, 0.025)),
        conf_high_percentile_95 = unname(quantile(fit$boot, 0.975)),
        p_value_normal = 2 * pnorm(-abs(estimate / fit$se)),
        mean_monthly_reference = diag$mean_monthly_reference,
        effect_percent_of_reference = 100 * estimate / diag$mean_monthly_reference,
        treated_radars = diag$unique_treated_sites,
        bootstrap_repetitions = length(fit$boot),
        crossfit_folds = crossfit_folds,
        nuisance_method = nuisance_method,
        propensity_trim_lower = propensity_trim[1],
        propensity_trim_upper = propensity_trim[2]
      )
      diagnostics[[diagnostic_cursor]] <- cbind(
        data.table(
          outcome = outcome, radius_km = radius,
          design_radius_km = design_radius,
          common_radius_sample = common_radius_sample,
          balanced_event_horizon = balanced_event_horizon,
          minimum_network_reach_ratio = minimum_network_reach_ratio,
          date_confidence_filter = date_confidence_filter,
          outcome_scale = outcome_scale,
          traffic_adjustment = traffic_adjustment,
          traffic_match_distance_m = if (traffic_adjustment) traffic_match_distance_m else NA_real_,
          corridor_half_width_m = corridor_half_width_m,
          spatial_method = spatial_method,
          window_type = window_types[j], window = label
        ),
        diag
      )
      bootstrap_draws[[bootstrap_cursor]] <- data.table(
        outcome = outcome,
        radius_km = radius,
        window_type = window_types[j],
        window = label,
        bootstrap_draw = seq_along(fit$boot),
        estimate = fit$boot
      )
      if (outcome == "accidents" && label == "months_1_12") {
        balance_diagnostics[[balance_cursor]] <- cbind(
          data.table(
            outcome = outcome, radius_km = radius,
            design_radius_km = design_radius,
            common_radius_sample = common_radius_sample,
            balanced_event_horizon = balanced_event_horizon,
            minimum_network_reach_ratio = minimum_network_reach_ratio,
            date_confidence_filter = date_confidence_filter,
            corridor_half_width_m = corridor_half_width_m,
            spatial_method = spatial_method,
            window = label
          ),
          fit$balance
        )
        balance_cursor <- balance_cursor + 1L
        cohort_diagnostics[[cohort_cursor]] <- cbind(
          data.table(
            outcome = outcome, radius_km = radius,
            design_radius_km = design_radius,
            common_radius_sample = common_radius_sample,
            balanced_event_horizon = balanced_event_horizon,
            minimum_network_reach_ratio = minimum_network_reach_ratio,
            date_confidence_filter = date_confidence_filter,
            corridor_half_width_m = corridor_half_width_m,
            spatial_method = spatial_method,
            window = label
          ),
          fit$leave_one_cohort_out
        )
        cohort_cursor <- cohort_cursor + 1L
      }
      if (label %chin% names(event_windows)[1:2]) {
        pretrend_boot[[outcome]][[label]] <- fit$boot
      }
      result_cursor <- result_cursor + 1L
      diagnostic_cursor <- diagnostic_cursor + 1L
      bootstrap_cursor <- bootstrap_cursor + 1L
    }
  }

  pretrend_tests <- list()
  for (outcome in c("accidents", "killed")) {
    pre_labels <- names(event_windows)[1:2]
    point <- vapply(pre_labels, function(label) {
      results[[which(vapply(results, function(x)
        x$outcome == outcome && x$window == label, logical(1)))]]$estimate
    }, numeric(1))
    common_boot <- min(vapply(pretrend_boot[[outcome]], length, integer(1)))
    boot_matrix <- do.call(rbind, lapply(pre_labels, function(label) {
      pretrend_boot[[outcome]][[label]][seq_len(common_boot)]
    }))
    covariance <- cov(t(boot_matrix))
    statistic <- tryCatch(
      as.numeric(t(point) %*% solve(covariance, point)),
      error = function(e) NA_real_
    )
    pretrend_tests[[outcome]] <- data.table(
      outcome = outcome,
      radius_km = radius,
      design_radius_km = design_radius,
      common_radius_sample = common_radius_sample,
      balanced_event_horizon = balanced_event_horizon,
      minimum_network_reach_ratio = minimum_network_reach_ratio,
      date_confidence_filter = date_confidence_filter,
      corridor_half_width_m = corridor_half_width_m,
      spatial_method = spatial_method,
      pretrend_wald_statistic = statistic,
      degrees_of_freedom = length(pre_labels),
      pretrend_p_value = pchisq(statistic, df = length(pre_labels), lower.tail = FALSE),
      bootstrap_repetitions = common_boot
    )
  }

  list(
    results = rbindlist(results, fill = TRUE),
    diagnostics = rbindlist(diagnostics, fill = TRUE),
    pretrends = rbindlist(pretrend_tests, fill = TRUE),
    bootstrap = rbindlist(bootstrap_draws, fill = TRUE),
    balance = rbindlist(balance_diagnostics, fill = TRUE),
    cohorts = rbindlist(cohort_diagnostics, fill = TRUE)
  )
}

message("Estimating stacked comparisons (primary estimator: ",
        primary_estimator, ")...")
radius_results <- lapply(radii_km, estimate_radius)
results <- rbindlist(lapply(radius_results, `[[`, "results"), fill = TRUE)
diagnostics <- rbindlist(lapply(radius_results, `[[`, "diagnostics"), fill = TRUE)
pretrends <- rbindlist(lapply(radius_results, `[[`, "pretrends"), fill = TRUE)
bootstrap_draws <- rbindlist(lapply(radius_results, `[[`, "bootstrap"), fill = TRUE)
balance_diagnostics <- rbindlist(
  lapply(radius_results, `[[`, "balance"), fill = TRUE
)
cohort_diagnostics <- rbindlist(
  lapply(radius_results, `[[`, "cohorts"), fill = TRUE
)

# Familywise 95% confidence bands across the seven displayed event-time bins.
# The same site-bootstrap draws are used in every bin, preserving dependence.
event_rows <- copy(results[window_type == "event"])
event_rows[, `:=`(
  conf_low_simultaneous_95 = NA_real_,
  conf_high_simultaneous_95 = NA_real_,
  p_value_simultaneous = NA_real_,
  simultaneous_critical_95 = NA_real_
)]
for (outcome_value in unique(event_rows$outcome)) {
  for (radius_value in unique(event_rows$radius_km)) {
    idx <- which(
      event_rows$outcome == outcome_value & event_rows$radius_km == radius_value
    )
    point <- event_rows$estimate[idx]
    standard_error <- event_rows$std_error_bootstrap[idx]
    labels <- event_rows$window[idx]
    boot <- bootstrap_draws[
      outcome == outcome_value & radius_km == radius_value &
        window_type == "event" & window %chin% labels
    ]
    boot_wide <- dcast(boot, bootstrap_draw ~ window, value.var = "estimate")
    setcolorder(boot_wide, c("bootstrap_draw", labels))
    deviations <- sweep(as.matrix(boot_wide[, -"bootstrap_draw"]), 2, point, "-")
    t_draws <- sweep(deviations, 2, standard_error, "/")
    max_t <- apply(abs(t_draws), 1, max, na.rm = TRUE)
    critical <- unname(quantile(max_t, 0.95, na.rm = TRUE))
    observed_t <- abs(point / standard_error)
    event_rows$conf_low_simultaneous_95[idx] <- point - critical * standard_error
    event_rows$conf_high_simultaneous_95[idx] <- point + critical * standard_error
    event_rows$p_value_simultaneous[idx] <- vapply(
      observed_t, function(x) mean(max_t >= x, na.rm = TRUE), numeric(1)
    )
    event_rows$simultaneous_critical_95[idx] <- critical
  }
}
results[window_type == "event", `:=`(
  conf_low_simultaneous_95 = event_rows$conf_low_simultaneous_95,
  conf_high_simultaneous_95 = event_rows$conf_high_simultaneous_95,
  p_value_simultaneous = event_rows$p_value_simultaneous,
  simultaneous_critical_95 = event_rows$simultaneous_critical_95
)]

output_stem <- function(stem) {
  if (primary_estimator == "plain") stem else paste0(stem, "_dr")
}

fwrite(
  results[window_type == "event"][, window_type := NULL],
  output_path(output_stem("event_study_monthly_road_corridor"))
)
fwrite(
  results[window_type == "average"][, window_type := NULL],
  output_path(output_stem("estimates_monthly_road_corridor"))
)
fwrite(diagnostics, output_path(output_stem("diagnostics_monthly_road_corridor")))
fwrite(pretrends, output_path(output_stem("pretrend_tests_monthly_road_corridor")))
fwrite(bootstrap_draws, output_path(output_stem("bootstrap_monthly_road_corridor")))
fwrite(balance_diagnostics, output_path(output_stem("balance_monthly_road_corridor")))
fwrite(
  cohort_diagnostics,
  output_path(output_stem("leave_one_cohort_out_monthly_road_corridor"))
)

message("First-year injury-crash estimates:")
print(results[
  outcome == "accidents" & window == "months_1_12",
  .(radius_km, estimate, std_error_bootstrap, conf_low_95, conf_high_95,
    p_value_normal, treated_radars, mean_monthly_reference)
])
message("Pretrend tests:")
print(pretrends)
