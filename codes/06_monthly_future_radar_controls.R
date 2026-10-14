#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
processed <- file.path(root, "data", "processed")
out_dir <- file.path(root, "results", "current")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

earth_radius_km <- 6371.0088
outcome_radius_km <- as.numeric(Sys.getenv("OUTCOME_RADIUS_KM", "0.5"))
output_suffix <- Sys.getenv("OUTPUT_SUFFIX", "")
exclude_large_cities <- Sys.getenv("EXCLUDE_LARGE_CITIES", "0") == "1"
large_city_population_thousands <- 100
large_city_buffer_km <- 10
output_path <- function(stem) {
  file.path(out_dir, paste0(stem, output_suffix, ".csv"))
}
prior_radar_exclusion_km <- max(1.0, outcome_radius_km)
minimum_control_distance_km <- max(2.0, 2 * outcome_radius_km)
minimum_controls <- 10L
bootstrap_repetitions <- 499L
speed_point_types <- c("Radar fixe", "Radar discriminant")

haversine_km <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 +
    cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  earth_radius_km * 2 * atan2(sqrt(a), sqrt(pmax(0, 1 - a)))
}

road_class <- function(route) {
  route <- toupper(gsub("[^A-Z0-9]", "", route))
  fifelse(grepl("^A[0-9]", route), "autoroute",
          fifelse(grepl("^(RN|N)[0-9]", route), "nationale",
                  fifelse(grepl("^(RD|D)[0-9]", route), "departementale", "autre")))
}

message("Reconstruction de la première date des radars ponctuels...")
radars <- fread(file.path(processed, "radars.csv.gz"))[
  !is.na(latitude) & !is.na(longitude)
]
radars[, `:=`(
  date_2025 = as.IDate(date_mise_en_service),
  date_2018 = as.IDate(date_installation_2018),
  current_point = type_radar_code %chin% c("ETF", "ETD", "ETT", "ETU"),
  old_point = type_radar_libelle_2018 %chin% speed_point_types
)]
radars[, point_speed := current_point | (actif_liste_2025 == 0L & old_point)]
radars <- radars[point_speed == TRUE]
radars[, installation_date := as.IDate(NA)]
radars[current_point == TRUE, installation_date := date_2025]
# Un appareil 2025 apparié à un radar ponctuel de 2018 n'est pas une nouvelle
# implantation : on conserve la première date des deux inventaires.
radars[current_point == TRUE & old_point == TRUE & !is.na(date_2018),
       installation_date := fifelse(
         is.na(date_2025), date_2018, pmin(date_2025, date_2018)
       )]
radars[actif_liste_2025 == 0L & old_point == TRUE,
       installation_date := date_2018]
radars <- radars[!is.na(installation_date)]
point_sites_with_date <- nrow(radars)
radars[, installation_month := as.integer(format(installation_date, "%Y")) * 12L +
         as.integer(format(installation_date, "%m")) - 1L]
radars[, installation_year := as.integer(format(installation_date, "%Y"))]
radars[, route_for_class := fcoalesce(route_2018, "")]
radars[, route_class := road_class(route_for_class)]

# Sensibilité urbaine : une « grande ville » est une commune d'au moins
# 100 000 habitants. On retire les sites situés à moins de 10 km de son centre,
# ce qui évite de dépendre des limites communales anciennes du fichier source.
radars[, near_large_city := FALSE]
if (exclude_large_cities) {
  city_environment <- new.env(parent = emptyenv())
  load(file.path(root, "data", "auxiliary", "result.radar.commune.rda"), envir = city_environment)
  city_centres <- unique(as.data.table(city_environment$radars)[
    Population >= large_city_population_thousands &
      !is.na(lat.commune) & !is.na(long.commune),
    .(INSEE_COM, Population, latitude = lat.commune, longitude = long.commune)
  ], by = "INSEE_COM")
  for (i in seq_len(nrow(radars))) {
    radars$near_large_city[i] <- any(haversine_km(
      radars$latitude[i], radars$longitude[i],
      city_centres$latitude, city_centres$longitude
    ) <= large_city_buffer_km)
  }
}

# Les cohortes 2012-2022 ont au plus 24 mois avant et après dans BAAC
# 2010-2024. Les implantations 2023-2024 restent dans le bassin de témoins.
candidates <- radars[installation_year %between% c(2012L, 2024L)]
candidate_sites_2012_2024_before_city_filter <- nrow(candidates)
large_city_excluded_candidates <- candidates[near_large_city == TRUE, .N]
if (exclude_large_cities) candidates <- candidates[near_large_city == FALSE]
candidate_sites_2012_2024 <- nrow(candidates)
potential_treatments_2012_2022 <- candidates[installation_year <= 2022L, .N]
keep <- logical(nrow(candidates))
next_nearby_installation_month <- rep(Inf, nrow(candidates))
for (i in seq_len(nrow(candidates))) {
  distance <- haversine_km(
    candidates$latitude[i], candidates$longitude[i],
    radars$latitude, radars$longitude
  )
  other <- radars$cle_radar != candidates$cle_radar[i]
  prior_or_same <- radars$installation_month <= candidates$installation_month[i]
  keep[i] <- !any(
    distance < prior_radar_exclusion_km & other & prior_or_same,
    na.rm = TRUE
  )
  later_nearby <- radars$installation_month[
    distance < prior_radar_exclusion_km & other &
      radars$installation_month > candidates$installation_month[i]
  ]
  if (length(later_nearby)) next_nearby_installation_month[i] <- min(later_nearby)
}
candidates[, next_nearby_installation_month := next_nearby_installation_month]
candidates <- candidates[keep]
setorder(candidates, id_radar_2018)
candidates[, radar_index := .I]
candidates[, eligible_treated := installation_year <= 2022L]
candidates[, vma_group := fifelse(is.na(vma_kmh), "vma_missing", paste0("vma_", vma_kmh))]
candidates[, matching_stratum := paste(route_class, vma_group, sep = "__")]
message(
  "Sites admissibles : ", nrow(candidates),
  "; traitements 2012-2022 : ", candidates[eligible_treated == TRUE, .N]
)

message("Construction des comptes mensuels sur tous les futurs sites...")
accidents <- fread(
  file.path(processed, "accidents.csv.gz"),
  select = c("id_accident", "annee", "mois", "latitude", "longitude",
             "geolocalisation_valide")
)[
  annee %between% c(2010L, 2024L) & geolocalisation_valide == 1L &
    !is.na(latitude) & !is.na(longitude) & !is.na(mois)
]
deaths <- fread(
  file.path(processed, "usagers.csv.gz"),
  select = c("id_accident", "gravite", "source_annee")
)[source_annee %between% c(2010L, 2024L),
  .(killed = sum(gravite == 2L, na.rm = TRUE)), by = id_accident]
accidents <- deaths[accidents, on = "id_accident"]
accidents[is.na(killed), killed := 0L]
accidents[, month_index := annee * 12L + mois - 1L]

all_months <- seq(2010L * 12L, 2024L * 12L + 11L)
counts <- vector("list", nrow(candidates))
for (i in seq_len(nrow(candidates))) {
  radar <- candidates[i]
  latitude_delta <- outcome_radius_km / 111.32
  longitude_delta <- outcome_radius_km /
    (111.32 * cos(radar$latitude * pi / 180))
  nearby <- accidents[
    abs(latitude - radar$latitude) <= latitude_delta &
      abs(longitude - radar$longitude) <= longitude_delta
  ]
  if (nrow(nearby)) {
    nearby[, distance_km := haversine_km(
      radar$latitude, radar$longitude, latitude, longitude
    )]
    nearby <- nearby[distance_km <= outcome_radius_km]
    monthly <- nearby[, .(accidents = .N, killed = sum(killed)), by = month_index]
  } else {
    monthly <- data.table(month_index = integer(), accidents = integer(), killed = integer())
  }
  base <- data.table(month_index = all_months)
  base <- monthly[base, on = "month_index"]
  base[is.na(accidents), `:=`(accidents = 0, killed = 0)]
  base[, radar_index := i]
  counts[[i]] <- base
}
counts <- rbindlist(counts)
accident_matrix <- dcast(counts, radar_index ~ month_index, value.var = "accidents")
killed_matrix <- dcast(counts, radar_index ~ month_index, value.var = "killed")
accident_values <- as.matrix(accident_matrix[, -"radar_index"])
killed_values <- as.matrix(killed_matrix[, -"radar_index"])
month_columns <- as.integer(colnames(accident_values))

mean_window <- function(values, radar_indices, months) {
  columns <- match(months, month_columns)
  if (anyNA(columns)) return(rep(NA_real_, length(radar_indices)))
  rowMeans(values[radar_indices, columns, drop = FALSE])
}

build_cells <- function(values, event_start, event_end) {
  cells <- list()
  j <- 1L
  treated_pool <- candidates[eligible_treated == TRUE, radar_index]
  for (i in treated_pool) {
    anchor <- candidates$installation_month[i]
    evaluation_months <- anchor + event_start:event_end
    reference_months <- anchor + (-6L:-1L)
    if (min(c(evaluation_months, reference_months)) < min(all_months) ||
        max(c(evaluation_months, reference_months)) > max(all_months)) next
    if (event_end > 0 &&
        candidates$next_nearby_installation_month[i] <= anchor + event_end) next

    untreated_through <- anchor + max(0L, event_end)
    controls <- candidates[
      installation_month > untreated_through &
        matching_stratum == candidates$matching_stratum[i],
      radar_index
    ]
    if (length(controls)) {
      distance <- haversine_km(
        candidates$latitude[i], candidates$longitude[i],
        candidates$latitude[controls], candidates$longitude[controls]
      )
      controls <- controls[distance > minimum_control_distance_km]
    }
    if (length(controls) < minimum_controls) next

    treated_change <- mean_window(values, i, evaluation_months) -
      mean_window(values, i, reference_months)
    control_change <- mean_window(values, controls, evaluation_months) -
      mean_window(values, controls, reference_months)
    cells[[j]] <- list(
      treated = i,
      controls = controls,
      treated_change = treated_change,
      control_change = control_change
    )
    j <- j + 1L
  }
  cells
}

aggregate_cells <- function(cells, weights = NULL) {
  if (is.null(weights)) weights <- rep(1, nrow(candidates))
  numerator <- 0
  denominator <- 0
  for (cell in cells) {
    treated_weight <- weights[cell$treated]
    control_weights <- weights[cell$controls]
    if (treated_weight == 0 || sum(control_weights) == 0) next
    control_mean <- weighted.mean(cell$control_change, control_weights)
    numerator <- numerator + treated_weight * (cell$treated_change - control_mean)
    denominator <- denominator + treated_weight
  }
  if (denominator == 0) return(NA_real_)
  numerator / denominator
}

event_windows <- list(
  `-24:-19` = c(-24L, -19L),
  `-18:-13` = c(-18L, -13L),
  `-12:-7` = c(-12L, -7L),
  `+1:+6` = c(1L, 6L),
  `+7:+12` = c(7L, 12L),
  `+13:+18` = c(13L, 18L),
  `+19:+24` = c(19L, 24L)
)

message("Estimation avec futurs sites de radar comme témoins...")
set.seed(20261011)
results <- list()
j <- 1L
for (outcome in c("accidents", "killed")) {
  values <- if (outcome == "accidents") accident_values else killed_values
  for (label in names(event_windows)) {
    window <- event_windows[[label]]
    cells <- build_cells(values, window[1], window[2])
    estimate <- aggregate_cells(cells)
    boot <- replicate(bootstrap_repetitions, {
      weights <- tabulate(
        sample.int(nrow(candidates), nrow(candidates), replace = TRUE),
        nbins = nrow(candidates)
      )
      aggregate_cells(cells, weights)
    })
    boot <- boot[is.finite(boot)]
    se <- sd(boot)
    results[[j]] <- data.table(
      outcome = outcome,
      radius_km = outcome_radius_km,
      event_window_months = label,
      estimate = estimate,
      std_error_bootstrap = se,
      conf_low_95 = estimate - qnorm(0.975) * se,
      conf_high_95 = estimate + qnorm(0.975) * se,
      p_value_normal = 2 * pnorm(-abs(estimate / se)),
      treated_radars = length(unique(vapply(cells, `[[`, integer(1), "treated"))),
      treated_installation_years = paste(sort(unique(
        candidates$installation_year[vapply(cells, `[[`, integer(1), "treated")]
      )), collapse = ";"),
      median_controls_per_radar = median(vapply(cells, function(x) length(x$controls), integer(1))),
      minimum_controls = minimum_controls,
      bootstrap_repetitions = length(boot),
      reference_window_months = "-6:-1"
    )
    j <- j + 1L
  }
}
results <- rbindlist(results)
fwrite(results, output_path("event_study_monthly_future_radar_controls"))

average_windows <- list(`months_1_12` = c(1L, 12L), `months_1_24` = c(1L, 24L))
average_results <- list()
j <- 1L
for (outcome in c("accidents", "killed")) {
  values <- if (outcome == "accidents") accident_values else killed_values
  for (label in names(average_windows)) {
    window <- average_windows[[label]]
    cells <- build_cells(values, window[1], window[2])
    estimate <- aggregate_cells(cells)
    boot <- replicate(bootstrap_repetitions, {
      weights <- tabulate(
        sample.int(nrow(candidates), nrow(candidates), replace = TRUE),
        nbins = nrow(candidates)
      )
      aggregate_cells(cells, weights)
    })
    boot <- boot[is.finite(boot)]
    se <- sd(boot)
    treated_indices <- unique(vapply(cells, `[[`, integer(1), "treated"))
    average_results[[j]] <- data.table(
      outcome = outcome,
      radius_km = outcome_radius_km,
      post_window = label,
      estimate = estimate,
      std_error_bootstrap = se,
      conf_low_95 = estimate - qnorm(0.975) * se,
      conf_high_95 = estimate + qnorm(0.975) * se,
      p_value_normal = 2 * pnorm(-abs(estimate / se)),
      treated_radars = length(treated_indices),
      treated_installation_years = paste(
        sort(unique(candidates$installation_year[treated_indices])), collapse = ";"
      ),
      median_controls_per_radar = median(
        vapply(cells, function(x) length(x$controls), integer(1))
      ),
      bootstrap_repetitions = length(boot),
      reference_window_months = "-6:-1"
    )
    j <- j + 1L
  }
}
average_results <- rbindlist(average_results)
fwrite(average_results, output_path("estimates_monthly_future_radar_controls"))

# Entonnoir d'échantillonnage : il rend explicite pourquoi les quelque 3 000
# radars ponctuels de l'inventaire ne peuvent pas tous identifier un effet.
sample_flow <- data.table(
  stage = c(
    "radars_ponctuels_avec_date",
    "sites_installes_2012_2024_avant_filtre_villes",
    "sites_installes_2012_2024_apres_filtre_villes",
    "traitements_potentiels_2012_2022",
    paste0("sites_sans_radar_anterieur_a_", outcome_radius_km, "km"),
    "traitements_admissibles_2012_2022",
    "traitements_estimables_pre_24_mois",
    "traitements_estimables_post_1_12",
    "traitements_estimables_post_1_24"
  ),
  n = c(
    point_sites_with_date,
    candidate_sites_2012_2024_before_city_filter,
    candidate_sites_2012_2024,
    potential_treatments_2012_2022,
    nrow(candidates),
    candidates[eligible_treated == TRUE, .N],
    results[outcome == "accidents" & event_window_months == "-24:-19", treated_radars],
    average_results[outcome == "accidents" & post_window == "months_1_12", treated_radars],
    average_results[outcome == "accidents" & post_window == "months_1_24", treated_radars]
  )
)
fwrite(sample_flow, output_path("monthly_future_radar_sample_flow"))

sample <- candidates[, .(
  outcome_radius_km = outcome_radius_km,
  prior_radar_exclusion_km = prior_radar_exclusion_km,
  minimum_control_distance_km = minimum_control_distance_km,
  large_city_exclusion = exclude_large_cities,
  large_city_population_threshold = large_city_population_thousands * 1000,
  large_city_buffer_km = large_city_buffer_km,
  sites_removed_by_large_city_filter = large_city_excluded_candidates,
  admissible_sites = .N,
  eligible_treated_radars = sum(eligible_treated),
  future_control_only_sites = sum(!eligible_treated),
  radar_fixe = sum(type_radar_libelle_2018 == "Radar fixe"),
  active_in_2025 = sum(actif_liste_2025 == 1L),
  historical_2018_only = sum(actif_liste_2025 == 0L),
  vma_observed = sum(!is.na(vma_kmh)),
  matching_strata = uniqueN(matching_stratum),
  autoroute = sum(route_class == "autoroute"),
  nationale = sum(route_class == "nationale"),
  departementale = sum(route_class == "departementale"),
  autre = sum(route_class == "autre")
)]
fwrite(sample, output_path("monthly_future_radar_sample"))

message("Analyse terminée.")
print(sample)
print(sample_flow)
print(results)
print(average_results)
