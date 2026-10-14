#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(sandwich)
})

root <- normalizePath(".")
processed <- file.path(root, "data", "processed")
out_dir <- file.path(root, "results", "current")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

earth_radius_km <- 6371.0088
inner_radius_km <- 0.5
outer_radius_km <- 1.0
isolation_radius_km <- 2.0
window_months <- 24L
speed_point_types <- c("Radar fixe", "Radar discriminant")

haversine_km <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 +
    cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  earth_radius_km * 2 * atan2(sqrt(a), sqrt(pmax(0, 1 - a)))
}

message("Lecture et sélection des radars ponctuels de vitesse...")
radars <- fread(file.path(processed, "radars.csv.gz"))[
  observe_2018 == 1L &
    type_radar_libelle_2018 %chin% speed_point_types &
    !is.na(date_installation_2018) & !is.na(latitude) & !is.na(longitude)
]
radars[, installation_date := as.IDate(date_installation_2018)]
radars[, installation_year := as.integer(format(installation_date, "%Y"))]

candidates <- radars[installation_year %between% c(2013L, 2018L)]
nearest_other_km <- numeric(nrow(candidates))
for (i in seq_len(nrow(candidates))) {
  distance <- haversine_km(
    candidates$latitude[i], candidates$longitude[i],
    radars$latitude, radars$longitude
  )
  distance[radars$id_radar_2018 == candidates$id_radar_2018[i]] <- Inf
  nearest_other_km[i] <- min(distance)
}
candidates[, nearest_other_speed_radar_km := nearest_other_km]
candidates <- candidates[nearest_other_speed_radar_km > isolation_radius_km]
setorder(candidates, id_radar_2018)

message("Radars retenus : ", nrow(candidates))

message("Lecture des accidents, des dates et des décès...")
accidents <- fread(
  file.path(processed, "accidents.csv.gz"),
  select = c("id_accident", "annee", "mois", "date_accident", "latitude",
             "longitude", "geolocalisation_valide")
)[
  annee %between% c(2011L, 2020L) & geolocalisation_valide == 1L &
    !is.na(latitude) & !is.na(longitude) & !is.na(mois)
]
deaths <- fread(
  file.path(processed, "usagers.csv.gz"),
  select = c("id_accident", "gravite", "source_annee")
)[source_annee %between% c(2011L, 2020L),
  .(killed = sum(gravite == 2L, na.rm = TRUE)), by = id_accident]
accidents <- deaths[accidents, on = "id_accident"]
accidents[is.na(killed), killed := 0L]
accidents[, month_index := annee * 12L + mois - 1L]

message("Construction du panel mensuel proche/couronne...")
panels <- vector("list", nrow(candidates))
for (i in seq_len(nrow(candidates))) {
  radar <- candidates[i]
  installation_month <- as.integer(format(radar$installation_date, "%Y")) * 12L +
    as.integer(format(radar$installation_date, "%m")) - 1L
  relative_months <- c(-window_months:-1L, 1L:window_months)
  base <- data.table(
    id_radar = as.character(radar$id_radar_2018),
    installation_date = as.character(radar$installation_date),
    installation_year = radar$installation_year,
    type_radar = radar$type_radar_libelle_2018,
    nearest_other_speed_radar_km = radar$nearest_other_speed_radar_km,
    event_month = relative_months,
    month_index = installation_month + relative_months
  )

  latitude_delta <- outer_radius_km / 111.32
  longitude_delta <- outer_radius_km /
    (111.32 * cos(radar$latitude * pi / 180))
  nearby <- accidents[
    month_index %between% range(base$month_index) &
      abs(latitude - radar$latitude) <= latitude_delta &
      abs(longitude - radar$longitude) <= longitude_delta
  ]
  if (nrow(nearby)) {
    nearby[, distance_km := haversine_km(
      radar$latitude, radar$longitude, latitude, longitude
    )]
    nearby <- nearby[distance_km <= outer_radius_km]
    nearby[, zone := fifelse(distance_km <= inner_radius_km, "inner", "outer")]
    counts <- nearby[, .(
      accidents = .N,
      killed = sum(killed)
    ), by = .(month_index, zone)]
    wide <- dcast(
      counts, month_index ~ zone,
      value.var = c("accidents", "killed"), fill = 0
    )
    base <- wide[base, on = "month_index"]
  }
  for (name in c("accidents_inner", "accidents_outer", "killed_inner", "killed_outer")) {
    if (!name %in% names(base)) base[, (name) := 0]
    setnafill(base, type = "const", fill = 0, cols = name)
  }
  base[, calendar_year := month_index %/% 12L]
  base[, calendar_month_number := month_index %% 12L + 1L]
  base[, calendar_month := sprintf("%04d-%02d", calendar_year, calendar_month_number)]
  base[, post := as.integer(event_month > 0L)]
  # La couronne de 0,5 à 1 km a une aire trois fois supérieure au disque
  # intérieur. Les différences sont exprimées en équivalent de surface interne.
  base[, accidents_local_difference := accidents_inner - accidents_outer / 3]
  base[, killed_local_difference := killed_inner - killed_outer / 3]
  panels[[i]] <- base
}
panel <- rbindlist(panels, use.names = TRUE, fill = TRUE)
setorder(panel, id_radar, month_index)
fwrite(panel, file.path(out_dir, "panel_monthly_local_control.csv.gz"))

cluster_row <- function(model, cluster, term, outcome, specification, sample_name) {
  vc <- vcovCL(model, cluster = cluster, type = "HC1")
  estimate <- unname(coef(model)[term])
  se <- sqrt(diag(vc)[term])
  data.table(
    outcome = outcome,
    specification = specification,
    sample = sample_name,
    term = term,
    estimate = estimate,
    std_error_cluster = unname(se),
    conf_low_95 = estimate - qt(0.975, length(unique(cluster)) - 1L) * se,
    conf_high_95 = estimate + qt(0.975, length(unique(cluster)) - 1L) * se,
    p_value_cluster = 2 * pt(-abs(estimate / se), length(unique(cluster)) - 1L),
    n = nobs(model),
    clusters = length(unique(cluster))
  )
}

estimate_post <- function(x, outcome, sample_name) {
  model <- lm(
    reformulate(c("post", "factor(calendar_month)", "factor(id_radar)"), outcome),
    data = x
  )
  cluster_row(model, x$id_radar, "post", outcome,
              "monthly_local_ring_FE", sample_name)
}

message("Estimation de l'effet moyen avant/après...")
monthly_estimates <- rbindlist(lapply(
  c("accidents_local_difference", "killed_local_difference"),
  function(outcome) {
    rbindlist(list(
      estimate_post(panel, outcome, "full_2011_2020"),
      estimate_post(
        panel[!(calendar_month >= "2020-03" & calendar_month <= "2020-12")],
        outcome, "excluding_covid_2020_03_12"
      )
    ))
  }
))
fwrite(monthly_estimates, file.path(out_dir, "estimates_monthly_local_control.csv"))

# Semestres relatifs; les six mois -6 à -1 forment la référence omise.
panel[, `:=`(
  event_m24_m19 = as.integer(event_month %between% c(-24L, -19L)),
  event_m18_m13 = as.integer(event_month %between% c(-18L, -13L)),
  event_m12_m7 = as.integer(event_month %between% c(-12L, -7L)),
  event_p1_p6 = as.integer(event_month %between% c(1L, 6L)),
  event_p7_p12 = as.integer(event_month %between% c(7L, 12L)),
  event_p13_p18 = as.integer(event_month %between% c(13L, 18L)),
  event_p19_p24 = as.integer(event_month %between% c(19L, 24L))
)]
event_terms <- c(
  "event_m24_m19", "event_m18_m13", "event_m12_m7",
  "event_p1_p6", "event_p7_p12", "event_p13_p18", "event_p19_p24"
)
event_labels <- c(
  event_m24_m19 = "-24:-19", event_m18_m13 = "-18:-13",
  event_m12_m7 = "-12:-7", event_p1_p6 = "+1:+6",
  event_p7_p12 = "+7:+12", event_p13_p18 = "+13:+18",
  event_p19_p24 = "+19:+24"
)

message("Étude d'événement mensuelle...")
event_results <- list()
event_diagnostics <- list()
j <- 1L
for (outcome in c("accidents_local_difference", "killed_local_difference")) {
  model <- lm(
    reformulate(c(event_terms, "factor(calendar_month)", "factor(id_radar)"), outcome),
    data = panel
  )
  vc <- vcovCL(model, cluster = panel$id_radar, type = "HC1")
  cf <- coef(model)
  se <- sqrt(diag(vc)[event_terms])
  event_results[[j]] <- data.table(
    outcome = outcome,
    event_window_months = unname(event_labels[event_terms]),
    term = event_terms,
    estimate = unname(cf[event_terms]),
    std_error_cluster = unname(se),
    conf_low_95 = unname(cf[event_terms] - qt(0.975, nrow(candidates) - 1L) * se),
    conf_high_95 = unname(cf[event_terms] + qt(0.975, nrow(candidates) - 1L) * se),
    p_value_cluster = 2 * pt(-abs(unname(cf[event_terms] / se)), nrow(candidates) - 1L),
    reference_window_months = "-6:-1",
    n = nobs(model),
    clusters = nrow(candidates)
  )
  lead_terms <- event_terms[1:3]
  b <- cf[lead_terms]
  v <- vc[lead_terms, lead_terms, drop = FALSE]
  statistic <- as.numeric(t(b) %*% solve(v, b))
  event_diagnostics[[j]] <- data.table(
    outcome = outcome,
    pretrend_wald_chisq = statistic,
    pretrend_df = length(lead_terms),
    pretrend_p_value = pchisq(statistic, length(lead_terms), lower.tail = FALSE)
  )
  j <- j + 1L
}
event_results <- rbindlist(event_results)
event_diagnostics <- rbindlist(event_diagnostics)
fwrite(event_results, file.path(out_dir, "event_study_monthly_local_control.csv"))
fwrite(event_diagnostics, file.path(out_dir, "event_study_monthly_local_diagnostics.csv"))

sample_description <- data.table(
  initial_speed_point_candidates_2013_2018 = nrow(
    radars[installation_year %between% c(2013L, 2018L)]
  ),
  isolated_candidates = nrow(candidates),
  inner_radius_km = inner_radius_km,
  outer_radius_km = outer_radius_km,
  isolation_radius_km = isolation_radius_km,
  months_before = window_months,
  months_after = window_months,
  panel_rows = nrow(panel),
  first_calendar_month = min(panel$calendar_month),
  last_calendar_month = max(panel$calendar_month)
)
fwrite(sample_description, file.path(out_dir, "monthly_local_sample.csv"))

message("Analyse mensuelle terminée.")
print(sample_description)
print(monthly_estimates)
print(event_diagnostics)
print(event_results)

