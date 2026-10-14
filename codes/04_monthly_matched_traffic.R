#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(sandwich)
})

root <- normalizePath(".")
processed <- file.path(root, "data", "processed")
traffic_dir <- file.path(root, "data", "traffic")
out_dir <- file.path(root, "results", "current")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

window_months <- 24L
outcome_radius_m <- 500
radar_exclusion_m <- 5000
maximum_control_distance_m <- 200000
maximum_radar_section_distance_m <- 300
speed_point_types <- c("Radar fixe", "Radar discriminant")

wgs84_to_lambert93 <- function(latitude, longitude) {
  eccentricity <- 0.081819191042816
  exponent <- 0.725607765053267
  constant <- 11754255.426096
  p <- pi / 180
  phi <- latitude * p
  radius <- constant * exp(
    -exponent * (atanh(sin(phi)) - eccentricity * atanh(eccentricity * sin(phi)))
  )
  angle <- exponent * (longitude * p - 3 * p)
  list(
    x = 700000 + radius * sin(angle),
    y = 12655612.049876 - radius * cos(angle)
  )
}

point_segment_distance <- function(px, py, x1, y1, x2, y2) {
  dx <- x2 - x1
  dy <- y2 - y1
  denominator <- dx^2 + dy^2
  fraction <- ifelse(
    denominator > 0,
    ((px - x1) * dx + (py - y1) * dy) / denominator,
    0
  )
  fraction <- pmax(0, pmin(1, fraction))
  sqrt((px - x1 - fraction * dx)^2 + (py - y1 - fraction * dy)^2)
}

normalize_national_route <- function(value) {
  value <- toupper(gsub("[^A-Z0-9]", "", value))
  match <- regexec("^(?:RN|N|A)([0-9]{1,4})", value)
  parts <- regmatches(value, match)
  vapply(parts, function(x) {
    if (length(x) < 2L) return(NA_character_)
    prefix <- if (grepl("^A", x[1])) "A" else "N"
    paste0(prefix, sprintf("%04d", as.integer(x[2])))
  }, character(1))
}

read_traffic <- function(year) {
  path <- file.path(traffic_dir, sprintf("tmja_%d.csv", year))
  if (!file.exists(path)) stop("Fichier TMJA absent : ", path)
  x <- fread(path, sep = "\t", dec = ",")
  x <- x[
    !is.na(TMJA) & TMJA > 0 &
      !is.na(xD) & !is.na(yD) & !is.na(xF) & !is.na(yF)
  ]
  x[, traffic_year := year]
  x[, segment_row := .I]
  x[, `:=`(
    midpoint_x = (xD + xF) / 2,
    midpoint_y = (yD + yF) / 2,
    chord_m = sqrt((xF - xD)^2 + (yF - yD)^2)
  )]
  # Les longs tronçons très courbes sont impropres à un contrôle ponctuel basé
  # sur le milieu de la corde entre leurs extrémités.
  x[longueur <= 5000 & longueur > 0 & chord_m / longueur >= 0.5]
}

message("Lecture des radars et conversion en Lambert-93...")
radars <- fread(file.path(processed, "radars.csv.gz"))[
  observe_2018 == 1L & !is.na(latitude) & !is.na(longitude)
]
coordinates <- wgs84_to_lambert93(radars$latitude, radars$longitude)
radars[, `:=`(x = coordinates$x, y = coordinates$y)]

speed_radars <- radars[
  type_radar_libelle_2018 %chin% speed_point_types &
    !is.na(date_installation_2018)
]
speed_radars[, installation_date := as.IDate(date_installation_2018)]
speed_radars[, installation_year := as.integer(format(installation_date, "%Y"))]

candidates <- speed_radars[installation_year %between% c(2013L, 2018L)]
nearest_other <- vapply(seq_len(nrow(candidates)), function(i) {
  distance <- sqrt(
    (candidates$x[i] - speed_radars$x)^2 +
      (candidates$y[i] - speed_radars$y)^2
  )
  distance[speed_radars$id_radar_2018 == candidates$id_radar_2018[i]] <- Inf
  min(distance)
}, numeric(1))
candidates[, nearest_other_speed_radar_m := nearest_other]
candidates <- candidates[nearest_other_speed_radar_m > 2000]
candidates[, normalized_route := normalize_national_route(route_2018)]
candidates <- candidates[!is.na(normalized_route)]

traffic <- setNames(lapply(2012:2017, read_traffic), as.character(2012:2017))

message("Création des listes de contrôles admissibles...")
eligible <- list()
treated_rows <- list()
j <- 1L
for (i in seq_len(nrow(candidates))) {
  radar <- candidates[i]
  pre_year <- radar$installation_year - 1L
  sections <- traffic[[as.character(pre_year)]]
  same_route <- sections[route == radar$normalized_route]
  if (!nrow(same_route)) next

  distance_to_section <- point_segment_distance(
    radar$x, radar$y,
    same_route$xD, same_route$yD, same_route$xF, same_route$yF
  )
  treated_index <- which.min(distance_to_section)
  if (distance_to_section[treated_index] > maximum_radar_section_distance_m) next
  treated_section <- same_route[treated_index]

  controls <- sections[route == radar$normalized_route]
  if (!nrow(controls)) next
  controls[, distance_to_treated_m := sqrt(
    (midpoint_x - radar$x)^2 + (midpoint_y - radar$y)^2
  )]
  controls[, nearest_known_radar_m := vapply(seq_len(.N), function(k) {
    min(sqrt(
      (midpoint_x[k] - radars$x)^2 + (midpoint_y[k] - radars$y)^2
    ))
  }, numeric(1))]
  controls[, traffic_ratio := TMJA / treated_section$TMJA]
  controls <- controls[
    nearest_known_radar_m > radar_exclusion_m &
      distance_to_treated_m <= maximum_control_distance_m &
      traffic_ratio %between% c(0.5, 2)
  ]
  if (!nrow(controls)) next
  controls[, match_score := abs(log(traffic_ratio)) +
             0.10 * distance_to_treated_m / maximum_control_distance_m]
  setorder(controls, match_score)
  controls[, id_radar := as.character(radar$id_radar_2018)]
  eligible[[j]] <- controls
  treated_rows[[j]] <- data.table(
    id_radar = as.character(radar$id_radar_2018),
    installation_date = as.character(radar$installation_date),
    installation_year = radar$installation_year,
    type_radar = radar$type_radar_libelle_2018,
    route_radar = radar$route_2018,
    normalized_route = radar$normalized_route,
    treated_x = radar$x,
    treated_y = radar$y,
    treated_tmja = treated_section$TMJA,
    treated_section_distance_m = distance_to_section[treated_index],
    eligible_controls = nrow(controls)
  )
  j <- j + 1L
}
eligible <- rbindlist(eligible, fill = TRUE)
treated_rows <- rbindlist(treated_rows, fill = TRUE)

# Les cas les plus contraints sont appariés d'abord. Deux contrôles sélectionnés
# ne peuvent pas être situés à moins de 2 km l'un de l'autre.
setorder(treated_rows, eligible_controls, id_radar)
chosen <- list()
chosen_x <- numeric()
chosen_y <- numeric()
for (i in seq_len(nrow(treated_rows))) {
  treated <- treated_rows[i]
  options <- eligible[id_radar == treated$id_radar]
  if (length(chosen_x)) {
    options <- options[vapply(seq_len(.N), function(k) {
      all(sqrt((midpoint_x[k] - chosen_x)^2 + (midpoint_y[k] - chosen_y)^2) > 2000)
    }, logical(1))]
  }
  if (!nrow(options)) next
  selected <- options[1]
  chosen_x <- c(chosen_x, selected$midpoint_x)
  chosen_y <- c(chosen_y, selected$midpoint_y)
  chosen[[length(chosen) + 1L]] <- cbind(treated, selected)
}
matches <- rbindlist(chosen, fill = TRUE)
matches[, pair_id := sprintf("P%03d", seq_len(.N))]
matches[, control_tmja := TMJA]
matches[, control_to_treated_tmja_ratio := control_tmja / treated_tmja]

match_export <- matches[, .(
  pair_id, id_radar, installation_date, installation_year, type_radar,
  route_radar, normalized_route, traffic_year, treated_tmja, control_tmja,
  control_to_treated_tmja_ratio, treated_section_distance_m,
  control_distance_to_treated_km = distance_to_treated_m / 1000,
  treated_x, treated_y, control_x = midpoint_x, control_y = midpoint_y,
  control_department = depPrD, eligible_controls
)]
fwrite(match_export, file.path(out_dir, "matched_traffic_segments.csv"))
message("Paires retenues : ", nrow(matches))

message("Lecture et projection des accidents...")
accidents <- fread(
  file.path(processed, "accidents.csv.gz"),
  select = c("id_accident", "annee", "mois", "latitude", "longitude",
             "geolocalisation_valide")
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
accident_xy <- wgs84_to_lambert93(accidents$latitude, accidents$longitude)
accidents[, `:=`(x = accident_xy$x, y = accident_xy$y)]
accidents[, month_index := annee * 12L + mois - 1L]

message("Construction du panel mensuel traité-témoin...")
panels <- vector("list", nrow(matches))
for (i in seq_len(nrow(matches))) {
  pair <- matches[i]
  installation_month <- pair$installation_year * 12L +
    as.integer(substr(pair$installation_date, 6, 7)) - 1L
  relative_months <- c(-window_months:-1L, 1L:window_months)
  base <- data.table(
    pair_id = pair$pair_id,
    id_radar = pair$id_radar,
    event_month = relative_months,
    month_index = installation_month + relative_months
  )
  nearby <- accidents[
    month_index %between% range(base$month_index) &
      ((abs(x - pair$treated_x) <= outcome_radius_m &
          abs(y - pair$treated_y) <= outcome_radius_m) |
         (abs(x - pair$midpoint_x) <= outcome_radius_m &
          abs(y - pair$midpoint_y) <= outcome_radius_m))
  ]
  if (nrow(nearby)) {
    nearby[, treated_distance_m := sqrt(
      (x - pair$treated_x)^2 + (y - pair$treated_y)^2
    )]
    nearby[, control_distance_m := sqrt(
      (x - pair$midpoint_x)^2 + (y - pair$midpoint_y)^2
    )]
    nearby[, zone := fifelse(
      treated_distance_m <= outcome_radius_m, "treated",
      fifelse(control_distance_m <= outcome_radius_m, "control", NA_character_)
    )]
    nearby <- nearby[!is.na(zone)]
    counts <- nearby[, .(
      accidents = .N,
      killed = sum(killed)
    ), by = .(month_index, zone)]
    wide <- dcast(counts, month_index ~ zone,
                  value.var = c("accidents", "killed"), fill = 0)
    base <- wide[base, on = "month_index"]
  }
  for (name in c("accidents_treated", "accidents_control",
                 "killed_treated", "killed_control")) {
    if (!name %in% names(base)) base[, (name) := 0]
    setnafill(base, type = "const", fill = 0, cols = name)
  }
  base[, calendar_year := month_index %/% 12L]
  base[, calendar_month_number := month_index %% 12L + 1L]
  base[, calendar_month := sprintf("%04d-%02d", calendar_year, calendar_month_number)]
  base[, post := as.integer(event_month > 0L)]
  base[, accidents_difference := accidents_treated - accidents_control]
  base[, killed_difference := killed_treated - killed_control]
  panels[[i]] <- base
}
panel <- rbindlist(panels, fill = TRUE)
setorder(panel, pair_id, month_index)
fwrite(panel, file.path(out_dir, "panel_monthly_matched_traffic.csv.gz"))

cluster_tidy <- function(model, cluster, terms, outcome, specification) {
  vc <- vcovCL(model, cluster = cluster, type = "HC1")
  cf <- coef(model)
  se <- sqrt(diag(vc)[terms])
  degrees <- length(unique(cluster)) - 1L
  data.table(
    outcome = outcome, specification = specification, term = terms,
    estimate = unname(cf[terms]), std_error_cluster = unname(se),
    conf_low_95 = unname(cf[terms] - qt(0.975, degrees) * se),
    conf_high_95 = unname(cf[terms] + qt(0.975, degrees) * se),
    p_value_cluster = 2 * pt(-abs(unname(cf[terms] / se)), degrees),
    n = nobs(model), pairs = length(unique(cluster))
  )
}

message("Estimation sur les paires appariées...")
average_results <- rbindlist(lapply(
  c("accidents_difference", "killed_difference"),
  function(outcome) {
    model <- lm(
      reformulate(c("post", "factor(calendar_month)", "factor(pair_id)"), outcome),
      data = panel
    )
    cluster_tidy(model, panel$pair_id, "post", outcome,
                 "monthly_matched_pre_treatment_TMJA")
  }
))
fwrite(average_results, file.path(out_dir, "estimates_monthly_matched_traffic.csv"))

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
  "-24:-19", "-18:-13", "-12:-7", "+1:+6", "+7:+12", "+13:+18", "+19:+24"
)
event_results <- list()
event_diagnostics <- list()
for (i in seq_along(c("accidents_difference", "killed_difference"))) {
  outcome <- c("accidents_difference", "killed_difference")[i]
  model <- lm(
    reformulate(c(event_terms, "factor(calendar_month)", "factor(pair_id)"), outcome),
    data = panel
  )
  event_results[[i]] <- cluster_tidy(
    model, panel$pair_id, event_terms, outcome,
    "event_study_matched_pre_treatment_TMJA"
  )
  event_results[[i]][, event_window_months := event_labels]
  vc <- vcovCL(model, cluster = panel$pair_id, type = "HC1")
  leads <- event_terms[1:3]
  b <- coef(model)[leads]
  statistic <- as.numeric(t(b) %*% solve(vc[leads, leads], b))
  event_diagnostics[[i]] <- data.table(
    outcome = outcome,
    pretrend_wald_chisq = statistic,
    pretrend_df = 3L,
    pretrend_p_value = pchisq(statistic, 3L, lower.tail = FALSE)
  )
}
event_results <- rbindlist(event_results)
event_diagnostics <- rbindlist(event_diagnostics)
fwrite(event_results, file.path(out_dir, "event_study_monthly_matched_traffic.csv"))
fwrite(event_diagnostics, file.path(out_dir, "event_study_monthly_matched_traffic_diagnostics.csv"))

message("Analyse appariée terminée.")
print(match_export[, .(
  pairs = .N,
  median_traffic_ratio = median(control_to_treated_tmja_ratio),
  p10_traffic_ratio = quantile(control_to_treated_tmja_ratio, 0.10),
  p90_traffic_ratio = quantile(control_to_treated_tmja_ratio, 0.90),
  median_control_distance_km = median(control_distance_to_treated_km)
)])
print(average_results)
print(event_diagnostics)
print(event_results)

