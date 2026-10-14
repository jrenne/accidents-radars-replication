#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
processed <- file.path(root, "data", "processed")
out_dir <- file.path(root, "results", "current")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

radii <- c(0.5, 1, 2, 5, 7)
years <- 2013:2021
earth_degree_km <- 40030 / 360

message("Lecture des accidents et des décès...")
acc_all <- fread(
  file.path(processed, "accidents.csv.gz"),
  select = c("id_accident", "annee", "latitude", "longitude",
             "geolocalisation_valide")
)[annee <= 2021 & geolocalisation_valide == 1L]

usr <- fread(
  file.path(processed, "usagers.csv.gz"),
  select = c("id_accident", "gravite", "source_annee")
)[source_annee <= 2021]
deaths <- usr[, .(killed = sum(gravite == 2L, na.rm = TRUE)), by = id_accident]
rm(usr)
acc_all <- deaths[acc_all, on = "id_accident"]
acc_all[is.na(killed), killed := 0L]

acc_all <- acc_all[!is.na(latitude) & !is.na(longitude),
           .(id_accident, annee, latitude, longitude, killed)]
acc <- acc_all[annee %in% years]
rm(deaths)

message("Lecture des radars historiques...")
rad <- fread(file.path(processed, "radars.csv.gz"))[
  observe_2018 == 1L & !is.na(id_radar_2018) & !is.na(date_installation_2018)
]
rad[, installation_year := as.integer(substr(date_installation_2018, 1, 4))]
rad[, installation_date := as.IDate(date_installation_2018)]
rad_all <- rad[, .(id_radar = as.character(id_radar_2018), installation_date,
                   installation_year, latitude, longitude, type_radar_libelle_2018)]
rad_candidates <- rad_all[installation_year %in% 2013:2018]

# Le Tableau 1 du mémoire fournit un test indépendant pour le radar 13618.
radar_example <- rad_all[id_radar == "13618"][1]
distance_example <- sqrt(
  (acc_all$latitude - radar_example$latitude)^2 +
    (acc_all$longitude - radar_example$longitude)^2
) * earth_degree_km
example <- acc_all[distance_example < 2,
                   .(accidents = .N, killed = sum(killed)), by = annee]
example <- merge(data.table(annee = 2005:2021), example, by = "annee", all.x = TRUE)
example[is.na(accidents), `:=`(accidents = 0L, killed = 0L)]
example[, `:=`(
  thesis_accidents = c(24,17,18,32,28,45,51,41,43,37,25,22,18,20,25,15,9),
  thesis_killed = c(0,0,1,1,0,1,2,0,0,2,0,0,1,0,1,1,0)
)]
example[, `:=`(
  accidents_difference = accidents - thesis_accidents,
  killed_difference = killed - thesis_killed
)]
fwrite(example, file.path(out_dir, "radar_13618_check.csv"))
rm(acc_all, distance_example)

# Pour chaque rayon, reproduit l'exclusion des caméras qui avaient déjà une autre
# caméra dans leur voisinage lors de leur installation.
message("Construction des échantillons de radars isolés...")
radar_samples <- rbindlist(lapply(radii, function(radius) {
  keep <- logical(nrow(rad_candidates))
  for (i in seq_len(nrow(rad_candidates))) {
    d <- sqrt(
      (rad_all$latitude - rad_candidates$latitude[i])^2 +
        (rad_all$longitude - rad_candidates$longitude[i])^2
    ) * earth_degree_km
    prior <- rad_all$installation_date < rad_candidates$installation_date[i]
    keep[i] <- !any(d < radius & prior, na.rm = TRUE)
  }
  rad_candidates[keep, .(radius_km = radius, id_radar, installation_year,
                          latitude, longitude)]
}))

message("Comptage spatial des accidents (ce calcul peut prendre quelques minutes)...")
panels <- vector("list", nrow(radar_samples))
for (i in seq_len(nrow(radar_samples))) {
  rr <- radar_samples[i]
  # Préfiltre rectangulaire, puis formule exacte utilisée dans le mémoire.
  delta <- rr$radius_km / earth_degree_km
  nearby <- acc[
    abs(latitude - rr$latitude) < delta & abs(longitude - rr$longitude) < delta
  ]
  if (nrow(nearby)) {
    nearby[, distance_km := sqrt(
      (latitude - rr$latitude)^2 + (longitude - rr$longitude)^2
    ) * earth_degree_km]
    counts <- nearby[distance_km < rr$radius_km,
                     .(accidents = .N, killed = sum(killed)), by = annee]
  } else {
    counts <- data.table(annee = integer(), accidents = integer(), killed = integer())
  }
  base <- data.table(annee = years)
  base <- counts[base, on = "annee"]
  base[is.na(accidents), `:=`(accidents = 0L, killed = 0L)]
  base[, `:=`(
    radius_km = rr$radius_km,
    id_radar = rr$id_radar,
    installation_year = rr$installation_year,
    installed = as.integer(annee > rr$installation_year)
  )]
  panels[[i]] <- base[annee != installation_year]
  if (i %% 100L == 0L) message("  radars-rayons traités : ", i, "/", nrow(radar_samples))
}
panel <- rbindlist(panels, use.names = TRUE)
setcolorder(panel, c("radius_km", "id_radar", "installation_year", "annee",
                     "installed", "accidents", "killed"))
fwrite(panel, file.path(out_dir, "panel_2013_2021.csv.gz"))

tidy_lm <- function(model, outcome, specification, radius) {
  co <- summary(model)$coefficients
  data.table(
    outcome = outcome,
    specification = specification,
    radius_km = radius,
    term = c("installed", "constant"),
    estimate = c(co["installed", "Estimate"], co["(Intercept)", "Estimate"]),
    std_error = c(co["installed", "Std. Error"], co["(Intercept)", "Std. Error"]),
    p_value = c(co["installed", "Pr(>|t|)"], co["(Intercept)", "Pr(>|t|)"]),
    n = nobs(model),
    r_squared = summary(model)$r.squared,
    adjusted_r_squared = summary(model)$adj.r.squared
  )
}

message("Estimation des modèles...")
estimates <- rbindlist(lapply(radii, function(radius) {
  x <- panel[radius_km == radius]
  rbindlist(lapply(c("accidents", "killed"), function(y) {
    rbindlist(list(
      tidy_lm(lm(reformulate("installed", y), data = x), y, "OLS", radius),
      tidy_lm(lm(reformulate(c("installed", "factor(annee)"), y), data = x),
              y, "year_FE", radius),
      tidy_lm(lm(reformulate(c("installed", "factor(annee)", "factor(id_radar)"), y),
                 data = x), y, "year_radar_FE", radius)
    ))
  }))
}))
fwrite(estimates, file.path(out_dir, "estimates_reconstructed.csv"))

# Valeurs publiées dans les Tables 2 à 7. La comparaison porte sur le coefficient
# Installed, qui est le paramètre d'intérêt du mémoire.
reference <- rbindlist(list(
  data.table(outcome="accidents", specification="year_FE", radius_km=radii,
             thesis_estimate=c(.462,1.012,1.573,-.685,-3.201),
             thesis_std_error=c(.284,.766,1.791,1.674,1.632)),
  data.table(outcome="killed", specification="year_FE", radius_km=radii,
             thesis_estimate=c(.003,-.022,-.020,-.003,.007),
             thesis_std_error=c(.010,.017,.029,.066,.102)),
  data.table(outcome="accidents", specification="year_radar_FE", radius_km=radii,
             thesis_estimate=c(-1.358,-2.690,-4.453,-2.736,-.217),
             thesis_std_error=c(.196,.514,1.177,.884,.758)),
  data.table(outcome="killed", specification="year_radar_FE", radius_km=radii,
             thesis_estimate=c(-.004,-.065,-.111,-.121,-.074),
             thesis_std_error=c(.012,.020,.033,.070,.104)),
  data.table(outcome="accidents", specification="OLS", radius_km=radii,
             thesis_estimate=c(1.283,2.857,4.236,3.062,2.046),
             thesis_std_error=c(.186,.500,1.154,1.047,1.018)),
  data.table(outcome="killed", specification="OLS", radius_km=radii,
             thesis_estimate=c(.012,.023,.064,.193,.265),
             thesis_std_error=c(.006,.011,.019,.042,.063))
))
comparison <- estimates[term == "installed"][reference,
  on = .(outcome, specification, radius_km)]
comparison[, `:=`(
  estimate_difference = estimate - thesis_estimate,
  std_error_difference = std_error - thesis_std_error
)]
fwrite(comparison, file.path(out_dir, "comparison_with_thesis.csv"))

expected_n <- data.table(radius_km = radii,
                         thesis_n = c(5080L, 4800L, 4248L, 2680L, 1920L))
diagnostics <- panel[, .(reconstructed_n = .N, radars = uniqueN(id_radar)),
                     by = radius_km][expected_n, on = "radius_km"]
diagnostics[, `:=`(
  expected_radars = thesis_n / 8,
  n_difference = reconstructed_n - thesis_n,
  coordinate_rule = "observed coordinates only",
  accidents_total = nrow(acc)
)]
fwrite(diagnostics, file.path(out_dir, "sample_diagnostics.csv"))

writeLines(capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))

message("Réplication terminée. Résultats dans : ", out_dir)
print(diagnostics)
