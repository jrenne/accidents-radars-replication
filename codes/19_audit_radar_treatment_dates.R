#!/usr/bin/env Rscript

# Audit the first-treatment date used by the camera event study.
#
# The harmonized radar table combines a community archive observed in October
# 2018 with official inventories published in October 2024 and December 2025.
# A later official commissioning date at a location already containing a point
# camera in 2018 is treated as a probable equipment replacement, not as a new
# site treatment. The audit keeps the raw dates, evidence flags, and a
# deliberately conservative confidence tier; it never overwrites source data.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
processed <- file.path(root, "data", "processed")
out_dir <- file.path(root, "results", "current")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

point_codes <- c("ETF", "ETD", "ETT", "ETU")
point_labels_2018 <- c("Radar fixe", "Radar discriminant")
archive_date <- as.IDate("2018-10-25")
agreement_tolerance_days <- 31L
close_match_threshold_m <- 50

radars <- fread(file.path(processed, "radars.csv.gz"))
inventory <- fread(file.path(processed, "radars_inventaires.csv.gz"))

official <- dcast(
  inventory,
  id_radar_officiel ~ millesime,
  value.var = c(
    "date_mise_en_service", "type_radar_code", "latitude", "longitude"
  )
)
setnames(
  official,
  old = c(
    "date_mise_en_service_2024-10", "date_mise_en_service_2025-12",
    "type_radar_code_2024-10", "type_radar_code_2025-12",
    "latitude_2024-10", "latitude_2025-12",
    "longitude_2024-10", "longitude_2025-12"
  ),
  new = c(
    "official_date_2024", "official_date_2025",
    "official_type_2024", "official_type_2025",
    "official_latitude_2024", "official_latitude_2025",
    "official_longitude_2024", "official_longitude_2025"
  ),
  skip_absent = TRUE
)

radars[, id_radar_2025 := trimws(as.character(id_radar_2025))]
radars[id_radar_2025 == "", id_radar_2025 := NA_character_]
radars <- merge(
  radars,
  official,
  by.x = "id_radar_2025",
  by.y = "id_radar_officiel",
  all.x = TRUE
)

radars[, `:=`(
  current_point = type_radar_code %chin% point_codes,
  archive_point = type_radar_libelle_2018 %chin% point_labels_2018,
  date_2025 = as.IDate(date_mise_en_service),
  date_2018 = as.IDate(date_installation_2018),
  present_official_2024 = !is.na(official_date_2024),
  present_official_2025 = !is.na(official_date_2025)
)]
radars <- radars[
  current_point == TRUE |
    (actif_liste_2025 == 0L & archive_point == TRUE)
]

radars[, official_date_stable :=
  present_official_2024 & present_official_2025 &
    official_date_2024 == official_date_2025]
radars[, official_type_stable :=
  present_official_2024 & present_official_2025 &
    official_type_2024 == official_type_2025]
radars[, date_gap_days := as.integer(date_2025 - date_2018)]
radars[, close_cross_inventory_match :=
  !is.na(distance_rapprochement_m) &
    distance_rapprochement_m <= close_match_threshold_m]
radars[, exact_date_agreement :=
  !is.na(date_gap_days) & abs(date_gap_days) <= agreement_tolerance_days]
radars[, probable_replacement :=
  !is.na(date_gap_days) & date_gap_days > 365L]
radars[, reverse_date_conflict :=
  !is.na(date_gap_days) & date_gap_days < -agreement_tolerance_days]

radars[, retained_installation_date := as.IDate(NA)]
radars[current_point == TRUE, retained_installation_date := date_2025]
radars[current_point == TRUE & archive_point == TRUE & !is.na(date_2018),
       retained_installation_date := fifelse(
         is.na(date_2025), date_2018, pmin(date_2025, date_2018)
       )]
radars[actif_liste_2025 == 0L & archive_point == TRUE,
       retained_installation_date := date_2018]

radars[, date_evidence := fcase(
  exact_date_agreement, "cross_source_date_agreement",
  probable_replacement, "earlier_2018_date_probable_replacement",
  reverse_date_conflict, "reverse_cross_source_conflict",
  !is.na(date_2018) & is.na(date_2025), "archive_2018_only",
  is.na(date_2018) & !is.na(date_2025), "official_only",
  default = "unclassified"
)]

# High confidence requires either direct cross-source agreement or a new site
# installed after the 2018 archive and stable in both official releases.
# Probable replacements are medium confidence when the cross-inventory match is
# within 50 m: the earlier presence is credible, but the exact first date still
# comes from the archive alone. Archive-only sites are also medium because they
# were observed in 2018 but have no recent official record. Wider geographic
# matches, reverse conflicts, and single-release official observations are low.
radars[, date_confidence := "low"]
radars[
  exact_date_agreement & close_cross_inventory_match,
  date_confidence := "high"
]
radars[
  date_evidence == "official_only" & date_2025 > archive_date &
    official_date_stable & official_type_stable,
  date_confidence := "high"
]
radars[
  date_confidence == "low" & probable_replacement &
    close_cross_inventory_match & official_date_stable,
  date_confidence := "medium"
]
radars[
  date_confidence == "low" & date_evidence == "archive_2018_only",
  date_confidence := "medium"
]
radars[
  date_confidence == "low" & date_evidence == "official_only" &
    official_date_stable,
  date_confidence := "medium"
]

radars[, confidence_reason := fcase(
  date_confidence == "high" & exact_date_agreement,
    "2018 and official dates agree; geographic match within 50 m",
  date_confidence == "high" & date_evidence == "official_only",
    "post-archive installation stable in both official releases",
  date_confidence == "medium" & probable_replacement,
    "earlier 2018 date at a close match; later official date suggests replacement",
  date_confidence == "medium" & date_evidence == "archive_2018_only",
    "date observed only in the 2018 archive",
  date_confidence == "medium" & date_evidence == "official_only",
    "date stable in official releases but absence from 2018 is unresolved",
  reverse_date_conflict,
    "official date predates the matched 2018 date",
  probable_replacement & !close_cross_inventory_match,
    "probable replacement but geographic match exceeds 50 m",
  !official_date_stable & present_official_2025,
    "official evidence available in only one retained release",
  default = "insufficient or conflicting evidence"
)]

audit_columns <- c(
  "cle_radar", "id_radar_2018", "id_radar_2025", "latitude", "longitude",
  "type_radar_libelle_2018", "type_radar_code", "date_2018", "date_2025",
  "retained_installation_date", "date_gap_days", "date_evidence",
  "date_confidence", "confidence_reason", "statut_rapprochement",
  "distance_rapprochement_m", "close_cross_inventory_match",
  "exact_date_agreement", "probable_replacement", "reverse_date_conflict",
  "present_official_2024", "present_official_2025",
  "official_date_stable", "official_type_stable", "route_2018",
  "source_principale"
)
audit <- radars[, ..audit_columns]
audit[, confidence_order := match(date_confidence, c("low", "medium", "high"))]
setorder(audit, confidence_order, date_evidence, cle_radar)
audit[, confidence_order := NULL]

summary <- audit[, .(
  sites = .N,
  retained_date_min = min(retained_installation_date, na.rm = TRUE),
  retained_date_max = max(retained_installation_date, na.rm = TRUE),
  median_match_distance_m = if (all(is.na(distance_rapprochement_m))) {
    NA_real_
  } else median(distance_rapprochement_m, na.rm = TRUE),
  probable_replacements = sum(probable_replacement, na.rm = TRUE),
  reverse_conflicts = sum(reverse_date_conflict, na.rm = TRUE)
), by = .(date_confidence, date_evidence)]
summary[, confidence_order := match(date_confidence, c("high", "medium", "low"))]
setorder(summary, confidence_order, -sites)
summary[, confidence_order := NULL]

fwrite(audit, file.path(out_dir, "radar_treatment_date_audit.csv"))
fwrite(summary, file.path(out_dir, "radar_treatment_date_audit_summary.csv"))
fwrite(
  audit[date_confidence != "high"],
  file.path(out_dir, "radar_treatment_date_manual_followup.csv")
)

message("Treatment-date confidence totals:")
print(audit[, .N, by = date_confidence][order(match(
  date_confidence, c("high", "medium", "low")
))])
message("Evidence by confidence tier:")
print(summary)
