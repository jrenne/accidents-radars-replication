#!/usr/bin/env Rscript

# Pipeline used by main.R. Run this file from the project root only.

check_only <- identical(Sys.getenv("REPLICATION_CHECK_ONLY", "0"), "1")

root <- normalizePath(Sys.getenv("REPLICATION_ROOT", "."), mustWork = TRUE)
mode <- match.arg(Sys.getenv("REPLICATION_MODE", "main"), c("main", "full"))
rebuild_spatial <- identical(Sys.getenv("REBUILD_SPATIAL_INPUTS", "0"), "1")
bootstrap_repetitions <- as.integer(Sys.getenv("DR_BOOTSTRAP_REPETITIONS", "1999"))
if (!is.finite(bootstrap_repetitions) || bootstrap_repetitions < 2L) {
  stop("DR_BOOTSTRAP_REPETITIONS must be an integer of at least 2.")
}
analysis_dir <- file.path(root, "codes")
output_dir <- file.path(root, "results", "current")
log_dir <- file.path(output_dir, "replication_logs")
python_engine <- Sys.getenv("REPLICATION_PYTHON", "python3")
local_python_lib <- file.path(root, ".python")
python_env <- if (dir.exists(local_python_lib)) {
  paste0("PYTHONPATH=", local_python_lib)
} else character()

required_packages <- c(
  "curl", "data.table", "ggplot2", "glmnet", "igraph", "jsonlite", "patchwork"
)
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)
)]
if (length(missing_packages)) {
  stop("Missing R package(s): ", paste(missing_packages, collapse = ", "))
}

required_inputs <- c(
  "data/processed/accidents.csv.gz",
  "data/processed/radars.csv.gz",
  "data/processed/radars_inventaires.csv.gz",
  "data/processed/usagers.csv.gz",
  "data/auxiliary/result.radar.commune.rda",
  "data/auxiliary/DEPARTEMENT/DEPARTEMENT.shp",
  "data/roads/radar_ign_road_matches.csv",
  "data/derived/road_corridor_monthly_counts_network_100m.csv.gz"
)
if (mode == "full") {
  required_inputs <- c(required_inputs, "data/traffic/README.md")
}
missing_inputs <- required_inputs[!file.exists(file.path(root, required_inputs))]
if (length(missing_inputs)) {
  stop("Missing required input(s): ", paste(missing_inputs, collapse = ", "))
}

step <- function(label, script, env = character(), engine = "Rscript",
                 main = FALSE, spatial = FALSE) {
  list(label = label, script = script, env = env, engine = engine,
       main = main, spatial = spatial)
}

# Defaults are the baseline specification; each check lists only its changes.
estimate <- function(label, width = 100, radii = "2", horizon = 12,
                     reach = "0.75", method = "network", env = character(),
                     main = FALSE) {
  step(label, "13_doubly_robust_road_corridor_did.R", c(
    paste0("DR_BOOTSTRAP_REPETITIONS=", bootstrap_repetitions),
    paste0("DR_BALANCED_EVENT_HORIZON=", horizon),
    paste0("DR_MIN_NETWORK_REACH_RATIO=", reach),
    paste0("DR_RADII=", radii),
    paste0("CORRIDOR_METHOD=", method),
    paste0("CORRIDOR_HALF_WIDTH_M=", width), env
  ), main = main)
}

steps <- list(
  step("Match cameras to IGN roads", "10_match_radars_to_ign_roads.R", spatial = TRUE),
  step("Audit treatment dates", "19_audit_radar_treatment_dates.R", main = TRUE),
  step("Build 50 m network corridors", "11_monthly_road_corridor_did.R",
       c("CORRIDOR_METHOD=network", "CORRIDOR_HALF_WIDTH_M=50"), spatial = TRUE),
  step("Build 100 m network corridors", "11_monthly_road_corridor_did.R",
       c("CORRIDOR_METHOD=network", "CORRIDOR_HALF_WIDTH_M=100"), spatial = TRUE),
  step("Build 100 m Euclidean road corridors", "11_monthly_road_corridor_did.R",
       c("CORRIDOR_METHOD=euclidean", "CORRIDOR_HALF_WIDTH_M=100"), spatial = TRUE),
  step("Build circular outcomes", "15_build_euclidean_circle_counts.R", spatial = TRUE),
  step("Build off-road negative control", "17_build_offroad_placebo_counts.R", spatial = TRUE),
  step("Build corridors including large cities", "11_monthly_road_corridor_did.R",
       c("CORRIDOR_METHOD=network", "CORRIDOR_HALF_WIDTH_M=100",
         "EXCLUDE_LARGE_CITIES=0"), spatial = TRUE),

  estimate("Estimate stacked DiD baseline, 50 m", width = 50, radii = "1,2,3", main = TRUE),
  estimate("Estimate stacked DiD baseline, 100 m", radii = "1,2,3", main = TRUE),
  estimate("Estimate doubly robust robustness check", radii = "1,2,3",
           env = "PRIMARY_ESTIMATOR=doubly_robust", main = TRUE),
  estimate("Estimate balanced dynamics, 50 m", width = 50, horizon = 24, main = TRUE),
  estimate("Estimate balanced dynamics, 100 m", horizon = 24, main = TRUE),
  estimate("Estimate common-radius sample", radii = "1,2,3",
           env = "DR_COMMON_RADIUS_SAMPLE=1", main = TRUE),

  estimate("Estimate circular outcome", method = "circle"),
  estimate("Estimate Euclidean road corridor", method = "euclidean"),
  estimate("Estimate off-road negative control", method = "offroad"),

  estimate("Estimate 50 percent graph threshold", reach = "0.50"),
  estimate("Estimate 90 percent graph threshold", reach = "0.90"),
  estimate("Estimate sample including large cities", env = "DR_EXCLUDE_LARGE_CITIES=0"),
  estimate("Estimate exact-road-match sample", env = "DR_ROAD_MATCH=exact"),
  estimate("Estimate binary outcome", env = "DR_OUTCOME_SCALE=binary"),
  estimate("Estimate high-confidence dates", env = "DR_DATE_CONFIDENCE=high"),
  estimate("Estimate high/medium-confidence dates", env = "DR_DATE_CONFIDENCE=high_medium"),

  step("Validate doubly robust score", "18_validate_local_dr_score.R", main = TRUE),
  step("Plot main event study", "14_plot_network_dr_results.R", main = TRUE),
  step("Compare spatial outcomes", "16_compare_spatial_measurement.R"),
  step("Map and describe baseline sample", "20_map_and_describe_baseline_sample.R", main = TRUE),
  step("Adjust primary family", "21_adjust_primary_family.R", main = TRUE),
  step("Plot baseline balance", "22_plot_baseline_balance.R", main = TRUE),
  step("Plot leave-one-cohort-out estimates", "23_plot_leave_one_cohort_out.R", main = TRUE),
  step("Summarize graph thresholds", "24_graph_completeness_sensitivity.R"),
  step("Summarize large-city sensitivity", "25_large_city_sensitivity.R"),
  step("Summarize exact-road sensitivity", "26_exact_road_match_sensitivity.R"),
  step("Summarize placebos", "27_placebo_summary.R"),
  step("Summarize binary outcome", "28_binary_outcome_sensitivity.R"),

  step("Match TMJA endpoint approximation", "29_build_tmja_radar_matches.R"),
  step("Match full TMJA polylines", "29b_match_tmja_shapefiles.py", engine = "python3"),
  estimate("Estimate TMJA sensitivity, 50 m",
           env = c("DR_TRAFFIC_ADJUSTMENT=1", "DR_TRAFFIC_MATCH_DISTANCE_M=50")),
  estimate("Estimate TMJA sensitivity, 100 m",
           env = c("DR_TRAFFIC_ADJUSTMENT=1", "DR_TRAFFIC_MATCH_DISTANCE_M=100")),
  step("Summarize TMJA sensitivity", "30_tmja_sensitivity_summary.R")
)

steps <- Filter(function(x) {
  (rebuild_spatial || !x$spatial) && (mode == "full" || x$main)
}, steps)

script_paths <- file.path(analysis_dir, vapply(steps, `[[`, "", "script"))
missing_scripts <- unique(script_paths[!file.exists(script_paths)])
if (length(missing_scripts)) {
  stop("Missing replication script(s): ", paste(missing_scripts, collapse = ", "))
}

if (mode == "full") {
  python_check <- system2(
    python_engine,
    c("-c", shQuote("import shapefile; assert shapefile.__version__ == '2.3.1'")),
    stdout = FALSE, stderr = FALSE, env = python_env
  )
  if (!identical(python_check, 0L)) {
    stop("Python package pyshp==2.3.1 is required for the full TMJA robustness check.")
  }
}

message("Preflight passed for mode '", mode, "': ", length(steps),
        " ordered steps are available.")
if (!check_only) {
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  started <- Sys.time()
  for (i in seq_along(steps)) {
    x <- steps[[i]]
    slug <- tolower(gsub("[^A-Za-z0-9]+", "_", x$label))
    log_file <- file.path(log_dir, sprintf("%02d_%s.log", i, slug))
    message(sprintf("[%02d/%02d] %s", i, length(steps), x$label))
    status <- system2(
      if (x$engine == "python3") python_engine else x$engine,
      file.path("codes", x$script),
      stdout = log_file,
      stderr = log_file,
      env = c(if (x$engine == "python3") python_env else character(), x$env)
    )
    if (!identical(status, 0L)) {
      stop("Replication failed at step ", i, " (", x$label,
           "). See ", log_file, call. = FALSE)
    }
  }

  elapsed <- difftime(Sys.time(), started, units = "mins")
  message("Article replication completed in ", round(as.numeric(elapsed), 1), " minutes.")
}
