# Replication options ---------------------------------------------------------

mode <- "quick"                 # "quick", "main", or "full"
bootstrap_repetitions <- 1999L  # use a small value for a test run
rebuild_spatial_inputs <- FALSE # TRUE rebuilds spatial inputs in "full" mode

# Run -------------------------------------------------------------------------

stopifnot(
  mode %in% c("quick", "main", "full"),
  bootstrap_repetitions >= 2L
)

root <- normalizePath(".", mustWork = TRUE)
if (!file.exists(file.path(root, "accidents-radars-replication.Rproj"))) {
  stop("Open the R project and run main.R from the project root.")
}

if (mode == "quick") {
  manifest <- read.csv(file.path(root, "config", "cached_results_md5.csv"))
  cached <- file.path(root, "results", "cached", manifest$file)
  if (any(!file.exists(cached)) ||
      any(unname(tools::md5sum(cached)) != manifest$md5)) {
    stop("The archived results are incomplete or have been modified.")
  }
  current <- file.path(root, "results", "current")
  dir.create(current, recursive = TRUE, showWarnings = FALSE)
  if (!all(file.copy(cached, current, overwrite = TRUE))) {
    stop("Could not restore the archived results.")
  }
  message("Archived paper results restored in results/current.")
} else {
  Sys.setenv(
    REPLICATION_ROOT = root,
    REPLICATION_MODE = mode,
    REPLICATION_CHECK_ONLY = "0",
    REBUILD_SPATIAL_INPUTS = as.integer(rebuild_spatial_inputs),
    DR_BOOTSTRAP_REPETITIONS = bootstrap_repetitions
  )
  source(file.path(root, "codes", "00_prepare_data.R"))
  source(file.path(root, "codes", "run_article_replication.R"))
}
