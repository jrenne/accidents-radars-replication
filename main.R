#!/usr/bin/env Rscript

# One entry point for the replication package.
# Examples:
#   source("main.R")                                  # quick, cached results
#   Rscript main.R --mode=main                       # preferred estimates
#   Rscript main.R --mode=full                       # all robustness checks
#   Rscript main.R --mode=full --rebuild-spatial     # refresh IGN-derived inputs

script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) return(normalizePath(sub("^--file=", "", file_arg[[1]])))
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    candidate <- frames[[i]]$ofile
    if (!is.null(candidate)) return(normalizePath(candidate))
  }
  normalizePath("main.R")
}

root <- dirname(script_path())
setwd(root)
args <- commandArgs(trailingOnly = TRUE)
value_arg <- function(prefix, default = NULL) {
  hit <- grep(paste0("^", prefix, "="), args, value = TRUE)
  if (!length(hit)) default else sub(paste0("^", prefix, "="), "", hit[[1]])
}

mode <- match.arg(value_arg("--mode", "quick"), c("quick", "main", "full"))
check_only <- "--check-only" %in% args
prepare_only <- "--prepare-only" %in% args
rebuild_spatial <- "--rebuild-spatial" %in% args
bootstrap <- as.integer(value_arg("--bootstrap", "1999"))
known <- grepl("^--mode=|^--bootstrap=", args) |
  args %in% c("--check-only", "--prepare-only", "--rebuild-spatial")
if (any(!known)) stop("Unknown argument(s): ", paste(args[!known], collapse = ", "))
if (!is.finite(bootstrap) || bootstrap < 2L) stop("--bootstrap must be at least 2.")

dir.create(file.path(root, "results", "current"), recursive = TRUE, showWarnings = FALSE)

if (mode == "quick") {
  cached_manifest <- read.csv(
    file.path(root, "config", "cached_results_md5.csv"),
    stringsAsFactors = FALSE
  )
  cached <- file.path(root, "results", "cached", cached_manifest$file)
  if (any(!file.exists(cached))) stop("One or more cached results are missing.")
  observed_md5 <- unname(tools::md5sum(cached))
  if (!identical(observed_md5, cached_manifest$md5)) {
    stop("One or more cached results failed checksum verification.")
  }
  if (!check_only) {
    ok <- file.copy(cached, file.path(root, "results", "current"), overwrite = TRUE)
    if (!all(ok)) stop("Some cached results could not be restored.")
  }
  message("Quick mode ready: ", length(cached), " archived result files verified.")
  message("Use --mode=main to re-estimate the preferred specifications, or ",
          "--mode=full for every robustness check.")
} else {
  Sys.setenv(
    REPLICATION_ROOT = root,
    REPLICATION_MODE = mode,
    REPLICATION_CHECK_ONLY = if (check_only) "1" else "0",
    REBUILD_SPATIAL_INPUTS = if (rebuild_spatial) "1" else "0",
    DR_BOOTSTRAP_REPETITIONS = bootstrap
  )
  source(file.path(root, "codes", "00_prepare_data.R"), local = new.env())
  if (!prepare_only) {
    source(file.path(root, "codes", "run_article_replication.R"), local = .GlobalEnv)
  }
}
