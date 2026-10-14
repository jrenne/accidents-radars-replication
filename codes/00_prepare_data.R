#!/usr/bin/env Rscript

# Download versioned inputs from jrenne/accidents-radars-france. A sibling
# checkout is used when present, which is convenient for package development.

root <- normalizePath(Sys.getenv("REPLICATION_ROOT", "."), mustWork = TRUE)
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required to verify downloaded inputs.")
}

manifest <- read.csv(
  file.path(root, "config", "data_manifest.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
processed_dir <- file.path(root, "data", "processed")
downloads_dir <- file.path(root, "data", "downloads")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(downloads_dir, recursive = TRUE, showWarnings = FALSE)

sha256 <- function(path) digest::digest(path, algo = "sha256", file = TRUE)
verified <- function(path, expected) file.exists(path) && identical(sha256(path), expected)
data_repo <- Sys.getenv(
  "ACCIDENTS_DATA_REPO",
  file.path(dirname(root), "accidents-radars-france")
)

for (i in seq_len(nrow(manifest))) {
  row <- manifest[i, ]
  target <- file.path(processed_dir, row$file)
  if (verified(target, row$sha256)) next
  local_source <- file.path(data_repo, "data", "processed", row$file)
  temporary <- paste0(target, ".download")
  if (file.exists(local_source)) {
    if (!file.copy(local_source, temporary, overwrite = TRUE)) {
      stop("Could not copy local input: ", local_source)
    }
  } else {
    message("Downloading ", row$file, " from the data repository...")
    download.file(row$url, temporary, mode = "wb", quiet = FALSE)
  }
  if (!verified(temporary, row$sha256)) {
    unlink(temporary)
    stop("SHA-256 verification failed for ", row$file)
  }
  if (!file.rename(temporary, target)) stop("Could not install ", target)
}

bundle_name <- "accidents_radars_replication_inputs-v1.1.0.tar.gz"
bundle_sha <- "1011a15b8df6970854e66b320e84a30defbac9580799a035805e4bbb094c62d4"
bundle_target <- file.path(downloads_dir, bundle_name)
bundle_local <- Sys.getenv(
  "REPLICATION_INPUT_BUNDLE",
  file.path(data_repo, "release", bundle_name)
)
bundle_url <- paste0(
  "https://github.com/jrenne/accidents-radars-france/releases/download/v1.1.0/",
  bundle_name
)
sentinel <- file.path(root, "data", paste0(".", bundle_sha))
if (!file.exists(sentinel)) {
  if (!verified(bundle_target, bundle_sha)) {
    temporary <- paste0(bundle_target, ".download")
    if (file.exists(bundle_local)) {
      if (!file.copy(bundle_local, temporary, overwrite = TRUE)) {
        stop("Could not copy local replication-input bundle.")
      }
    } else {
      message("Downloading the versioned auxiliary-input bundle...")
      download.file(bundle_url, temporary, mode = "wb", quiet = FALSE)
    }
    if (!verified(temporary, bundle_sha)) {
      unlink(temporary)
      stop("SHA-256 verification failed for the auxiliary-input bundle.")
    }
    if (!file.rename(temporary, bundle_target)) stop("Could not install input bundle.")
  }
  utils::untar(bundle_target, exdir = file.path(root, "data"))
  writeLines(bundle_sha, sentinel)
}

# Seed immutable spatial matches and derived monthly counts. They are rebuilt
# only when rebuild_spatial_inputs is selected in main.R.
result_dir <- file.path(root, "results", "current")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
seed_files <- c(
  list.files(file.path(root, "data", "roads"), pattern = "\\.csv$", full.names = TRUE),
  list.files(file.path(root, "data", "derived"), full.names = TRUE)
)
for (source in seed_files) {
  target <- file.path(result_dir, basename(source))
  if (!file.exists(target) && !file.copy(source, target)) {
    stop("Could not seed derived input: ", basename(source))
  }
}
message("Data preparation complete; all inputs passed SHA-256 verification.")
