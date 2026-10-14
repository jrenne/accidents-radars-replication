#!/usr/bin/env Rscript

required <- c(
  "curl", "data.table", "digest", "ggplot2", "glmnet",
  "igraph", "jsonlite", "patchwork"
)
missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) install.packages(missing)
message("All required R packages are installed.")
