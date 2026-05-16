#!/usr/bin/env Rscript
# Phase 1A rollup runner — backfills sessions from unified_sessions.json
# into inst/extdata/telemetry/v1/sessions.parquet.
#
# Usage (from any working directory):
#   Rscript /path/to/llmtelemetry/inst/scripts/run_rollup.R

# Locate the package root from the --file argument passed by Rscript
args        <- commandArgs(trailingOnly = FALSE)
file_arg    <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
} else {
  stop("Cannot determine script path. Run via: Rscript /path/to/run_rollup.R")
}
package_dir <- normalizePath(file.path(dirname(script_path), "..", ".."),
                             mustWork = FALSE)

pkgload::load_all(package_dir, quiet = TRUE)

output_path <- file.path(
  package_dir, "inst", "extdata", "telemetry", "v1", "sessions.parquet"
)
input_path <- file.path(
  package_dir, "inst", "extdata", "unified_sessions.json"
)

result <- rollup_sessions(
  input_path  = input_path,
  output_path = output_path
)

cat(sprintf(
  "rollup_sessions: wrote %d rows to %s\n",
  nrow(result),
  output_path
))
