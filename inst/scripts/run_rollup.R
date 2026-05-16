#!/usr/bin/env Rscript
# Phase 1A/B/C rollup runner — backfills all three v1 parquets from source JSON.
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

extdata  <- file.path(package_dir, "inst", "extdata")
telv1    <- file.path(extdata, "telemetry", "v1")

# --- Sessions (Phase 1A) -----------------------------------------------------
n_sess <- nrow(rollup_sessions(
  input_path  = file.path(extdata, "unified_sessions.json"),
  output_path = file.path(telv1,   "sessions.parquet")
))

# --- Costs (Phase 1B) --------------------------------------------------------
n_costs <- nrow(rollup_costs(
  input_path  = file.path(extdata, "cost_by_project_estimated.json"),
  output_path = file.path(telv1,   "costs.parquet")
))

# --- Git commits (Phase 1C) --------------------------------------------------
n_git <- nrow(rollup_git_commits(
  input_path  = file.path(extdata, "git_commits_by_project.json"),
  output_path = file.path(telv1,   "git_commits.parquet")
))

cat(sprintf("rollup: sessions=%d costs=%d git_commits=%d\n", n_sess, n_costs, n_git))
