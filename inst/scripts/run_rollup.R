#!/usr/bin/env Rscript
# Phase 1A/B/C/E/F rollup runner — backfills all three v1 parquets from source
# JSON, then drains the staging directory for all three tables.
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

# Privacy exclusion — single source of truth (#265).
# excluded_dashboard_projects() is now exported from R/privacy_exclusion.R.
# The rollup_* functions call drop_confidential_projects() internally (#83),
# which uses confidential_project_names().  The full dashboard exclusion list
# (including demo projects) is available here for any future post-rollup filter.
local({
  helper <- file.path(package_dir, "R", "privacy_exclusion.R")
  if (file.exists(helper) && !exists("excluded_dashboard_projects", mode = "function")) {
    source(helper, local = FALSE)
  }
})

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

# --- Staging drain (Phase 1E/F) ----------------------------------------------
# Append any hook-emitted events that have not yet been written to the parquets.
# Each appender is idempotent: re-running adds 0 rows for events already present.
n_new_sess <- append_sessions_from_staging(
  parquet_path = file.path(telv1, "sessions.parquet")
)

n_new_costs <- append_costs_from_staging(
  parquet_path = file.path(telv1, "costs.parquet")
)

n_new_git <- append_git_commits_from_staging(
  parquet_path = file.path(telv1, "git_commits.parquet")
)

cat(sprintf(
  "rollup: sessions=%d (+%d) costs=%d (+%d) git_commits=%d (+%d)\n",
  n_sess, n_new_sess, n_costs, n_new_costs, n_git, n_new_git
))

# --- Refresh costs in unified.duckdb (Phase #309) ----------------------------
# The costs table in unified.duckdb (opus/sonnet/haiku daily breakdown produced
# by cmonitor-rs) stopped updating 2026-04-21 because refresh_costs_from_jsonl.R
# (in the llm repo) had no scheduled job.  run_refresh_costs_unified.R is the
# llmtelemetry-side replacement: it calls cmonitor-rs directly and upserts
# fresh rows.  Fail-open: exits 0 when cmonitor-rs is absent so this does not
# break CI or non-macOS environments.  See llmtelemetry#309.
refresh_costs_script <- file.path(dirname(script_path), "run_refresh_costs_unified.R")
if (file.exists(refresh_costs_script)) {
  cat("Refreshing unified.duckdb costs table via cmonitor-rs (#309)...\n")
  rc <- tryCatch({
    sys.source(refresh_costs_script, envir = new.env(parent = baseenv()))
    0L
  }, error = function(e) {
    message("run_rollup: costs refresh failed (non-fatal): ", e$message)
    1L
  })
  if (rc == 0L) cat("run_rollup: unified.duckdb costs refresh complete\n")
}
