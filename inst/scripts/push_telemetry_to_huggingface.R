#!/usr/bin/env Rscript
# push_telemetry_to_huggingface.R — Archive sanitised telemetry to HuggingFace
#
# PURPOSE
# -------
# Archives the SANITISED v1 PIT parquet files (sessions.parquet,
# costs.parquet) to a HuggingFace dataset repository.  All logic lives in
# R/hf_push.R; this script is the CLI entry point.
#
# DEFAULT BEHAVIOUR: DRY-RUN
# --------------------------
# By default this script validates the privacy guard, prints the file
# manifest and target repository, and exits WITHOUT touching the network.
# Pass --push to perform a real archive.
#
# USAGE
# -----
#   # Dry-run (default — safe to call from CI or interactively):
#   Rscript inst/scripts/push_telemetry_to_huggingface.R
#
#   # Live push (human operator or Phase F scheduled CI only):
#   Rscript inst/scripts/push_telemetry_to_huggingface.R --push
#
#   # Specify a different repo:
#   HF_DATASET_REPO=JohnGavin/llmtelemetry-dev \
#     Rscript inst/scripts/push_telemetry_to_huggingface.R
#
# ONE-TIME SETUP (human operator)
# --------------------------------
# 1. Create the HuggingFace dataset repository:
#      https://huggingface.co/new-dataset
#    Suggested name: JohnGavin/llmtelemetry-metrics
#    Set visibility to Private (public only after privacy review).
#
# 2. Authenticate:
#      huggingface-cli login
#    This writes a token to ~/.cache/huggingface/token and configures git
#    to use it via a credential helper.  Run this once on any machine
#    that will push.
#
# 3. Verify access:
#      git ls-remote https://huggingface.co/datasets/JohnGavin/llmtelemetry-metrics
#    You should see refs without a password prompt.
#
# CI TOKEN SUPPLY (Phase F — deferred)
# -------------------------------------
# In GitHub Actions, write the HF_TOKEN secret into the expected location
# before calling this script:
#
#   - run: |
#       mkdir -p ~/.cache/huggingface
#       echo "$HF_TOKEN" > ~/.cache/huggingface/token
#       chmod 600 ~/.cache/huggingface/token
#     env:
#       HF_TOKEN: ${{ secrets.HF_TOKEN }}
#
# The script uses a GIT_ASKPASS helper that reads the token file at
# git-clone/push time.  The token is NEVER embedded in a URL or loaded into
# an R variable — it flows only from the file to git's stdin via the helper.
#
# ARCHIVE SCOPE
# -------------
# Only session metadata and cost aggregates are archived.  Excluded:
#   - Raw conversation logs
#   - PHI
#   - working_dir columns (absolute filesystem paths)
#
# A mandatory privacy guard asserts ZERO confidential rows (drop_confidential_
# projects()) before any file is staged.  The guard aborts with a clear error
# if a confidential row is detected; the parquet must be regenerated.
#
# DEFERRED PHASES
# ---------------
# Phase E: dashboard hf:// read via DuckDB (not implemented here)
# Phase F: scheduled CI push via GitHub Actions cron (not implemented here)
#
# See: issue #83

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
do_push <- "--push" %in% args

# ---------------------------------------------------------------------------
# Load the package (prefers working-tree dev load, falls back to installed)
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  if (file.exists(here::here("DESCRIPTION")) && file.exists(here::here("R"))) {
    pkgload::load_all(here::here(), quiet = TRUE)
  } else {
    library(llmtelemetry)
  }
})

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------

result <- hf_push_telemetry(push = do_push)

if (isTRUE(result$dry_run)) {
  message("Dry-run complete.  Pass --push to perform a live archive.")
} else {
  message("Archive complete: ", result$sessions_rows, " session rows, ",
          result$costs_rows, " cost rows pushed to ", result$hf_repo)
}
