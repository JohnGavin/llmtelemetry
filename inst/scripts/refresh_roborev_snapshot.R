#!/usr/bin/env Rscript
# Refresh roborev snapshot — standalone CLI script.
#
# Usage:
#   Rscript inst/scripts/refresh_roborev_snapshot.R [--repo <path>]
#   LLMTELEMETRY_UNIFIED_DB=/path/to/db.duckdb Rscript inst/scripts/refresh_roborev_snapshot.R
#
# Steps:
#   1. Migrate unified.duckdb (create tables idempotently)
#   2. Scrape all roborev findings for this repository
#   3. Upsert into unified.duckdb
#   4. Print summary

suppressPackageStartupMessages({
  library(llmtelemetry)
  library(cli)
  library(here)
})

# ---------------------------------------------------------------------------
# Parse arguments
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

repo_path <- here::here()

i <- 1L
while (i <= length(args)) {
  if (args[i] == "--repo" && i < length(args)) {
    repo_path <- args[i + 1L]
    i <- i + 2L
  } else {
    i <- i + 1L
  }
}

db_path <- Sys.getenv(
  "LLMTELEMETRY_UNIFIED_DB",
  path.expand("~/.claude/logs/unified.duckdb")
)

cli_h1("Roborev Snapshot Refresh")
cli_inform("Repository: {.path {repo_path}}")
cli_inform("Database:   {.path {db_path}}")

# ---------------------------------------------------------------------------
# Step 1: Migrate schema (idempotent)
# ---------------------------------------------------------------------------

cli_h2("Step 1: Migrate schema")
migrate_unified_db(db_path = db_path)

# ---------------------------------------------------------------------------
# Step 2: Scrape findings
# ---------------------------------------------------------------------------

cli_h2("Step 2: Scrape roborev findings")
findings <- scrape_roborev(repo_path = repo_path, require_bin = TRUE)
cli_inform("Scraped {nrow(findings)} finding(s) from {dplyr::n_distinct(findings$job_id)} review(s)")

# ---------------------------------------------------------------------------
# Step 3: Upsert
# ---------------------------------------------------------------------------

cli_h2("Step 3: Upsert into {.path {db_path}}")
summary_tbl <- upsert_roborev_findings(findings, db_path = db_path)

# ---------------------------------------------------------------------------
# Step 4: Print summary
# ---------------------------------------------------------------------------

cli_h2("Summary")
cli_inform("Reviews seen:  {summary_tbl$reviews_seen}")
cli_inform("New findings:  {summary_tbl$new_findings}")
cli_inform("New reviews:   {summary_tbl$new_reviews}")
cli_inform("Done.")
