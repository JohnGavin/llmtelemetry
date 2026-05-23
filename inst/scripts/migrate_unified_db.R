#!/usr/bin/env Rscript
# Migrate unified.duckdb — create roborev tables idempotently.
#
# Usage:
#   Rscript inst/scripts/migrate_unified_db.R
#   LLMTELEMETRY_UNIFIED_DB=/path/to/db.duckdb Rscript inst/scripts/migrate_unified_db.R
#
# This script creates the four roborev tables and sequences in the unified
# DuckDB database if they do not already exist. Safe to run multiple times.

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(cli)
})

# ---------------------------------------------------------------------------
# Resolve database path
# ---------------------------------------------------------------------------

db_path <- Sys.getenv(
  "LLMTELEMETRY_UNIFIED_DB",
  path.expand("~/.claude/logs/unified.duckdb")
)

dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

cli_inform("Using database: {.path {db_path}}")

# ---------------------------------------------------------------------------
# Connect and migrate
# ---------------------------------------------------------------------------

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

statements <- c(
  "CREATE SEQUENCE IF NOT EXISTS roborev_finding_seq",
  "CREATE SEQUENCE IF NOT EXISTS roborev_snapshot_seq",

  "CREATE TABLE IF NOT EXISTS roborev_reviews (
    job_id       INTEGER PRIMARY KEY,
    project      VARCHAR NOT NULL DEFAULT '',
    branch       VARCHAR,
    commit_sha   VARCHAR NOT NULL DEFAULT '',
    reviewed_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    agent        VARCHAR,
    verdict      VARCHAR,
    raw_show     VARCHAR,
    scraped_at   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )",

  "CREATE TABLE IF NOT EXISTS roborev_findings (
    finding_id    INTEGER PRIMARY KEY,
    job_id        INTEGER NOT NULL,
    severity      VARCHAR NOT NULL,
    primary_file  VARCHAR,
    full_location VARCHAR,
    problem_text  VARCHAR,
    fix_text      VARCHAR,
    content_hash  VARCHAR NOT NULL,
    scraped_at    TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )",

  "CREATE TABLE IF NOT EXISTS roborev_loops (
    content_hash         VARCHAR PRIMARY KEY,
    severity             VARCHAR,
    primary_file         VARCHAR,
    summary              VARCHAR,
    first_seen           TIMESTAMP,
    last_seen            TIMESTAMP,
    cycles               INTEGER,
    tier                 VARCHAR,
    fix_commit_shas      VARCHAR,
    estimated_wasted_usd DOUBLE,
    ack_by               VARCHAR,
    ack_at               TIMESTAMP,
    ack_reason           VARCHAR,
    ack_until            TIMESTAMP,
    updated_at           TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )",

  "CREATE TABLE IF NOT EXISTS roborev_snapshot_log (
    run_id       INTEGER PRIMARY KEY,
    started_at   TIMESTAMP NOT NULL,
    ended_at     TIMESTAMP,
    reviews_seen INTEGER,
    new_findings INTEGER,
    loops_active INTEGER,
    status       VARCHAR
  )"
)

for (stmt in statements) {
  DBI::dbExecute(con, stmt)
}

# Verify tables were created
tables <- DBI::dbListTables(con)
roborev_tables <- grep("^roborev_", tables, value = TRUE)

cli_inform(
  "Created/verified {length(roborev_tables)} roborev table(s): {paste(roborev_tables, collapse = ', ')}"
)
cli_inform("Migration complete.")
