#!/usr/bin/env Rscript
# backfill_historical_data.R
# One-shot script to backfill historical data from llm repo into llmtelemetry.
#
# Recovers:
#   - 16 ccusage rows (2025-12-25 to 2026-01-09) missing from llm_usage_history.duckdb
#   - Entire gemini_usage.duckdb (29 daily + 30 session rows)
#
# Uses INSERT OR IGNORE for ccusage so existing telemetry rows (with fresher
# collected_at timestamps) are preserved for overlapping keys.
#
# Uses ATTACH to avoid DuckDB read-only mode issues with dbConnect().

library(DBI)
library(duckdb)

# --- Paths ---
old_ccusage_path <- "/tmp/llm_history_old.duckdb"
old_gemini_path <- "/tmp/gemini_old.duckdb"
telemetry_ccusage_path <- "inst/extdata/llm_usage_history.duckdb"
telemetry_gemini_path <- "inst/extdata/gemini_usage.duckdb"

stopifnot(
  "Old ccusage file not found" = file.exists(old_ccusage_path),
  "Old gemini file not found" = file.exists(old_gemini_path),
  "Telemetry ccusage file not found" = file.exists(telemetry_ccusage_path)
)

# ============================================================================
# Part 1: Backfill ccusage history
# ============================================================================
cli::cli_h1("Backfilling ccusage history")

con <- dbConnect(duckdb())
dbExecute(con, sprintf("ATTACH '%s' AS tel", telemetry_ccusage_path))
dbExecute(con, sprintf("ATTACH '%s' AS old_db (READ_ONLY)", old_ccusage_path))

old_count <- dbGetQuery(con, "SELECT count(*) as n FROM old_db.daily_usage")$n
old_range <- dbGetQuery(con, "SELECT min(date) as min_d, max(date) as max_d FROM old_db.daily_usage")
cli::cli_alert_info("Old data: {old_count} rows, {old_range$min_d} to {old_range$max_d}")

before_count <- dbGetQuery(con, "SELECT count(*) as n FROM tel.daily_usage")$n
before_range <- dbGetQuery(con, "SELECT min(date) as min_d, max(date) as max_d FROM tel.daily_usage")
cli::cli_alert_info("Telemetry before: {before_count} rows, {before_range$min_d} to {before_range$max_d}")

inserted <- dbExecute(con, "INSERT OR IGNORE INTO tel.daily_usage SELECT * FROM old_db.daily_usage")

after_count <- dbGetQuery(con, "SELECT count(*) as n FROM tel.daily_usage")$n
after_range <- dbGetQuery(con, "SELECT min(date) as min_d, max(date) as max_d FROM tel.daily_usage")
cli::cli_alert_success("Inserted {inserted} new rows")
cli::cli_alert_success("Telemetry after: {after_count} rows, {after_range$min_d} to {after_range$max_d}")

dbExecute(con, "DETACH old_db")
dbExecute(con, "DETACH tel")
dbDisconnect(con)

# ============================================================================
# Part 2: Copy Gemini data
# ============================================================================
cli::cli_h1("Copying Gemini history")

con <- dbConnect(duckdb())
dbExecute(con, sprintf("ATTACH '%s' AS gem", telemetry_gemini_path))
dbExecute(con, sprintf("ATTACH '%s' AS old_gem (READ_ONLY)", old_gemini_path))

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS gem.daily_usage (
    date DATE PRIMARY KEY,
    total_tokens BIGINT,
    total_cost DOUBLE,
    message_count INTEGER
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS gem.sessions_summary (
    sessionId VARCHAR PRIMARY KEY,
    title VARCHAR,
    project VARCHAR,
    total_tokens BIGINT,
    total_cost DOUBLE,
    start_time TIMESTAMP,
    last_updated TIMESTAMP
  )
")

n_daily <- dbExecute(con, "INSERT OR IGNORE INTO gem.daily_usage SELECT * FROM old_gem.daily_usage")
n_sessions <- dbExecute(con, "INSERT OR IGNORE INTO gem.sessions_summary SELECT * FROM old_gem.sessions_summary")

gem_range <- dbGetQuery(con, "SELECT min(date) as min_d, max(date) as max_d FROM gem.daily_usage")
cli::cli_alert_success("Gemini daily_usage: {n_daily} rows inserted")
cli::cli_alert_success("Gemini sessions_summary: {n_sessions} rows inserted")
cli::cli_alert_success("Gemini date range: {gem_range$min_d} to {gem_range$max_d}")

dbExecute(con, "DETACH old_gem")
dbExecute(con, "DETACH gem")
dbDisconnect(con)

cli::cli_h1("Backfill complete")
