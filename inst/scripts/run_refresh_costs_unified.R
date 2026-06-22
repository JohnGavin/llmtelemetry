#!/usr/bin/env Rscript
# run_refresh_costs_unified.R — Refresh the costs table in unified.duckdb
#
# Context (#309): the costs table in ~/.claude/logs/unified.duckdb stores
# daily opus/sonnet/haiku cost breakdowns produced by cmonitor-rs.  As of
# 2026-04-21 this table stopped updating because the upstream script
# (~llm/inst/scripts/refresh_costs_from_jsonl.R) has no scheduled launchd job
# and is not called from export_and_deploy_data.sh.
#
# This script is the llmtelemetry-side wrapper.  It can be called from:
#   - run_rollup.R (after the parquet rollups, before reporting)
#   - export_and_deploy_data.sh (added as a step after the JSON export)
#   - A new launchd plist pointing at this script
#
# Usage:
#   Rscript /path/to/llmtelemetry/inst/scripts/run_refresh_costs_unified.R
#   CMONITOR_RS=/path/to/cmonitor-rs Rscript ...   # override binary path
#   UNIFIED_DB=/path/to/unified.duckdb Rscript ...  # override DB path
#
# Exits 0 on success or when cmonitor-rs is not available (fail-open so the
# rest of the export pipeline is not blocked by a missing optional binary).
# Exits non-zero only on DB-write errors.

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(jsonlite)
})

CMONITOR <- Sys.getenv("CMONITOR_RS",
                       unset = file.path(Sys.getenv("HOME"), ".cargo", "bin", "cmonitor-rs"))
DB_PATH  <- Sys.getenv("UNIFIED_DB",
                       unset = file.path(Sys.getenv("HOME"), ".claude", "logs", "unified.duckdb"))

# ── Guard: cmonitor-rs binary must exist ──────────────────────────────────────
if (!nzchar(CMONITOR) || !file.exists(CMONITOR)) {
  message(sprintf(
    "run_refresh_costs_unified: cmonitor-rs not found at %s — skipping costs refresh.\n",
    CMONITOR
  ))
  quit(status = 0L)
}

if (!file.exists(DB_PATH)) {
  message(sprintf(
    "run_refresh_costs_unified: unified.duckdb not found at %s — skipping.\n",
    DB_PATH
  ))
  quit(status = 0L)
}

t0 <- proc.time()
message("run_refresh_costs_unified: fetching daily costs from cmonitor-rs...")

# ── Fetch daily JSON from cmonitor-rs ─────────────────────────────────────────
raw_json <- tryCatch(
  system2(CMONITOR,
          args   = c("--plan", "max20", "--view", "daily", "--output", "json",
                     "--since", "90d"),
          stdout = TRUE, stderr = FALSE),
  error = function(e) {
    message("run_refresh_costs_unified: cmonitor-rs failed: ", e$message)
    character(0L)
  }
)

if (!length(raw_json)) {
  message("run_refresh_costs_unified: cmonitor-rs returned no output — skipping.")
  quit(status = 0L)
}

combined <- paste(raw_json, collapse = "\n")

data <- tryCatch(
  fromJSON(combined, simplifyVector = FALSE),
  error = function(e) {
    message("run_refresh_costs_unified: JSON parse failed: ", e$message)
    NULL
  }
)

if (is.null(data) || is.null(data$blocks)) {
  message("run_refresh_costs_unified: no 'blocks' in cmonitor-rs JSON — skipping.")
  quit(status = 0L)
}

# ── Parse each daily block ────────────────────────────────────────────────────

parse_block <- function(b) {
  if (isTRUE(b$is_gap)) return(NULL)
  st   <- b$start_time
  date <- tryCatch(
    as.Date(st[[2]] - 1L, origin = paste0(st[[1]], "-01-01")),
    error = function(e) NULL
  )
  if (is.null(date)) return(NULL)
  opus <- sonnet <- haiku <- 0
  for (ms in b$model_stats) {
    m    <- tolower(ms$model)
    cost <- as.numeric(ms$cost_usd)
    if (grepl("opus",  m))     opus   <- opus   + cost
    else if (grepl("haiku", m)) haiku  <- haiku  + cost
    else                        sonnet <- sonnet + cost
  }
  data.frame(date = date, opus = opus, sonnet = sonnet, haiku = haiku,
             stringsAsFactors = FALSE)
}

rows <- Filter(Negate(is.null), lapply(data$blocks, parse_block))
if (!length(rows)) {
  message("run_refresh_costs_unified: cmonitor-rs returned no usable daily blocks.")
  quit(status = 0L)
}

wide <- do.call(rbind, rows)
wide <- aggregate(cbind(opus, sonnet, haiku) ~ date, data = wide, FUN = sum)
wide$total     <- wide$opus + wide$sonnet + wide$haiku
wide$opus_pct   <- ifelse(wide$total > 0, round(wide$opus   / wide$total * 100, 1), NA_real_)
wide$sonnet_pct <- ifelse(wide$total > 0, round(wide$sonnet / wide$total * 100, 1), NA_real_)
wide$haiku_pct  <- ifelse(wide$total > 0, round(wide$haiku  / wide$total * 100, 1), NA_real_)
wide <- wide[order(wide$date, decreasing = TRUE), ]

message(sprintf("run_refresh_costs_unified: parsed %d daily blocks from cmonitor-rs", nrow(wide)))

# ── Upsert into unified.duckdb costs table ────────────────────────────────────

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = DB_PATH)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

# Ensure the costs table exists with the expected schema
DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS costs (
    date        DATE PRIMARY KEY,
    opus_cost   DOUBLE DEFAULT 0,
    sonnet_cost DOUBLE DEFAULT 0,
    haiku_cost  DOUBLE DEFAULT 0,
    total_cost  DOUBLE DEFAULT 0,
    opus_pct    DOUBLE,
    sonnet_pct  DOUBLE,
    haiku_pct   DOUBLE
  )
")

# Write staging table and upsert
DBI::dbWriteTable(con, "costs_staging",
  wide[, c("date", "opus", "sonnet", "haiku", "total",
           "opus_pct", "sonnet_pct", "haiku_pct")],
  overwrite = TRUE
)

DBI::dbExecute(con, "
  INSERT INTO costs
    SELECT date,
           opus    AS opus_cost,
           sonnet  AS sonnet_cost,
           haiku   AS haiku_cost,
           total   AS total_cost,
           opus_pct, sonnet_pct, haiku_pct
    FROM costs_staging
  ON CONFLICT (date) DO UPDATE SET
    opus_cost   = excluded.opus_cost,
    sonnet_cost = excluded.sonnet_cost,
    haiku_cost  = excluded.haiku_cost,
    total_cost  = excluded.total_cost,
    opus_pct    = excluded.opus_pct,
    sonnet_pct  = excluded.sonnet_pct,
    haiku_pct   = excluded.haiku_pct
")

DBI::dbExecute(con, "DROP TABLE IF EXISTS costs_staging")

elapsed <- (proc.time() - t0)[["elapsed"]]
message(sprintf(
  "run_refresh_costs_unified: upserted %d dates | Total cost: $%.2f | Elapsed: %.1fs",
  nrow(wide), sum(wide$total, na.rm = TRUE), elapsed
))
message(sprintf("run_refresh_costs_unified: costs table now covers up to %s",
                format(max(wide$date), "%Y-%m-%d")))
