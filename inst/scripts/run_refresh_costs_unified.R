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
# Exit / status contract (llm#907):
#   "skipped" — an OPTIONAL precondition is absent (cmonitor-rs binary, the
#               unified.duckdb file, or cmonitor-rs returning zero daily
#               blocks for the window). This is a legitimate machine/data
#               state, not a defect. Exits 0 when run standalone.
#   "error"   — the script itself malfunctioned (JSON that cmonitor-rs
#               produced could not be parsed, or any other unexpected
#               condition). This must NOT be described as the binary being
#               absent, and exits 1 when run standalone so a caller can
#               surface it.
#   "ok"      — costs table upserted successfully. Exits 0 when standalone.
#
# All logic lives in refresh_costs_unified(), which returns a status list
# instead of calling quit() — quit() previously terminated whatever process
# had sys.source()'d this file (llm#907 Deliverable 3), not just this
# fragment. quit() is called at most once, at the very bottom, and only when
# this file was invoked directly via Rscript (detected below) — never when
# sourced by a caller such as run_rollup.R.

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(jsonlite)
})

# Detect direct invocation (`Rscript run_refresh_costs_unified.R`) vs. being
# sys.source()'d by a caller (e.g. run_rollup.R). commandArgs() reflects the
# whole process's args regardless of who sourced what, so comparing the
# --file= basename against this script's own name tells us which case we're
# in — a caller sourcing this file will still show ITS OWN --file=.
.this_script_name <- "run_refresh_costs_unified.R"
.cli_args         <- commandArgs(trailingOnly = FALSE)
.file_arg         <- grep("^--file=", .cli_args, value = TRUE)
.invoked_directly <- length(.file_arg) > 0 &&
  identical(basename(sub("^--file=", "", .file_arg[1])), .this_script_name)

refresh_costs_unified <- function() {
  CMONITOR <- Sys.getenv("CMONITOR_RS",
                         unset = file.path(Sys.getenv("HOME"), ".cargo", "bin", "cmonitor-rs"))
  DB_PATH  <- Sys.getenv("UNIFIED_DB",
                         unset = file.path(Sys.getenv("HOME"), ".claude", "logs", "unified.duckdb"))

  # ── Guard: cmonitor-rs binary must exist (optional binary — quiet skip) ────
  if (!nzchar(CMONITOR) || !file.exists(CMONITOR)) {
    message(sprintf(
      "run_refresh_costs_unified: cmonitor-rs not found at %s — skipping costs refresh.",
      CMONITOR
    ))
    return(list(status = "skipped", message = "cmonitor-rs binary not found"))
  }

  if (!file.exists(DB_PATH)) {
    message(sprintf(
      "run_refresh_costs_unified: unified.duckdb not found at %s — skipping.",
      DB_PATH
    ))
    return(list(status = "skipped", message = "unified.duckdb not found"))
  }

  t0 <- proc.time()
  message("run_refresh_costs_unified: fetching daily costs from cmonitor-rs...")

  # ── Fetch daily JSON from cmonitor-rs ─────────────────────────────────────
  raw_json <- tryCatch(
    system2(CMONITOR,
            args   = c("--plan", "max20", "--view", "daily", "--output", "json",
                       "--since", "90d"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) {
      message("run_refresh_costs_unified: cmonitor-rs failed to execute: ", conditionMessage(e))
      character(0L)
    }
  )

  if (!length(raw_json)) {
    message("run_refresh_costs_unified: cmonitor-rs returned no output — skipping.")
    return(list(status = "skipped", message = "cmonitor-rs returned no output"))
  }

  combined <- paste(raw_json, collapse = "\n")

  # ── Parse JSON — a parse failure here is a SCRIPT defect, not an absent
  # binary. Keep it a distinct branch from "blocks missing" below (llm#907
  # Deliverable 2): conflating the two previously mislabelled every parse
  # failure as "no 'blocks' in cmonitor-rs JSON", which blames the binary
  # for a bug in this script.
  parsed <- tryCatch(
    list(ok = TRUE, data = jsonlite::fromJSON(combined, simplifyVector = FALSE)),
    error = function(e) list(ok = FALSE, error = e)
  )

  if (!isTRUE(parsed$ok)) {
    msg <- paste0(
      "run_refresh_costs_unified: failed to parse cmonitor-rs JSON output — ",
      "this is a defect in this script, not a missing binary: ",
      conditionMessage(parsed$error)
    )
    message(msg)
    return(list(status = "error", message = msg))
  }

  data <- parsed$data

  if (is.null(data$blocks)) {
    message("run_refresh_costs_unified: cmonitor-rs JSON has no 'blocks' field — skipping (no data for this window).")
    return(list(status = "skipped", message = "no 'blocks' field in cmonitor-rs JSON"))
  }

  # ── Parse each daily block ──────────────────────────────────────────────

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
    return(list(status = "skipped", message = "no usable daily blocks after parsing"))
  }

  wide <- do.call(rbind, rows)
  wide <- stats::aggregate(cbind(opus, sonnet, haiku) ~ date, data = wide, FUN = sum)
  wide$total     <- wide$opus + wide$sonnet + wide$haiku
  wide$opus_pct   <- ifelse(wide$total > 0, round(wide$opus   / wide$total * 100, 1), NA_real_)
  wide$sonnet_pct <- ifelse(wide$total > 0, round(wide$sonnet / wide$total * 100, 1), NA_real_)
  wide$haiku_pct  <- ifelse(wide$total > 0, round(wide$haiku  / wide$total * 100, 1), NA_real_)
  wide <- wide[order(wide$date, decreasing = TRUE), ]

  message(sprintf("run_refresh_costs_unified: parsed %d daily blocks from cmonitor-rs", nrow(wide)))

  # ── Upsert into unified.duckdb costs table ────────────────────────────────
  # DB-write failures are NOT caught here — they propagate as normal R
  # errors, exactly as before this refactor. A caller's tryCatch (e.g.
  # run_rollup.R) still sees them; a standalone Rscript invocation still
  # halts with a non-zero exit via R's default top-level error handling.

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

  list(status = "ok", message = sprintf("upserted %d dates", nrow(wide)))
}

# `.refresh_result` is left as a top-level binding in this file's evaluation
# environment (whether that's globalenv(), for standalone Rscript execution,
# or the caller-supplied envir passed to sys.source()) so a caller can read
# it back after sourcing completes — sys.source() itself always returns
# invisible(NULL), so the return value of refresh_costs_unified() cannot be
# recovered any other way.
.refresh_result <- refresh_costs_unified()

if (.invoked_directly) {
  quit(status = if (identical(.refresh_result$status, "error")) 1L else 0L)
}
