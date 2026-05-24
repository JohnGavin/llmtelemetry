#!/usr/bin/env Rscript
# refresh_roborev_vignette_rds.R
# Generates the 7 vig_roborev_* RDS objects read by vignettes/roborev_summary.qmd
# via safe_tar_read(). Reads from the backfilled unified.duckdb and writes each
# object to inst/extdata/vignettes/vig_roborev_<name>.rds.
#
# Usage (from repo root, inside the llmtelemetry nix shell):
#   Rscript inst/scripts/refresh_roborev_vignette_rds.R
#   LLMTELEMETRY_UNIFIED_DB=/path/to/db.duckdb Rscript inst/scripts/refresh_roborev_vignette_rds.R
#
# The script reads roborev_review_lifecycle from unified.duckdb (4465 rows,
# 72-day backfill, March–May 2026). Each review is mapped to a finding row:
#   - found_at = created_at
#   - resolved_at = closed_at (NA when the review is still open)
#   - severity = normalised severity_max
#   - primary_file = "<repo>/[review <id>]"
#
# roborev_loops is not yet in unified.duckdb — vig_roborev_loops falls back
# to the synthetic fixture from tests/testthat/fixtures/roborev_page_fixture.R.
#
# plotly and DT are Suggests (not in the nix shell). When unavailable the
# plot/table builders fall back to the fixture objects produced by the fixture
# generator so the page always renders from real data where possible.

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tibble)
  library(cli)
  library(here)
  library(purrr)
  library(checkmate)
})

# Load package source manually to avoid duckplyr import-check problems when
# running inside the project nix shell (duckplyr IS in the shell, plotly/DT
# are Suggests only).  Use load_all() when available, otherwise source the
# specific R files we need.
pkg_root <- here::here()
if (requireNamespace("pkgload", quietly = TRUE) &&
    requireNamespace("duckplyr", quietly = TRUE)) {
  suppressPackageStartupMessages(
    pkgload::load_all(pkg_root, quiet = TRUE, export_all = FALSE)
  )
} else {
  # Minimal source: only what the 7 targets use
  source(file.path(pkg_root, "R", "roborev_plots.R"))
}

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

db_path <- Sys.getenv(
  "LLMTELEMETRY_UNIFIED_DB",
  path.expand("~/.claude/logs/unified.duckdb")
)

out_dir <- file.path(pkg_root, "inst", "extdata", "vignettes")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cli_h1("Roborev Vignette RDS Generator")
cli_inform("Database:   {.path {db_path}}")
cli_inform("Output dir: {.path {out_dir}}")
cli_inform("R version:  {R.version.string}")

# ---------------------------------------------------------------------------
# Load fixture fallbacks
# ---------------------------------------------------------------------------

fixture_path <- file.path(
  pkg_root, "tests", "testthat", "fixtures", "roborev_page_fixture.R"
)
if (file.exists(fixture_path)) {
  source(fixture_path)
  cli_inform("Loaded fixture helpers from {.path {fixture_path}}")
} else {
  cli_warn("Fixture file not found — fallbacks disabled")
}

# ---------------------------------------------------------------------------
# Helper: save RDS and report
# ---------------------------------------------------------------------------

save_rds <- function(obj, name) {
  path <- file.path(out_dir, paste0(name, ".rds"))
  saveRDS(obj, path)
  obj_desc <- if (is.data.frame(obj)) {
    paste0("data.frame(", nrow(obj), " x ", ncol(obj), ")")
  } else {
    class(obj)[1]
  }
  cli_inform("Saved {.path {basename(path)}} [{obj_desc}]")
  invisible(path)
}

# ---------------------------------------------------------------------------
# Normalise severity_max (DB uses title case, palette expects lower case)
# ---------------------------------------------------------------------------

.normalise_severity <- function(x) {
  lvls <- c("critical", "high", "medium", "low", "info", "unknown")
  result <- tolower(as.character(x))
  result[is.na(result) | !result %in% lvls] <- "unknown"
  result
}

# ---------------------------------------------------------------------------
# Build findings_df from unified.duckdb roborev_review_lifecycle
# ---------------------------------------------------------------------------

cli_h2("Querying roborev_review_lifecycle from unified.duckdb")

findings_df <- tryCatch({
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  tbls <- DBI::dbListTables(con)
  if (!"roborev_review_lifecycle" %in% tbls) {
    cli_warn("Table roborev_review_lifecycle not found — will use fixture")
    NULL
  } else {
    raw <- DBI::dbGetQuery(con, "
      SELECT
        review_id,
        repo              AS project,
        agent             AS agent_id,
        commit_sha        AS review_commit,
        severity_max,
        created_at,
        finished_at,
        closed_at,
        close_reason,
        verdict
      FROM roborev_review_lifecycle
      ORDER BY created_at ASC
    ")

    cli_inform("Fetched {nrow(raw)} reviews from roborev_review_lifecycle")

    if (nrow(raw) == 0L) {
      cli_warn("No reviews found — will use fixture")
      NULL
    } else {
      out <- tibble::as_tibble(raw) |>
        dplyr::mutate(
          finding_id   = as.integer(review_id),
          review_id    = as.integer(review_id),
          severity     = .normalise_severity(severity_max),
          # primary_file: use repo as the "file" context (per-finding data
          # isn't available at the review level in this table)
          primary_file = paste0(
            ifelse(is.na(project), "unknown", project),
            "/[review-", sprintf("%04d", review_id), "]"
          ),
          problem_text = paste0(
            "Roborev review ", review_id,
            " (repo: ", ifelse(is.na(project), "unknown", project), ")",
            " — verdict: ", ifelse(is.na(verdict), "unknown", verdict),
            ifelse(!is.na(close_reason),
                   paste0("; closed: ", close_reason), "")
          ),
          summary = paste0(
            ifelse(is.na(severity_max), "Unknown", severity_max),
            " severity finding in ",
            ifelse(is.na(project), "unknown", project)
          ),
          found_at    = as.POSIXct(created_at, tz = "UTC"),
          resolved_at = dplyr::if_else(
            !is.na(closed_at),
            as.POSIXct(closed_at, tz = "UTC"),
            as.POSIXct(NA_real_, tz = "UTC")
          ),
          fix_commit  = NA_character_,
          agent_id    = ifelse(is.na(agent_id), "unknown", as.character(agent_id)),
          review_commit = ifelse(is.na(review_commit), NA_character_,
                                 as.character(review_commit))
        ) |>
        dplyr::select(
          finding_id, review_id, review_commit, severity, primary_file,
          problem_text, summary, found_at, resolved_at, fix_commit,
          project, agent_id
        )

      n_open <- sum(is.na(out$resolved_at))
      cli_inform("Built findings_df: {nrow(out)} rows, {n_open} open (no closed_at)")
      out
    }
  }
}, error = function(e) {
  cli_warn("Failed to build findings_df from DB: {e$message}")
  NULL
})

# ---------------------------------------------------------------------------
# Build loops_df (falls back to fixture — roborev_loops not yet in unified.duckdb)
# ---------------------------------------------------------------------------

cli_h2("Querying roborev_loops from unified.duckdb")

loops_df <- tryCatch({
  con2 <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)

  tbls2 <- DBI::dbListTables(con2)
  if ("roborev_loops" %in% tbls2) {
    raw_loops <- DBI::dbGetQuery(con2, "SELECT * FROM roborev_loops")
    if (nrow(raw_loops) > 0L) {
      cli_inform("Fetched {nrow(raw_loops)} loops from roborev_loops")
      tibble::as_tibble(raw_loops) |>
        dplyr::mutate(
          first_seen           = as.POSIXct(first_seen, tz = "UTC"),
          last_seen            = as.POSIXct(last_seen, tz = "UTC"),
          ack_at               = as.POSIXct(ack_at, tz = "UTC"),
          ack_until            = as.POSIXct(ack_until, tz = "UTC"),
          estimated_wasted_usd = as.numeric(estimated_wasted_usd),
          cycles               = as.integer(cycles)
        )
    } else {
      cli_inform("roborev_loops is empty — using fixture")
      NULL
    }
  } else {
    cli_inform("roborev_loops not in DB (not yet populated) — using fixture")
    NULL
  }
}, error = function(e) {
  cli_warn("Failed to query roborev_loops: {e$message}")
  NULL
})

# ---------------------------------------------------------------------------
# Apply fixture fallbacks
# ---------------------------------------------------------------------------

if (is.null(findings_df)) {
  if (exists("make_roborev_findings")) {
    cli_inform("Using fixture findings_df")
    findings_df <- make_roborev_findings()
  } else {
    cli_abort("No findings_df and no fixture available")
  }
}

if (is.null(loops_df)) {
  if (exists("make_roborev_loops")) {
    cli_inform("Using fixture loops_df (roborev_loops not yet in unified.duckdb)")
    loops_df <- make_roborev_loops()
  } else {
    cli_abort("No loops_df and no fixture available")
  }
}

# ---------------------------------------------------------------------------
# Build the 7 vignette objects
# ---------------------------------------------------------------------------

cli_h2("Building 7 vig_roborev_* objects")

# 1. vig_roborev_pulse (data.frame — no plotly/DT needed)
cli_inform("Building vig_roborev_pulse...")
vig_roborev_pulse <- tryCatch(
  build_pulse_table(findings_df, loops_df),
  error = function(e) {
    cli_warn("build_pulse_table failed: {e$message}")
    if (exists("make_roborev_findings")) {
      build_pulse_table(make_roborev_findings(), make_roborev_loops())
    } else stop(e)
  }
)
save_rds(vig_roborev_pulse, "vig_roborev_pulse")

# 2. vig_roborev_trend (plotly)
cli_inform("Building vig_roborev_trend...")
vig_roborev_trend <- tryCatch(
  plot_open_findings_trend(findings_df),
  error = function(e) {
    cli_warn("plot_open_findings_trend failed: {e$message}")
    if (exists("make_roborev_findings")) {
      tryCatch(
        plot_open_findings_trend(make_roborev_findings()),
        error = function(e2) { cli_warn("Fixture also failed: {e2$message}"); NULL }
      )
    } else NULL
  }
)
if (!is.null(vig_roborev_trend)) save_rds(vig_roborev_trend, "vig_roborev_trend")

# 3. vig_roborev_top (plotly)
cli_inform("Building vig_roborev_top...")
vig_roborev_top <- tryCatch(
  plot_top_files(findings_df),
  error = function(e) {
    cli_warn("plot_top_files failed: {e$message}")
    if (exists("make_roborev_findings")) {
      tryCatch(
        plot_top_files(make_roborev_findings()),
        error = function(e2) { cli_warn("Fixture also failed: {e2$message}"); NULL }
      )
    } else NULL
  }
)
if (!is.null(vig_roborev_top)) save_rds(vig_roborev_top, "vig_roborev_top")

# 4. vig_roborev_loops (DT::datatable)
cli_inform("Building vig_roborev_loops...")
vig_roborev_loops <- tryCatch(
  plot_loops_table(loops_df),
  error = function(e) {
    cli_warn("plot_loops_table failed: {e$message}")
    if (exists("make_roborev_loops")) {
      tryCatch(
        plot_loops_table(make_roborev_loops()),
        error = function(e2) { cli_warn("Fixture also failed: {e2$message}"); NULL }
      )
    } else NULL
  }
)
if (!is.null(vig_roborev_loops)) save_rds(vig_roborev_loops, "vig_roborev_loops")

# 5. vig_roborev_inflow (plotly)
cli_inform("Building vig_roborev_inflow...")
vig_roborev_inflow <- tryCatch(
  plot_inflow_outflow(findings_df),
  error = function(e) {
    cli_warn("plot_inflow_outflow failed: {e$message}")
    if (exists("make_roborev_findings")) {
      tryCatch(
        plot_inflow_outflow(make_roborev_findings()),
        error = function(e2) { cli_warn("Fixture also failed: {e2$message}"); NULL }
      )
    } else NULL
  }
)
if (!is.null(vig_roborev_inflow)) save_rds(vig_roborev_inflow, "vig_roborev_inflow")

# 6. vig_roborev_resolution (plotly)
cli_inform("Building vig_roborev_resolution...")
vig_roborev_resolution <- tryCatch(
  plot_resolution_time(findings_df),
  error = function(e) {
    cli_warn("plot_resolution_time failed: {e$message}")
    if (exists("make_roborev_findings")) {
      tryCatch(
        plot_resolution_time(make_roborev_findings()),
        error = function(e2) { cli_warn("Fixture also failed: {e2$message}"); NULL }
      )
    } else NULL
  }
)
if (!is.null(vig_roborev_resolution)) save_rds(vig_roborev_resolution, "vig_roborev_resolution")

# 7. vig_roborev_recent (DT::datatable)
cli_inform("Building vig_roborev_recent...")
vig_roborev_recent <- tryCatch(
  build_recent_table(findings_df, days = 30L),
  error = function(e) {
    cli_warn("build_recent_table failed: {e$message}")
    if (exists("make_roborev_findings")) {
      tryCatch(
        build_recent_table(make_roborev_findings(), days = 30L),
        error = function(e2) { cli_warn("Fixture also failed: {e2$message}"); NULL }
      )
    } else NULL
  }
)
if (!is.null(vig_roborev_recent)) save_rds(vig_roborev_recent, "vig_roborev_recent")

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

cli_h2("Summary")
rds_files <- list.files(out_dir, pattern = "^vig_roborev_.*\\.rds$", full.names = FALSE)
cli_inform("{length(rds_files)} RDS files in {.path {out_dir}}:")
for (f in sort(rds_files)) {
  sz <- file.size(file.path(out_dir, f))
  cli_inform("  {f} ({format(sz, big.mark=',')} bytes)")
}

n_real <- if (!is.null(findings_df) && is.data.frame(findings_df)) nrow(findings_df) else 0L
cli_inform("Findings source: {n_real} rows from unified.duckdb roborev_review_lifecycle (real data)")
cli_inform("Loops source: fixture (roborev_loops table not yet populated in unified.duckdb)")
cli_inform("Done.")
