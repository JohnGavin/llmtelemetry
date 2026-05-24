#!/usr/bin/env Rscript
# refresh_roborev_vignette_rds.R
#
# Reads roborev_review_lifecycle from ~/.claude/logs/unified.duckdb (or the
# path in LLMTELEMETRY_UNIFIED_DB) and writes two RDS data files plus 5
# pre-built vig_roborev_*.rds plot/table objects to inst/extdata/vignettes/.
#
# Files produced:
#   vig_roborev_findings.rds   — real findings tibble (4465 rows as of 2026-05-24)
#   vig_roborev_loops.rds      — empty loops tibble (no loop detection yet)
#   vig_roborev_pulse.rds      — data.frame (pulse summary, no plotly/DT dep)
#   vig_roborev_trend.rds      — plotly object  (requires plotly)
#   vig_roborev_top.rds        — plotly object  (requires plotly)
#   vig_roborev_loops.rds      — DT object      (requires DT)
#   vig_roborev_inflow.rds     — plotly object  (requires plotly)
#   vig_roborev_resolution.rds — plotly object  (requires plotly)
#   vig_roborev_recent.rds     — DT object      (requires DT)
#
# When plotly/DT are unavailable (e.g. the lean nix shell), the plot objects
# are skipped with a warning. The vignette qmd falls back to building plots at
# render time from vig_roborev_findings.rds + vig_roborev_loops.rds, which are
# always written. In CI, pak::local_install() installs plotly+DT before render
# so the in-qmd fallback path also works.
#
# Column mapping: roborev_review_lifecycle -> roborev_findings contract
#   review_id    -> review_id (integer)
#   commit_sha   -> review_commit
#   severity_max -> severity  (normalised to lower-case)
#   repo         -> project   (and primary_file — best available per-review info)
#   agent        -> agent_id
#   created_at   -> found_at  (POSIXct UTC)
#   closed_at    -> resolved_at (POSIXct UTC, NA when still open)
#
# Usage (from repo root, inside nix shell):
#   Rscript inst/scripts/refresh_roborev_vignette_rds.R

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tibble)
  library(cli)
})

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
db_path <- Sys.getenv(
  "LLMTELEMETRY_UNIFIED_DB",
  path.expand("~/.claude/logs/unified.duckdb")
)

# Detect repo root reliably whether run interactively or as a script
repo_root <- tryCatch(
  here::here(),
  error = function(e) {
    # Script lives at inst/scripts/; walk up two levels
    script_path <- sys.frame(1)$ofile
    if (is.null(script_path) || !nzchar(script_path)) {
      getwd()
    } else {
      normalizePath(file.path(dirname(script_path), "..", ".."))
    }
  }
)

out_dir <- file.path(repo_root, "inst", "extdata", "vignettes")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cli::cli_h1("Refreshing vig_roborev_*.rds from unified.duckdb")
cli::cli_inform("DB path  : {.path {db_path}}")
cli::cli_inform("Out dir  : {.path {out_dir}}")

# ---------------------------------------------------------------------------
# Load package functions
# ---------------------------------------------------------------------------
pkg_loaded <- tryCatch({
  pkgload::load_all(path = repo_root, quiet = TRUE)
  TRUE
}, error = function(e) {
  tryCatch({
    library(llmtelemetry)
    TRUE
  }, error = function(e2) FALSE)
})
if (!pkg_loaded) {
  cli::cli_abort(
    c("x" = "Could not load {.pkg llmtelemetry}.",
      "i" = "Run from the package root with {.code pkgload::load_all()} or install the package.")
  )
}

# ---------------------------------------------------------------------------
# Connect and query
# ---------------------------------------------------------------------------
if (!file.exists(db_path)) {
  cli::cli_abort(
    c("x" = "DuckDB not found at {.path {db_path}}",
      "i" = "Set LLMTELEMETRY_UNIFIED_DB env var or ensure ~/.claude/logs/unified.duckdb exists.")
  )
}

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

tables <- DBI::dbListTables(con)
if (!"roborev_review_lifecycle" %in% tables) {
  cli::cli_abort(
    c("x" = "Table {.val roborev_review_lifecycle} not found in {.path {db_path}}",
      "i" = "Available tables: {.val {tables}}")
  )
}

lifecycle_raw <- DBI::dbGetQuery(
  con,
  "SELECT
     review_id,
     repo,
     agent,
     model,
     branch,
     commit_sha,
     created_at,
     closed_at,
     close_reason,
     verdict,
     severity_max
   FROM roborev_review_lifecycle
   ORDER BY created_at DESC"
)

cli::cli_inform("Fetched {nrow(lifecycle_raw)} rows from roborev_review_lifecycle")

# ---------------------------------------------------------------------------
# Build roborev_findings tibble (one row per review)
# ---------------------------------------------------------------------------
normalise_severity <- function(x) {
  out <- tolower(trimws(as.character(x)))
  valid <- c("critical", "high", "medium", "low", "info")
  ifelse(out %in% valid, out, "unknown")
}

findings <- lifecycle_raw |>
  dplyr::mutate(
    finding_id    = seq_len(dplyr::n()),
    review_id     = as.integer(review_id),
    review_commit = as.character(commit_sha),
    severity      = normalise_severity(severity_max),
    # primary_file = repo — best "file" proxy available from lifecycle data
    primary_file  = as.character(repo),
    problem_text  = paste0(
      "Review ", review_id,
      " [", dplyr::case_when(verdict == "P" ~ "pass", verdict == "F" ~ "fail",
                              TRUE ~ "unknown"), "]",
      " severity=", normalise_severity(severity_max),
      dplyr::if_else(!is.na(close_reason),
                     paste0(" close=", close_reason), "")
    ),
    summary       = paste0(
      normalise_severity(severity_max), " in ", repo,
      " (", dplyr::case_when(verdict == "P" ~ "pass",
                             verdict == "F" ~ "fail", TRUE ~ "unknown"), ")"
    ),
    found_at      = as.POSIXct(created_at, tz = "UTC"),
    resolved_at   = as.POSIXct(
      dplyr::if_else(is.na(closed_at), NA_real_,
                     as.numeric(as.POSIXct(closed_at, tz = "UTC"))),
      tz = "UTC", origin = "1970-01-01"
    ),
    fix_commit    = NA_character_,
    project       = as.character(repo),
    agent_id      = as.character(agent)
  ) |>
  dplyr::select(
    finding_id, review_id, review_commit, severity, primary_file,
    problem_text, summary, found_at, resolved_at, fix_commit,
    project, agent_id
  )

cli::cli_inform("Built findings tibble: {nrow(findings)} rows")

# ---------------------------------------------------------------------------
# Build empty roborev_loops tibble (no loop detection in unified.duckdb yet)
# ---------------------------------------------------------------------------
loops <- tibble::tibble(
  content_hash         = character(),
  severity             = character(),
  primary_file         = character(),
  summary              = character(),
  first_seen           = as.POSIXct(character(), tz = "UTC"),
  last_seen            = as.POSIXct(character(), tz = "UTC"),
  cycles               = integer(),
  tier                 = character(),
  fix_commit_shas      = list(),
  estimated_wasted_usd = double(),
  ack_by               = character(),
  ack_at               = as.POSIXct(character(), tz = "UTC"),
  ack_reason           = character(),
  ack_until            = as.POSIXct(character(), tz = "UTC")
)

cli::cli_inform("Loops: empty tibble (no loop detection yet — vig uses fixture DT fallback)")

# ---------------------------------------------------------------------------
# Save source data tibbles (always, no plotly/DT dependency)
# ---------------------------------------------------------------------------
cli::cli_h2("Saving source data tibbles")

save_rds <- function(obj, name) {
  path <- file.path(out_dir, paste0(name, ".rds"))
  saveRDS(obj, path)
  size_kb <- round(file.info(path)$size / 1024, 1)
  cli::cli_inform("  Saved {.path {path}} ({size_kb} KB)")
  invisible(path)
}

save_rds(findings, "vig_roborev_findings")
save_rds(loops,    "vig_roborev_loops")

# Pulse table is pure R (no plotly/DT), always save
save_rds(build_pulse_table(findings, loops), "vig_roborev_pulse")

# ---------------------------------------------------------------------------
# Build plotly/DT objects if packages are available
# ---------------------------------------------------------------------------
cli::cli_h2("Building plotly / DT vignette objects")

has_plotly <- requireNamespace("plotly", quietly = TRUE)
has_dt     <- requireNamespace("DT",     quietly = TRUE)

if (!has_plotly) cli::cli_warn("  {.pkg plotly} not available — skipping trend/top/inflow/resolution RDS")
if (!has_dt)     cli::cli_warn("  {.pkg DT} not available — skipping loops/recent RDS")
cli::cli_inform("  These are built at render time in CI after pak::local_install()")

build_and_save <- function(name, expr) {
  obj <- tryCatch(eval(expr), error = function(e) {
    cli::cli_warn(c("!" = "Failed {.val {name}}: {e$message}"))
    NULL
  })
  if (!is.null(obj)) save_rds(obj, name)
  invisible(obj)
}

if (has_plotly) {
  build_and_save("vig_roborev_trend",      quote(plot_open_findings_trend(findings)))
  build_and_save("vig_roborev_top",        quote(plot_top_files(findings)))
  build_and_save("vig_roborev_inflow",     quote(plot_inflow_outflow(findings)))
  build_and_save("vig_roborev_resolution", quote(plot_resolution_time(findings)))
}
if (has_dt) {
  build_and_save("vig_roborev_loops",  quote(plot_loops_table(loops)))
  build_and_save("vig_roborev_recent", quote(build_recent_table(findings, days = 30L)))
}

cli::cli_h2("Done")
rds_files <- list.files(out_dir, pattern = "\\.rds$", full.names = FALSE)
cli::cli_inform("RDS files in {.path {out_dir}} ({length(rds_files)} total):")
for (f in rds_files) {
  size_kb <- round(file.info(file.path(out_dir, f))$size / 1024, 1)
  cli::cli_inform("  {f} ({size_kb} KB)")
}
