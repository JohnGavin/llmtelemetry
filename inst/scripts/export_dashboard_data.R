#!/usr/bin/env Rscript
# Export dashboard data: flatten/trim raw data to browser-friendly JSON
# Run: Rscript inst/scripts/export_dashboard_data.R
# Data source: cmonitor-rs CLI for Claude usage, DuckDB for Gemini

library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(DBI)
library(duckdb)

# Allow override via env var so worktree runs don't resolve to main checkout
pkg_root <- if (nzchar(Sys.getenv("PKG_ROOT"))) Sys.getenv("PKG_ROOT") else here::here()
extdata  <- file.path(pkg_root, "inst", "extdata")
out_dir  <- file.path(pkg_root, "vignettes", "data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Load shared sanitize_session_id helper (unifies hash format with rollup_sessions.R)
# so the same (path, project, started_at) triple always produces the same id.
local({
  helper <- file.path(pkg_root, "R", "sanitize_session_id.R")
  if (file.exists(helper)) source(helper, local = FALSE)
})

cmonitor_bin <- "/Users/johngavin/.cargo/bin/cmonitor-rs"

# ---------------------------------------------------------------------------
# Privacy exclusion / remap constants (#83 Phase B)
# ---------------------------------------------------------------------------
# Projects whose names must NEVER appear in public dashboard output.
EXCLUDED_DASHBOARD_PROJECTS <- c(
  "mycare", "crypto", "crypto_solwatch", "crypto_swarms",
  "solwatch", "swarms",    # short-form aliases of crypto_solwatch / crypto_swarms
  "my_t_project", "hello_t", "t_demos"
)
# Subset that is privacy-gated (used by the CI gate to match per-project JSON fields).
# TODO(#83): unify with R/confidential_projects.R to avoid list drift
CONFIDENTIAL_PROJECTS <- c("mycare", "crypto", "crypto_solwatch", "crypto_swarms",
                            "solwatch", "swarms")

# Name-folding remap: fold suffixed aliases into their canonical parent project,
# then re-aggregate so their rows merge into the parent's totals.
PROJECT_REMAP <- c("llmtelemetry-hook-sync" = "llmtelemetry")

#' Clean per-project data frame before writing to public output.
#'
#' Applies three steps in order:
#'   1. Normalise worktree/branch-suffixed names to their base project
#'      (delegated to canonicalize_project / canonicalize_session_project which
#'      are defined later in this script; can also be called post-definition).
#'   2. Apply PROJECT_REMAP (fold aliases into canonical parents).
#'   3. Drop rows whose project matches EXCLUDED_DASHBOARD_PROJECTS.
#'
#' Matching is case-insensitive and handles suffix variants like
#' "mycare-feat-something" by checking whether the base name (before the first
#' "-feat-", "-fix-", or agent-worktree suffix) matches an excluded entry.
#'
#' @param df        A data frame with a project column.
#' @param project_col  Name of the project column (default "project").
#' @return The filtered/remapped data frame.
clean_projects <- function(df, project_col = "project") {
  if (!is.data.frame(df) || nrow(df) == 0L) return(df)
  if (!project_col %in% names(df)) return(df)

  proj <- df[[project_col]]

  # Step 1: base-name extraction (strip branch/worktree suffixes to reveal root).
  # Matches suffixes like "-feat-foo", "-fix-foo", "-worktree-NNN", "-YYYYMMDD-...",
  # and the "-hook-sync" suffix so we can check the base against the remap table.
  base_proj <- sub(
    "-(?:feat|fix|chore|docs|refactor|test|ci|perf|style|build|revert|worktree)-.*$",
    "",
    proj,
    ignore.case = TRUE
  )

  # Step 2: apply PROJECT_REMAP on the base name (case-insensitive key lookup).
  remap_keys_lower <- tolower(names(PROJECT_REMAP))
  for (i in seq_along(PROJECT_REMAP)) {
    hits <- tolower(base_proj) == remap_keys_lower[i] |
            tolower(proj)      == remap_keys_lower[i]
    proj[hits]      <- PROJECT_REMAP[[i]]
    base_proj[hits] <- PROJECT_REMAP[[i]]
  }
  df[[project_col]] <- proj

  # Step 3: drop rows whose BASE project name matches EXCLUDED_DASHBOARD_PROJECTS
  # (case-insensitive). This catches both exact matches ("mycare") and suffixed
  # variants ("mycare-feat-something").
  excluded_lower <- tolower(EXCLUDED_DASHBOARD_PROJECTS)
  keep <- !tolower(base_proj) %in% excluded_lower
  df[keep, , drop = FALSE]
}

# Helper: convert cmonitor-rs time array to "YYYY-MM-DD HH:MM:SS" string
# t = c(year, day_of_year, hour, min, sec, nanosec, ...)
parse_cmonitor_time <- function(t) {
  if (is.null(t) || length(t) < 5) return(NA_character_)
  origin <- as.Date(paste0(t[1], "-01-01"))
  d <- origin + (t[2] - 1L)
  sprintf("%s %02d:%02d:%02d", d, t[3], t[4], t[5])
}

# Branch/worktree suffixes that, when appended to a project name with a dash,
# indicate the project itself and not a separate sub-entity.
# Pattern: <project>-<SUFFIX>-<anything>
# Examples: llmtelemetry-feat-cc-20260524-102501 -> llmtelemetry
#           llm-wt-193 -> llm
#           llm-sonnet -> llm (bare suffix, no trailing tokens)
.BRANCH_SUFFIX_RE_EXPORT <- paste0(
  "-(feat|fix|chore|docs|refactor|test|ci|perf|style|build|revert|wt|",
  "sonnet|haiku|opus|worktree)(-.*)?$"
)

# Helper: shorten project path to last meaningful component (kept for legacy use)
shorten_project <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)

  # Handle .claude/worktrees/agent-<hex> paths: ephemeral agent checkouts.
  if (grepl("^[.]claude/worktrees/agent-", x) ||
      grepl("[-/]claude[-/]worktrees[-/]agent-", x)) {
    return(".claude/worktrees/agent")
  }

  x |>
    # NHS/personal path prefix (double-dash form):
    # "-Users-johngavin-docs--pers-NHS-health-data-antigravity-<project>"
    gsub("^-Users-johngavin-docs--pers-NHS-health-data-antigravity-", "", x = _) |>
    gsub("^-Users-johngavin-docs[-_]gh-", "", x = _) |>
    # Strip branch/worktree suffixes BEFORE stripping the "llm-" prefix.
    # Without this, "llm-feat-cc-..." becomes "feat-cc-..." -> first segment "feat".
    sub(.BRANCH_SUFFIX_RE_EXPORT, "", x = _, perl = TRUE, ignore.case = TRUE) |>
    gsub("^llm-", "", x = _) |>
    gsub("^proj-", "", x = _) |>
    gsub("-", "/", x = _)
}

# Helper: map raw project path to canonical project name.
# Returns NA_character_ for orphan/meta names that should be excluded from
# project-aware aggregations (agent-worktree IDs, generic meta names).
# Returns "agent-tooling" for real Claude usage sessions with no recoverable
# parent project (roborev, ClaudeProbe, sonnet, cc, eval, subagents, worker).
canonicalize_project <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(name)) return(NA_character_)
  # Defensively drop "agent-tooling" label before any dash-to-slash conversion
  # (the dash in "agent-tooling" would otherwise yield first segment "agent").
  if (identical(name, "agent-tooling")) return(NA_character_)

  # 0a. Reject worktree-suffixed names (e.g. "roborev-worktree-3047898692",
  #    "llm-worktree-1234567890", or paths containing "/worktree/").
  #    These are ephemeral agent checkout directories, not real projects.
  if (grepl("-worktree-\\d+", name) || grepl("/worktree/\\d+", name)) {
    return(NA_character_)
  }

  # 0b. Reject bare hex-hash strings (12+ lowercase hex chars) — these are
  #     agent worktree hashes recorded as project names by some hooks.
  if (grepl("^[0-9a-f]{12,}$", name)) return(NA_character_)

  # 1a. 2026-05-26 user decision: former "agent-tooling" tokens are noise (NA).
  #     They are handled via meta_only below (reverses the 2026-05-25 bucketing).

  # 1b. Explicit single-token remaps: bare token → full canonical project name.
  token_remaps <- c(
    "network"   = "irish_buoy_network",
    "telemetry" = "llmtelemetry"
  )
  if (name %in% names(token_remaps)) return(token_remaps[[name]])

  # 1c. Drop pure meta-names (agent dirs, generic worktree marker, and
  #    top-level container directories that are not real projects).
  meta_only <- c(
    # Original set (container dirs and true noise):
    "worktree",
    "antigravity", "crypto", "data", "github", "hello",
    "simulations", "sport",
    "t", "io", "notmineraft",
    # Added: branch fragment tokens
    "feat", "fix", "chore", "ci", "perf", "style", "build", "revert",
    "wt", "scope", "repo", "docs", "project",
    # Added: user-confirmed noise (demos = demo content, wiki = internal wiki)
    "demos", "wiki",
    # Added: .claude/worktrees/agent sentinel from shorten_project()
    ".claude/worktrees/agent",
    # Added 2026-05-26: former agent-tooling tokens are now treated as noise
    # (reverses 2026-05-25 bucketing decision). "agent-tooling" itself is added
    # defensively so any pre-computed stale value re-canonicalizes to NA.
    "roborev", "ClaudeProbe", "ClaudeProject",
    "sonnet", "cc", "eval", "subagents", "worker", "agent-tooling",
    # 2026-05-26: 'agent' is an agent-<x> path residual, not a real project
    "agent"
  )
  if (name %in% meta_only) return(NA_character_)

  # 2. Strip agent worktree ID prefix — patterns like "D73dOZsvyf/repo" or
  #    "kSBNJFuu6G/repo/subdir". Match alphanumeric strings of 8+ chars
  #    followed by "/repo" (with optional path continuation).
  name <- sub("^[A-Za-z0-9_]{8,}/repo/?", "", name)
  if (!nzchar(name)) return(NA_character_)

  # 3. Explicit prefix overrides — sub-paths mapped to canonical parent project.
  #    Checked BEFORE the container-prefix strip so named overrides win.
  overrides <- list(
    "buoy/network"               = "irish_buoy_network",
    "irishbuoys"                 = "irish_buoy_network",
    # raw path proj-data-weather-irish-buoy-network -> data/weather/irish/buoy/network
    "data/weather/irish/buoy/network" = "irish_buoy_network",
    # after data/ strip: weather/irish/buoy/network
    "weather/irish/buoy/network" = "irish_buoy_network"
  )
  for (pat in names(overrides)) {
    if (startsWith(name, pat)) return(overrides[[pat]])
  }

  # 4. Strip container-directory prefixes (sub-paths where the FIRST segment is
  #    a meta container and the SECOND segment is the actual project).
  #    e.g. "simulations/randomwalk" -> "randomwalk", "sport/footbet" -> "footbet"
  container_prefixes <- c(
    # Multi-segment prefixes (must come before single-segment ones):
    "stats/simulations/", "stats/sport/", "finance/data/",
    # Single-segment container prefixes:
    "worktree/", "simulations/", "sport/", "data/", "crypto/",
    "subagents/", "knowledge/", "github/", "antigravity/", "hello/",
    "stats/", "pers/", "finance/"
  )
  for (pfx in container_prefixes) {
    if (startsWith(name, pfx)) {
      name <- sub(paste0("^", pfx), "", name)
      break
    }
  }
  if (!nzchar(name)) return(NA_character_)

  # 5. Default: take the FIRST path segment as the canonical project name.
  #    e.g. "llm/vignettes" -> "llm", "footbet" -> "footbet"
  parts <- strsplit(name, "/", fixed = TRUE)[[1]]
  if (length(parts) == 0L || !nzchar(parts[1L])) return(NA_character_)
  first <- parts[1L]
  # Drop purely numeric segments (worktree numeric IDs, e.g. "1020043174")
  if (grepl("^[0-9]+$", first)) return(NA_character_)
  # meta_only now includes former agent-tooling tokens (2026-05-26 decision)
  if (first %in% meta_only) return(NA_character_)
  first
}

# Internal helper: canonicalize a session project name stored in
# dash-form by unified.duckdb hooks (e.g. "D73dOZsvyf-repo",
# "buoy-network", "worktree-12345678").  Converts to slash form via
# shorten_project() first, then falls through canonicalize_project().
# Not exported; used only by the session and cost_by_project exports.
canonicalize_session_project <- function(raw) {
  canonicalize_project(shorten_project(raw))
}
# Vectorise so they can be applied to a column directly. canonicalize_session_project
# MUST also be vectorised: it is the function actually applied to project columns
# (cost_by_project, unified_sessions, tokens). Its shorten_project() call is scalar-only,
# so on a vector it errors with "'length = N' in coercion to 'logical(1)'".
canonicalize_project <- Vectorize(canonicalize_project, USE.NAMES = FALSE)
canonicalize_session_project <- Vectorize(canonicalize_session_project, USE.NAMES = FALSE)

#' Sanitize a data frame before committing to public inst/extdata/ (#936)
#'
#' Drops rows where canonical_project IS NA (orphan/agent-worktree rows).
#' Replaces the raw `project` column with the canonical form so that raw
#' filesystem paths (e.g. "-Users-johngavin-docs-gh-llm") do not appear in
#' publicly committed JSON/Parquet files.
#' Sanitizes `session_id` if present: path-style ids (starting with "-" or
#' containing "/") are replaced with a deterministic salted hash so that
#' deduplication in rollup functions still works without exposing private paths.
#'
#' @param df A data frame to sanitize.
#' @return A sanitized data frame (or the input unchanged if it has no rows
#'   or no canonical_project column).
sanitize_for_public <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L) return(df)
  if ("canonical_project" %in% names(df)) {
    # Drop orphan/agent-worktree rows (canonical_project is NA)
    df <- df[!is.na(df$canonical_project), , drop = FALSE]
    # Replace raw project with canonical form
    df$project <- df$canonical_project
  }
  # Sanitize session_id: path-style ids contain private filesystem paths.
  # Delegate to the shared helper in R/sanitize_session_id.R (loaded at the
  # top of this script) so that the hash format matches rollup_sessions.R.
  # Format: "sanitized@{canonical_project}@{iso8601_utc}@h{hash12}"
  # Previously used an incompatible polynomial-%%1e6 6-decimal hash (roborev V/b).
  if ("session_id" %in% names(df) &&
      exists(".sanitize_session_id_local", envir = globalenv(),
             mode = "function", inherits = FALSE)) {
    has_started_at <- "started_at" %in% names(df)
    has_cp         <- "canonical_project" %in% names(df)
    cp_col  <- if (has_cp)         df$canonical_project else rep(NA_character_, nrow(df))
    sat_col <- if (has_started_at) df$started_at        else rep(NA_real_, nrow(df))
    # Convert started_at to POSIXct if it is character (export script may have strings)
    if (!inherits(sat_col, "POSIXct")) {
      sat_col <- tryCatch(
        as.POSIXct(sat_col, tz = "UTC"),
        error = function(e) rep(as.POSIXct(NA), nrow(df))
      )
    }
    df$session_id <- .sanitize_session_id_local(
      session_ids        = df$session_id,
      canonical_projects = cp_col,
      started_at         = sat_col
    )
  }
  df
}

# --- 1. Daily usage from cmonitor-rs ------------------------------------------
cat("Exporting ccusage daily (via cmonitor-rs)...\n")
has_cmonitor <- file.exists(cmonitor_bin)
if (!has_cmonitor) {
  cat("  -> cmonitor-rs not found, falling back to ccusage JSON files\n")
}

if (has_cmonitor) {
  cmon_json_raw <- system2(
    cmonitor_bin,
    args   = c("--plan", "max20", "--view", "daily", "--output", "json", "--since", "90d"),
    stdout = TRUE,
    stderr = FALSE
  )
  cmon_data <- fromJSON(paste(cmon_json_raw, collapse = "\n"), simplifyDataFrame = FALSE)
  blocks_all <- cmon_data$blocks

  # Aggregate blocks by date for the daily view
  daily_rows <- lapply(blocks_all, function(b) {
    if (isTRUE(b$is_gap)) return(NULL)
    st <- b$start_time
    origin <- as.Date(paste0(st[1], "-01-01"))
    date_str <- as.character(origin + (st[2] - 1L))
    tok <- b$tokens
    tibble(
      date          = date_str,
      inputTokens   = tok$input_tokens,
      outputTokens  = tok$output_tokens,
      cacheCreation = tok$cache_creation_tokens,
      cacheRead     = tok$cache_read_tokens,
      totalTokens   = tok$input_tokens + tok$output_tokens +
                      tok$cache_creation_tokens + tok$cache_read_tokens,
      totalCost     = round(b$cost_usd, 4),
      modelsUsed    = paste(b$models, collapse = ", ")
    )
  }) |> bind_rows()

  daily_rows <- daily_rows |>
    group_by(date) |>
    summarise(
      inputTokens   = sum(inputTokens),
      outputTokens  = sum(outputTokens),
      cacheCreation = sum(cacheCreation),
      cacheRead     = sum(cacheRead),
      totalTokens   = sum(totalTokens),
      totalCost     = round(sum(totalCost), 4),
      modelsUsed    = paste(unique(unlist(strsplit(modelsUsed, ", "))), collapse = ", "),
      .groups = "drop"
    ) |>
    mutate(project = "all") |>
    arrange(date)

  write_json(daily_rows, file.path(out_dir, "ccusage_daily.json"), auto_unbox = TRUE)
  # Also persist to inst/extdata so CI has a valid committed snapshot
  write_json(daily_rows, file.path(extdata, "ccusage_daily.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d daily rows\n", nrow(daily_rows)))

  # --- 2. Sessions (not available from cmonitor-rs) ---------------------------
  cat("Exporting ccusage sessions (empty — cmonitor-rs has no session view)...\n")
  write_json(list(), file.path(out_dir, "ccusage_sessions.json"), auto_unbox = TRUE)
  cat("  -> 0 sessions (placeholder)\n")

  # --- 3. Blocks from cmonitor-rs ---------------------------------------------
  cat("Exporting ccusage blocks (via cmonitor-rs)...\n")
  blk_rows <- lapply(blocks_all, function(b) {
    if (isTRUE(b$is_gap)) return(NULL)
    tok <- b$tokens
    tibble(
      startTime     = parse_cmonitor_time(b$start_time),
      endTime       = parse_cmonitor_time(b$end_time),
      actualEndTime = parse_cmonitor_time(b$actual_end_time),
      entries       = b$message_count,
      inputTokens   = tok$input_tokens,
      outputTokens  = tok$output_tokens,
      cacheCreation = tok$cache_creation_tokens,
      cacheRead     = tok$cache_read_tokens,
      totalTokens   = tok$input_tokens + tok$output_tokens +
                      tok$cache_creation_tokens + tok$cache_read_tokens,
      costUSD       = round(b$cost_usd, 4),
      models        = paste(b$models, collapse = ", ")
    )
  }) |> bind_rows() |> arrange(startTime)

  write_json(blk_rows, file.path(out_dir, "ccusage_blocks.json"), auto_unbox = TRUE)
  # Also persist to inst/extdata so CI has a committed snapshot to fall back to
  write_json(blk_rows, file.path(extdata, "ccusage_blocks.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d active blocks\n", nrow(blk_rows)))

  # --- 3b. Per-model daily breakdown from model_stats -------------------------
  # NOTE (#1032): model_daily is a GLOBAL metric (cost/tokens by model, not by
  # project). It must NEVER be filtered by canonical_project or any project
  # membership condition — doing so would silently corrupt historical dates.
  cat("Exporting per-model daily data...\n")
  model_daily <- lapply(blocks_all, function(b) {
    if (isTRUE(b$is_gap) || is.null(b$model_stats)) return(NULL)
    st <- b$start_time
    origin <- as.Date(paste0(st[1], "-01-01"))
    date_str <- as.character(origin + (st[2] - 1L))
    lapply(b$model_stats, function(ms) {
      tibble(
        date   = date_str,
        model  = ms$model,
        cost   = round(ms$cost_usd, 4),
        tokens = ms$total_tokens %||% (ms$input_tokens + ms$output_tokens +
          ms$cache_creation_tokens + ms$cache_read_tokens)
      )
    }) |> bind_rows()
  }) |> bind_rows()
  if (nrow(model_daily) > 0) {
    model_daily <- model_daily |>
      group_by(date, model) |>
      summarise(cost = round(sum(cost), 4), tokens = sum(tokens), .groups = "drop") |>
      arrange(date, desc(cost))
  }
  write_json(model_daily, file.path(out_dir, "model_daily.json"), auto_unbox = TRUE)
  # Also save to inst/extdata for email script
  write_json(model_daily, file.path(extdata, "model_daily.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d model-day rows\n", nrow(model_daily)))

} else {
  # CI fallback: read existing ccusage JSON files from inst/extdata/
  blocks_all <- list()
  # ccusage_daily.json is the flat daily array the dashboard expects (updated by local
  # cmonitor-rs runs). ccusage_daily_all.json is the old nested {projects, totals}
  # format — NOT the same schema. Always use the flat version for CI fallback.
  # CI fallback: ccusage_daily.json is already a public flat export (no paths).
  # ccusage_session_all.json and ccusage_blocks_all.json contain raw sessionIds
  # and MUST NOT be copied verbatim to the public vignettes/data/ directory.
  # Write empty placeholders for the session/blocks public files instead.
  # (privacy regression fix: Issue #1 in roborev audit)
  fallback_daily_src <- file.path(extdata, "ccusage_daily.json")
  fallback_daily_dst <- file.path(out_dir, "ccusage_daily.json")
  if (file.exists(fallback_daily_src)) {
    file.copy(fallback_daily_src, fallback_daily_dst, overwrite = TRUE)
    cat("  -> copied ccusage_daily.json (CI fallback)\n")
  } else if (!file.exists(fallback_daily_dst)) {
    write_json(list(), fallback_daily_dst, auto_unbox = TRUE)
    cat("  -> ccusage_daily.json not found, wrote empty (CI fallback)\n")
  }
  # Always write empty sessions (raw sessionIds must not appear in public output).
  write_json(list(), file.path(out_dir, "ccusage_sessions.json"), auto_unbox = TRUE)
  cat("  -> wrote empty ccusage_sessions.json (CI fallback; cmonitor-rs not available)\n")
  # Blocks: use committed inst/extdata snapshot if present, else write empty array.
  # This keeps the "Time Blocks" dashboard page populated in CI. (#141)
  fallback_blocks_src <- file.path(extdata, "ccusage_blocks.json")
  fallback_blocks_dst <- file.path(out_dir, "ccusage_blocks.json")
  if (file.exists(fallback_blocks_src)) {
    file.copy(fallback_blocks_src, fallback_blocks_dst, overwrite = TRUE)
    cat("  -> copied ccusage_blocks.json from inst/extdata (CI fallback)\n")
  } else {
    write_json(list(), fallback_blocks_dst, auto_unbox = TRUE)
    cat("  -> wrote empty ccusage_blocks.json (CI fallback; no committed snapshot)\n")
  }
}

# --- 4. Export Gemini from DuckDB (unchanged) ----------------------------------
cat("Exporting Gemini data...\n")
gemini_db <- file.path(extdata, "gemini_usage.duckdb")
if (file.exists(gemini_db)) {
  con <- dbConnect(duckdb(), dbdir = gemini_db, read_only = TRUE)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  gem_daily <- dbReadTable(con, "daily_usage") |>
    as_tibble() |>
    mutate(date = as.character(date), total_cost = round(total_cost, 4))
  write_json(gem_daily, file.path(out_dir, "gemini_daily.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d daily rows\n", nrow(gem_daily)))

  gem_sess <- dbReadTable(con, "sessions_summary") |>
    as_tibble() |>
    mutate(
      start_time   = as.character(start_time),
      last_updated = as.character(last_updated),
      total_cost   = round(total_cost, 4)
    )
  write_json(gem_sess, file.path(out_dir, "gemini_sessions.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d sessions\n", nrow(gem_sess)))
} else {
  cat("  -> gemini_usage.duckdb not found, skipping\n")
  write_json(list(), file.path(out_dir, "gemini_daily.json"))
  write_json(list(), file.path(out_dir, "gemini_sessions.json"))
}

# --- 4.5. Refresh and export Codex usage data --------------------------------
# Sources: ~/.codex/log/codex-tui.log (OTEL-structured per-turn token usage)
# Writes:  inst/extdata/codex_daily.json
#          inst/extdata/codex_sessions.json
# Then copies codex_daily.json and codex_sessions.json to vignettes/data/.
cat("Refreshing Codex usage data...\n")
codex_refresh_ok <- FALSE
# Run refresh_codex_cache.R as a SUBPROCESS, not via source(): it calls
# quit(status = 0L) on the "no new turns" path, which — when sourced —
# terminates THIS export process before the dashboard-data sections run.
# A child Rscript isolates that quit().
codex_script <- here::here("inst", "scripts", "refresh_codex_cache.R")
codex_rc <- tryCatch(
  system2("Rscript", args = shQuote(codex_script), stdout = "", stderr = ""),
  error = function(e) {
    cat(sprintf("  -> refresh_codex_cache.R error: %s\n", conditionMessage(e)))
    1L
  }
)
if (identical(as.integer(codex_rc), 0L)) {
  codex_refresh_ok <- TRUE
  cat("  -> refresh_codex_cache.R completed\n")
} else {
  cat(sprintf("  -> refresh_codex_cache.R exited non-zero (%s); continuing export\n", codex_rc))
}

codex_daily_src    <- file.path(extdata, "codex_daily.json")
codex_sessions_src <- file.path(extdata, "codex_sessions.json")

if (file.exists(codex_daily_src)) {
  file.copy(codex_daily_src,    file.path(out_dir, "codex_daily.json"),    overwrite = TRUE)
  cat(sprintf("  -> copied codex_daily.json (%d bytes)\n",
              file.info(codex_daily_src)$size))
} else {
  write_json(list(), file.path(out_dir, "codex_daily.json"), auto_unbox = TRUE)
  cat("  -> codex_daily.json not found, wrote empty placeholder\n")
}

if (file.exists(codex_sessions_src)) {
  file.copy(codex_sessions_src, file.path(out_dir, "codex_sessions.json"), overwrite = TRUE)
  cat(sprintf("  -> copied codex_sessions.json (%d bytes)\n",
              file.info(codex_sessions_src)$size))
} else {
  write_json(list(), file.path(out_dir, "codex_sessions.json"), auto_unbox = TRUE)
  cat("  -> codex_sessions.json not found, wrote empty placeholder\n")
}

# Read back for downstream integration (subsequent pipeline stages can
# reference codex_daily_df and codex_sessions_df if needed).
codex_daily_df <- if (file.exists(codex_daily_src)) {
  tryCatch(
    as_tibble(fromJSON(codex_daily_src, simplifyDataFrame = TRUE)),
    error = function(e) tibble()
  )
} else tibble()
codex_sessions_df <- if (file.exists(codex_sessions_src)) {
  tryCatch(
    as_tibble(fromJSON(codex_sessions_src, simplifyDataFrame = TRUE)),
    error = function(e) tibble()
  )
} else tibble()
cat(sprintf("  -> codex: %d daily rows, %d sessions\n",
            nrow(codex_daily_df), nrow(codex_sessions_df)))

# --- 5. Compute cmonitor summary from cmonitor-rs blocks ----------------------
cat("Exporting cmonitor summary...\n")
active_blocks <- if (has_cmonitor) Filter(function(b) !isTRUE(b$is_gap), blocks_all) else list()
if (length(active_blocks) > 0) {
  total_cost   <- sum(vapply(active_blocks, function(b) b$cost_usd, numeric(1)))
  total_tokens <- sum(vapply(active_blocks, function(b) {
    tok <- b$tokens
    tok$input_tokens + tok$output_tokens +
      tok$cache_creation_tokens + tok$cache_read_tokens
  }, numeric(1)))
  entries      <- sum(vapply(active_blocks, function(b) b$message_count, numeric(1)))

  # Date range from first and last start_time
  dates <- vapply(active_blocks, function(b) {
    st <- b$start_time
    as.character(as.Date(paste0(st[1], "-01-01")) + (st[2] - 1L))
  }, character(1))
  date_range <- paste(min(dates), "to", max(dates))
} else {
  total_cost   <- 0
  total_tokens <- 0
  entries      <- 0
  date_range   <- NA_character_
}

cmon_summary <- list(
  date_range   = date_range,
  total_tokens = total_tokens,
  total_cost   = round(total_cost, 4),
  entries      = entries
)
write_json(cmon_summary, file.path(out_dir, "cmonitor_summary.json"), auto_unbox = TRUE)
cat(sprintf("  -> cost=$%.2f, tokens=%s\n",
  cmon_summary$total_cost, format(cmon_summary$total_tokens, big.mark = ",")))

# --- 6. Extract git commit stats (unchanged) -----------------------------------
cat("Exporting git commit stats...\n")
commits_raw <- system("git log '--format=%H|%ai|%s' --numstat", intern = TRUE)

# Parse the raw git log output into a data frame
# Each commit starts with a line: hash|date|message
# Followed by numstat lines: added\tdeleted\tfile
commit_rows <- list()
current <- NULL
for (line in commits_raw) {
  if (grepl("^[0-9a-f]{40}\\|", line)) {
    # Save previous commit if it exists
    if (!is.null(current)) {
      commit_rows[[length(commit_rows) + 1]] <- current
    }
    parts <- strsplit(line, "\\|", fixed = FALSE)[[1]]
    current <- list(
      hash = substr(parts[1], 1, 7),
      date = as.character(as.Date(parts[2])),
      message = paste(parts[-(1:2)], collapse = "|"),
      lines_added = 0L,
      lines_deleted = 0L,
      files_changed = 0L
    )
  } else if (nzchar(trimws(line)) && !is.null(current)) {
    # numstat line: added\tdeleted\tfile
    fields <- strsplit(line, "\t")[[1]]
    if (length(fields) >= 3) {
      added <- suppressWarnings(as.integer(fields[1]))
      deleted <- suppressWarnings(as.integer(fields[2]))
      if (!is.na(added)) current$lines_added <- current$lines_added + added
      if (!is.na(deleted)) current$lines_deleted <- current$lines_deleted + deleted
      current$files_changed <- current$files_changed + 1L
    }
  }
}
# Don't forget the last commit
if (!is.null(current)) {
  commit_rows[[length(commit_rows) + 1]] <- current
}

if (length(commit_rows) > 0) {
  commits_df <- bind_rows(lapply(commit_rows, as_tibble)) |>
    mutate(lines_changed = lines_added + lines_deleted) |>
    arrange(date)
  write_json(commits_df, file.path(out_dir, "git_commits.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d commits\n", nrow(commits_df)))
} else {
  cat("  -> no commits found, writing empty array\n")
  write_json(list(), file.path(out_dir, "git_commits.json"))
}

# --- 6c. Multi-project commits (cross-repo, last 1 year) ----------------------
cat("Exporting multi-project commits...\n")
tracked_repos <- list(
  llm          = path.expand("~/docs_gh/llm"),
  llmtelemetry = path.expand("~/docs_gh/llmtelemetry"),
  irishbuoys   = path.expand(
    "~/docs_gh/proj/data/weather/irish_buoy_network/irishbuoys"),
  mycare       = path.expand("~/docs_gh/mycare"),
  footbet      = path.expand("~/docs_gh/proj/stats/sport/footbet")
)

parse_git_log <- function(repo_path, project_name) {
  if (!file.exists(file.path(repo_path, ".git"))) return(NULL)
  raw <- tryCatch(
    system(
      sprintf("git -C '%s' log '--format=%%H|%%ai|%%s' --numstat --since='1 year ago'",
              repo_path),
      intern = TRUE
    ),
    error = function(e) character(0)
  )
  if (length(raw) == 0) return(NULL)
  rows <- list(); cur <- NULL
  for (line in raw) {
    if (grepl("^[0-9a-f]{40}\\|", line)) {
      if (!is.null(cur)) rows[[length(rows) + 1]] <- cur
      parts <- strsplit(line, "\\|", fixed = FALSE)[[1]]
      cur <- list(project = project_name, hash = substr(parts[1], 1, 7),
                  date = as.character(as.Date(parts[2])),
                  message = paste(parts[-(1:2)], collapse = "|"),
                  lines_added = 0L, lines_deleted = 0L, files_changed = 0L)
    } else if (nzchar(trimws(line)) && !is.null(cur)) {
      fields <- strsplit(line, "\t")[[1]]
      if (length(fields) >= 3) {
        a <- suppressWarnings(as.integer(fields[1]))
        d <- suppressWarnings(as.integer(fields[2]))
        if (!is.na(a)) cur$lines_added   <- cur$lines_added   + a
        if (!is.na(d)) cur$lines_deleted <- cur$lines_deleted + d
        cur$files_changed <- cur$files_changed + 1L
      }
    }
  }
  if (!is.null(cur)) rows[[length(rows) + 1]] <- cur
  if (length(rows) == 0) return(NULL)
  bind_rows(lapply(rows, as_tibble)) |>
    mutate(lines_changed = lines_added + lines_deleted)
}

proj_commits_list <- lapply(names(tracked_repos), function(proj)
  parse_git_log(tracked_repos[[proj]], proj))
proj_commits_list <- Filter(Negate(is.null), proj_commits_list)

if (length(proj_commits_list) > 0) {
  proj_commits_df <- bind_rows(proj_commits_list) |>
    arrange(date) |>
    mutate(canonical_project = canonicalize_project(project))
  # Sanitize before public commit: replace raw project with canonical, drop orphans (#936)
  proj_commits_df <- sanitize_for_public(proj_commits_df)
  # Exclude confidential/demo projects from public output (#83 Phase B)
  proj_commits_df <- clean_projects(proj_commits_df)
  write_json(proj_commits_df, file.path(out_dir, "git_commits_by_project.json"), auto_unbox = TRUE)
  write_json(proj_commits_df, file.path(extdata, "git_commits_by_project.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d commits across %d projects\n",
              nrow(proj_commits_df), length(unique(proj_commits_df$project))))
} else {
  cat("  -> no repos found, writing empty array\n")
  write_json(list(), file.path(out_dir, "git_commits_by_project.json"))
  write_json(list(), file.path(extdata, "git_commits_by_project.json"))
}

# --- 6d. Weekly commits by project -------------------------------------------
cat("Exporting weekly commits by project...\n")
# Re-aggregate from proj_commits_df (already in memory from 6c above)
# proj_commits_df has: project, date, lines_changed columns
# CI fallback: read from committed inst/extdata/ if not available in memory

if (exists("proj_commits_df") && nrow(proj_commits_df) > 0) {
  weekly_commits <- proj_commits_df |>
    mutate(
      date_parsed   = as.Date(date),
      iso_week      = format(date_parsed, "%G-%V"),
      week_start_date = as.character(date_parsed - as.integer(format(date_parsed, "%u")) + 1L)
    ) |>
    group_by(project, canonical_project, iso_week, week_start_date) |>
    summarise(
      n_commits           = n(),
      total_lines_changed = sum(lines_changed, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(project, iso_week)
  # Sanitize before public commit: replace raw project with canonical, drop orphans (#936)
  weekly_commits <- sanitize_for_public(weekly_commits)
  # Exclude confidential/demo projects from public output (#83 Phase B)
  weekly_commits <- clean_projects(weekly_commits)
  write_json(weekly_commits, file.path(out_dir, "weekly_commits_by_project.json"), auto_unbox = TRUE)
  write_json(weekly_commits, file.path(extdata, "weekly_commits_by_project.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d project-week rows across %d projects\n",
              nrow(weekly_commits), length(unique(weekly_commits$project))))
} else {
  # CI fallback: read committed JSON from inst/extdata/ and re-export
  fbsrc <- file.path(extdata, "git_commits_by_project.json")
  if (file.exists(fbsrc)) {
    fb_df <- fromJSON(fbsrc, simplifyDataFrame = TRUE)
    if (is.data.frame(fb_df) && nrow(fb_df) > 0) {
      # Add canonical_project if not already present (CI fallback path)
      if (!"canonical_project" %in% names(fb_df)) {
        fb_df$canonical_project <- canonicalize_project(fb_df$project)
      }
      weekly_commits <- fb_df |>
        mutate(
          date_parsed     = as.Date(date),
          iso_week        = format(date_parsed, "%G-%V"),
          week_start_date = as.character(date_parsed - as.integer(format(date_parsed, "%u")) + 1L)
        ) |>
        group_by(project, canonical_project, iso_week, week_start_date) |>
        summarise(
          n_commits           = n(),
          total_lines_changed = sum(lines_changed, na.rm = TRUE),
          .groups = "drop"
        ) |>
        arrange(project, iso_week)
      # Sanitize before public commit (#936)
      weekly_commits <- sanitize_for_public(weekly_commits)
      # Exclude confidential/demo projects from public output (#83 Phase B)
      weekly_commits <- clean_projects(weekly_commits)
      write_json(weekly_commits, file.path(out_dir, "weekly_commits_by_project.json"), auto_unbox = TRUE)
      write_json(weekly_commits, file.path(extdata, "weekly_commits_by_project.json"), auto_unbox = TRUE)
      cat(sprintf("  -> %d project-week rows (CI fallback)\n", nrow(weekly_commits)))
    } else {
      write_json(list(), file.path(out_dir, "weekly_commits_by_project.json"))
      write_json(list(), file.path(extdata, "weekly_commits_by_project.json"))
      cat("  -> no commit data found, writing empty\n")
    }
  } else {
    write_json(list(), file.path(out_dir, "weekly_commits_by_project.json"))
    write_json(list(), file.path(extdata, "weekly_commits_by_project.json"))
    cat("  -> no commit source found, writing empty\n")
  }
}

# --- 6e. Cost per commit by project-day --------------------------------------
cat("Exporting cost per commit...\n")
# Source: cost_by_project_estimated.json (daily project cost)
#         git_commits_by_project.json (daily project commits)
# Join on (project, date). Omit days with zero commits (no divide-by-zero).

cost_src <- file.path(extdata, "cost_by_project_estimated.json")
commits_src <- file.path(extdata, "git_commits_by_project.json")

if (file.exists(cost_src) && file.exists(commits_src)) {
  cost_df <- fromJSON(cost_src, simplifyDataFrame = TRUE) |>
    as_tibble() |>
    select(project, date, est_cost)

  # Daily commit counts per project from raw commits
  commits_raw_df <- fromJSON(commits_src, simplifyDataFrame = TRUE) |>
    as_tibble()

  if (nrow(cost_df) > 0 && nrow(commits_raw_df) > 0) {
    daily_n_commits <- commits_raw_df |>
      group_by(project, date) |>
      summarise(n_commits = n(), .groups = "drop")

    cost_per_commit <- cost_df |>
      inner_join(daily_n_commits, by = c("project", "date")) |>
      filter(n_commits > 0, !is.na(est_cost)) |>
      mutate(
        daily_cost_usd      = round(est_cost, 4),
        cost_per_commit_usd = round(est_cost / n_commits, 4),
        # project in cost_df is from unified_sessions (dash-form); apply normalization
        canonical_project   = canonicalize_session_project(project)
      ) |>
      select(project, canonical_project, date, daily_cost_usd, n_commits,
             cost_per_commit_usd) |>
      arrange(project, date)

    # Sanitize before public commit: replace raw project with canonical, drop orphans (#936)
    cost_per_commit <- sanitize_for_public(cost_per_commit)
    # Exclude confidential/demo projects from public output (#83 Phase B)
    cost_per_commit <- clean_projects(cost_per_commit)
    write_json(cost_per_commit, file.path(out_dir, "cost_per_commit.json"), auto_unbox = TRUE)
    write_json(cost_per_commit, file.path(extdata, "cost_per_commit.json"), auto_unbox = TRUE)
    cat(sprintf("  -> %d project-day rows\n", nrow(cost_per_commit)))
  } else {
    write_json(list(), file.path(out_dir, "cost_per_commit.json"))
    write_json(list(), file.path(extdata, "cost_per_commit.json"))
    cat("  -> insufficient data (empty cost or commits), writing empty\n")
  }
} else {
  write_json(list(), file.path(out_dir, "cost_per_commit.json"))
  write_json(list(), file.path(extdata, "cost_per_commit.json"))
  cat("  -> source files missing, writing empty\n")
}

# --- 6f. File churn per project (top 50 files by lines changed, 1 year) -----
cat("Exporting file churn per project...\n")
# For each tracked repo: git log --numstat --since='1 year ago', aggregate per file.

parse_git_numstat_files <- function(repo_path, project_name, since = "1 year ago") {
  if (!file.exists(file.path(repo_path, ".git"))) {
    warning(sprintf("Skipping %s: .git not found at %s", project_name, repo_path))
    return(NULL)
  }
  raw <- tryCatch(
    system(
      sprintf(
        "git -C '%s' log --numstat --since='%s' '--pretty=format:%%H|%%ad' --date=short",
        repo_path, since
      ),
      intern = TRUE
    ),
    error = function(e) {
      warning(sprintf("git log failed for %s: %s", project_name, conditionMessage(e)))
      character(0)
    }
  )
  if (length(raw) == 0) return(NULL)

  # Parse: commit header lines look like "abc123|2026-01-01", numstat lines are "N\tM\tfile"
  file_stats <- list()
  current_date <- NA_character_

  for (line in raw) {
    if (grepl("^[0-9a-f]{40}\\|", line)) {
      parts <- strsplit(line, "\\|")[[1]]
      current_date <- if (length(parts) >= 2) parts[2] else NA_character_
    } else if (nzchar(trimws(line))) {
      fields <- strsplit(line, "\t")[[1]]
      if (length(fields) >= 3) {
        added   <- suppressWarnings(as.integer(fields[1]))
        deleted <- suppressWarnings(as.integer(fields[2]))
        fname   <- fields[3]
        if (!is.na(added) && !is.na(deleted) && nzchar(fname)) {
          key <- fname
          if (is.null(file_stats[[key]])) {
            file_stats[[key]] <- list(
              n_commits = 0L, total_lines_added = 0L,
              total_lines_deleted = 0L, last_changed_date = current_date
            )
          }
          file_stats[[key]]$n_commits          <- file_stats[[key]]$n_commits + 1L
          file_stats[[key]]$total_lines_added  <- file_stats[[key]]$total_lines_added + added
          file_stats[[key]]$total_lines_deleted <- file_stats[[key]]$total_lines_deleted + deleted
          if (!is.na(current_date) &&
              (is.na(file_stats[[key]]$last_changed_date) ||
               current_date > file_stats[[key]]$last_changed_date)) {
            file_stats[[key]]$last_changed_date <- current_date
          }
        }
      }
    }
  }

  if (length(file_stats) == 0) return(NULL)

  result <- bind_rows(lapply(names(file_stats), function(f) {
    s <- file_stats[[f]]
    tibble(
      project             = project_name,
      file                = f,
      n_commits           = s$n_commits,
      total_lines_added   = s$total_lines_added,
      total_lines_deleted = s$total_lines_deleted,
      total_lines_changed = s$total_lines_added + s$total_lines_deleted,
      last_changed_date   = s$last_changed_date
    )
  }))

  # Top 50 files by total_lines_changed
  result |> arrange(desc(total_lines_changed)) |> slice_head(n = 50L)
}

churn_list <- lapply(names(tracked_repos), function(proj)
  parse_git_numstat_files(tracked_repos[[proj]], proj))
churn_list <- Filter(Negate(is.null), churn_list)

if (length(churn_list) > 0) {
  file_churn_df <- bind_rows(churn_list) |>
    arrange(project, desc(total_lines_changed)) |>
    mutate(canonical_project = canonicalize_project(project))
  # Sanitize before public commit: replace raw project with canonical, drop orphans (#936)
  file_churn_df <- sanitize_for_public(file_churn_df)
  # Exclude confidential/demo projects from public output (#83 Phase B)
  file_churn_df <- clean_projects(file_churn_df)
  write_json(file_churn_df, file.path(out_dir, "file_churn.json"), auto_unbox = TRUE)
  write_json(file_churn_df, file.path(extdata, "file_churn.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d files across %d projects\n",
              nrow(file_churn_df), length(unique(file_churn_df$project))))
} else {
  write_json(list(), file.path(out_dir, "file_churn.json"))
  write_json(list(), file.path(extdata, "file_churn.json"))
  cat("  -> no repos available, writing empty\n")
}

# --- 6g. Change coupling (co-changed file pairs per project, 1 year) ---------
cat("Exporting change coupling...\n")
# For each tracked repo: collect commits where >= 2 files changed,
# generate all file pairs (file_a < file_b), count co-changes.
# Keep top 100 pairs per project with n_cochanges >= 3.

parse_git_coupling <- function(repo_path, project_name, since = "1 year ago") {
  if (!file.exists(file.path(repo_path, ".git"))) {
    warning(sprintf("Skipping %s: .git not found at %s", project_name, repo_path))
    return(NULL)
  }
  raw <- tryCatch(
    system(
      sprintf(
        "git -C '%s' log --name-only --since='%s' '--pretty=format:%%H' --diff-filter=AMR",
        repo_path, since
      ),
      intern = TRUE
    ),
    error = function(e) {
      warning(sprintf("git log failed for %s: %s", project_name, conditionMessage(e)))
      character(0)
    }
  )
  if (length(raw) == 0) return(NULL)

  # Group lines by commit hash: a hash line starts a new commit, file names follow
  commits_files <- list()
  current_hash  <- NULL

  for (line in raw) {
    if (grepl("^[0-9a-f]{40}$", line)) {
      current_hash <- line
      commits_files[[current_hash]] <- character(0)
    } else if (!is.null(current_hash) && nzchar(trimws(line))) {
      commits_files[[current_hash]] <- c(commits_files[[current_hash]], trimws(line))
    }
  }

  # Build pair counts
  pair_counts <- list()

  for (files in commits_files) {
    files <- unique(files)
    if (length(files) < 2L) next
    # Use byte-order (radix) sort — consistent with C-locale < used in tests
    files <- sort(files, method = "radix")
    # Generate all pairs — limit files per commit to 20 to avoid O(n^2) blowup
    if (length(files) > 20L) files <- files[seq_len(20L)]
    for (i in seq_len(length(files) - 1L)) {
      for (j in seq(i + 1L, length(files))) {
        pair_key <- paste(files[i], files[j], sep = "|||")
        pair_counts[[pair_key]] <- (pair_counts[[pair_key]] %||% 0L) + 1L
      }
    }
  }

  if (length(pair_counts) == 0L) return(NULL)

  pairs_df <- bind_rows(lapply(names(pair_counts), function(k) {
    parts <- strsplit(k, "|||", fixed = TRUE)[[1]]
    fa <- parts[1]; fb <- parts[2]
    # Enforce file_a < file_b using byte-order comparison (consistent with C-locale tests)
    if (sort(c(fa, fb), method = "radix")[1L] != fa) { tmp <- fa; fa <- fb; fb <- tmp }
    tibble(
      project     = project_name,
      file_a      = fa,
      file_b      = fb,
      n_cochanges = pair_counts[[k]]
    )
  })) |>
    # Re-aggregate in case sort order changed causes duplicate pairs
    group_by(project, file_a, file_b) |>
    summarise(n_cochanges = sum(n_cochanges), .groups = "drop") |>
    filter(n_cochanges >= 3L) |>
    arrange(desc(n_cochanges)) |>
    slice_head(n = 100L) |>
    mutate(
      weight_normalised = if (max(n_cochanges) > 0)
        round(n_cochanges / max(n_cochanges), 4)
      else 0
    )

  if (nrow(pairs_df) == 0L) return(NULL)
  pairs_df
}

coupling_list <- lapply(names(tracked_repos), function(proj)
  parse_git_coupling(tracked_repos[[proj]], proj))
coupling_list <- Filter(Negate(is.null), coupling_list)

if (length(coupling_list) > 0) {
  coupling_df <- bind_rows(coupling_list) |>
    arrange(project, desc(n_cochanges)) |>
    mutate(canonical_project = canonicalize_project(project))
  # Sanitize before public commit: replace raw project with canonical, drop orphans (#936)
  coupling_df <- sanitize_for_public(coupling_df)
  # Exclude confidential/demo projects from public output (#83 Phase B)
  coupling_df <- clean_projects(coupling_df)
  write_json(coupling_df, file.path(out_dir, "change_coupling.json"), auto_unbox = TRUE)
  write_json(coupling_df, file.path(extdata, "change_coupling.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d file pairs across %d projects\n",
              nrow(coupling_df), length(unique(coupling_df$project))))
} else {
  write_json(list(), file.path(out_dir, "change_coupling.json"))
  write_json(list(), file.path(extdata, "change_coupling.json"))
  cat("  -> no repos available, writing empty\n")
}

# --- 6b. git-recon metrics (bus factor, velocity, timing, crisis, churn, bugs)
# Based on https://piechowski.io/post/git-commands-before-reading-code/
# and https://gist.github.com/gadenbuie/463ff1e9f3b0f48cddc44db2224d286b
cat("Exporting git-recon metrics...\n")

# Bus factor: contributor commit counts (all-time + 6 months)
bus_all <- system("git shortlog -sn --no-merges HEAD", intern = TRUE)
bus_6mo <- system('git shortlog -sn --no-merges --since="6 months ago" HEAD', intern = TRUE)
parse_shortlog <- function(lines) {
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) return(data.frame(commits = integer(0), author = character(0)))
  parts <- regmatches(lines, regexec("^\\s*(\\d+)\\s+(.+)$", lines))
  data.frame(
    commits = as.integer(vapply(parts, `[`, "", 2)),
    author = vapply(parts, `[`, "", 3),
    stringsAsFactors = FALSE
  )
}
bus_factor <- list(
  all_time = parse_shortlog(bus_all),
  recent_6mo = parse_shortlog(bus_6mo)
)
write_json(bus_factor, file.path(out_dir, "git_bus_factor.json"), auto_unbox = TRUE)
cat(sprintf("  -> bus factor: %d contributors (all-time), %d (6mo)\n",
  nrow(bus_factor$all_time), nrow(bus_factor$recent_6mo)))

# Velocity: commits per month
velocity_raw <- system("git log --format='%ad' --date=format:'%Y-%m'", intern = TRUE)
if (length(velocity_raw) > 0) {
  vel_tbl <- as.data.frame(table(velocity_raw), stringsAsFactors = FALSE)
  names(vel_tbl) <- c("month", "commits")
  vel_tbl <- vel_tbl[order(vel_tbl$month), ]
} else {
  vel_tbl <- data.frame(month = character(0), commits = integer(0))
}
write_json(vel_tbl, file.path(out_dir, "git_velocity.json"), auto_unbox = TRUE)
cat(sprintf("  -> velocity: %d months\n", nrow(vel_tbl)))

# Commit timing: hour × weekday
timing_raw <- system("git log --format='%ad' --date=format:'%u %H'", intern = TRUE)
if (length(timing_raw) > 0) {
  parts <- strsplit(timing_raw, " ")
  timing_df <- data.frame(
    weekday = as.integer(vapply(parts, `[`, "", 1)),
    hour = as.integer(vapply(parts, `[`, "", 2)),
    stringsAsFactors = FALSE
  )
  timing_agg <- as.data.frame(table(timing_df$weekday, timing_df$hour), stringsAsFactors = FALSE)
  names(timing_agg) <- c("weekday", "hour", "commits")
  timing_agg$weekday <- as.integer(timing_agg$weekday)
  timing_agg$hour <- as.integer(timing_agg$hour)
  day_labels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  timing_agg$day_name <- day_labels[timing_agg$weekday]
} else {
  timing_agg <- data.frame(weekday = integer(0), hour = integer(0),
    commits = integer(0), day_name = character(0))
}
write_json(timing_agg, file.path(out_dir, "git_timing.json"), auto_unbox = TRUE)
cat(sprintf("  -> timing: %d cells\n", nrow(timing_agg)))

# Crisis patterns: reverts, hotfixes, emergency commits (last year)
crisis_raw <- system(
  'git log --oneline --since="1 year ago" --grep="revert\\|hotfix\\|emergency\\|rollback\\|urgent\\|BREAKING" -i',
  intern = TRUE
)
crisis_df <- if (length(crisis_raw) > 0) {
  data.frame(commit = crisis_raw, stringsAsFactors = FALSE)
} else {
  data.frame(commit = character(0))
}
write_json(crisis_df, file.path(out_dir, "git_crisis.json"), auto_unbox = TRUE)
cat(sprintf("  -> crisis: %d commits\n", nrow(crisis_df)))

# Churn hotspots: most-modified files (6 months)
churn_raw <- system(
  'git log --since="6 months ago" --name-only --pretty=format: | sort | uniq -c | sort -rn | head -20',
  intern = TRUE
)
churn_raw <- trimws(churn_raw[nzchar(trimws(churn_raw))])
if (length(churn_raw) > 0) {
  parts <- regmatches(churn_raw, regexec("^\\s*(\\d+)\\s+(.+)$", churn_raw))
  churn_df <- data.frame(
    changes = as.integer(vapply(parts, `[`, "", 2)),
    file = vapply(parts, `[`, "", 3),
    stringsAsFactors = FALSE
  )
} else {
  churn_df <- data.frame(changes = integer(0), file = character(0))
}
write_json(churn_df, file.path(out_dir, "git_churn.json"), auto_unbox = TRUE)
cat(sprintf("  -> churn: %d hotspot files\n", nrow(churn_df)))

# Bug hotspots: files frequently touched by fix/bug commits (6 months)
bug_raw <- system(
  'git log --since="6 months ago" --name-only --pretty=format: --grep="fix\\|bug\\|broken\\|patch" -i | sort | uniq -c | sort -rn | head -15',
  intern = TRUE
)
bug_raw <- trimws(bug_raw[nzchar(trimws(bug_raw))])
if (length(bug_raw) > 0) {
  parts <- regmatches(bug_raw, regexec("^\\s*(\\d+)\\s+(.+)$", bug_raw))
  bug_df <- data.frame(
    fixes = as.integer(vapply(parts, `[`, "", 2)),
    file = vapply(parts, `[`, "", 3),
    stringsAsFactors = FALSE
  )
} else {
  bug_df <- data.frame(fixes = integer(0), file = character(0))
}
write_json(bug_df, file.path(out_dir, "git_bugs.json"), auto_unbox = TRUE)
cat(sprintf("  -> bugs: %d hotspot files\n", nrow(bug_df)))

# TODO/FIXME debt
todo_raw <- system(
  'grep -rn "TODO\\|FIXME\\|HACK\\|XXX" --include="*.R" --include="*.qmd" --include="*.yaml" --include="*.yml" -c 2>/dev/null | grep -v ":0$" | sort -t: -k2 -rn | head -20',
  intern = TRUE
)
if (length(todo_raw) > 0) {
  parts <- strsplit(todo_raw, ":")
  todo_df <- data.frame(
    file = vapply(parts, `[`, "", 1),
    count = as.integer(vapply(parts, `[`, "", 2)),
    stringsAsFactors = FALSE
  )
} else {
  todo_df <- data.frame(file = character(0), count = integer(0))
}
write_json(todo_df, file.path(out_dir, "git_todo.json"), auto_unbox = TRUE)
cat(sprintf("  -> TODO/FIXME: %d files with debt\n", nrow(todo_df)))

# Tags / releases
tag_raw <- system("git tag -l --format='%(refname:short)|%(creatordate:short)' --sort=-creatordate", intern = TRUE)
if (length(tag_raw) > 0) {
  parts <- strsplit(tag_raw, "\\|")
  tag_df <- data.frame(
    tag = vapply(parts, `[`, "", 1),
    date = vapply(parts, function(p) if (length(p) >= 2) p[2] else NA_character_, ""),
    stringsAsFactors = FALSE
  )
} else {
  tag_df <- data.frame(tag = character(0), date = character(0))
}
write_json(tag_df, file.path(out_dir, "git_tags.json"), auto_unbox = TRUE)
cat(sprintf("  -> tags: %d releases\n", nrow(tag_df)))

# --- 7. Export GitHub issue events (Layer 4 scope change tracking) -----------
cat("Exporting GitHub issue events...\n")
gh_events_src <- file.path(extdata, "github_issue_events.json")
gh_events_dst <- file.path(out_dir, "github_issue_events.json")
if (file.exists(gh_events_src)) {
  file.copy(gh_events_src, gh_events_dst, overwrite = TRUE)
  gh_data <- fromJSON(gh_events_src, simplifyDataFrame = TRUE)
  cat(sprintf("  -> %d issue events copied from inst/extdata\n", nrow(gh_data)))
} else {
  # Write empty placeholder
  write_json(list(), gh_events_dst)
  cat("  -> github_issue_events.json: wrote empty (run poll_github_events.R to populate)\n")
}

# --- 8. Export unified.duckdb sessions and costs ------------------------------
# Local-only data: write to inst/extdata/ (committed) + vignettes/data/ (preview)
# CI fallback: copy from inst/extdata/ to vignettes/data/
cat("Exporting unified.duckdb data...\n")
unified_db <- file.path(Sys.getenv("HOME"), ".claude/logs/unified.duckdb")
if (file.exists(unified_db)) {
  ucon <- dbConnect(duckdb(), dbdir = unified_db, read_only = TRUE)
  on.exit(dbDisconnect(ucon, shutdown = TRUE), add = TRUE)

  raw_sess <- dbReadTable(ucon, "sessions") |> as_tibble()
  n_inv <- sum(!is.na(raw_sess$ended_at) & raw_sess$ended_at < raw_sess$started_at,
               na.rm = TRUE)
  if (n_inv > 0L)
    message(sprintf("  WARN: %d session(s) have ended_at < started_at — clamping to started_at (#111)", n_inv))
  u_sess <- raw_sess |>
    mutate(
      # Normalize ended_at: ccusage occasionally exports ended_at < started_at
      # (UTC vs local timezone mismatch). Clamp to started_at before converting
      # to character so both fields stay monotonic.
      ended_at = as.character(pmax(ended_at, started_at)),
      started_at = as.character(started_at),
      duration_min = ifelse(is.na(duration_min), NA_real_, pmax(0, round(duration_min, 1)))
    ) |>
    select(session_id, project, started_at, ended_at, duration_min) |>
    arrange(desc(started_at)) |>
    mutate(
      # project is stored in dash-form by unified.duckdb hooks;
      # canonicalize_session_project() handles the dash→slash→canonical chain.
      canonical_project = canonicalize_session_project(project)
    )
  # Sanitize before public commit: replace raw project with canonical, drop
  # orphans, remove session_id (raw filesystem path) (#936)
  u_sess_pub <- sanitize_for_public(u_sess)
  # Exclude confidential/demo projects from public output (#83 Phase B)
  u_sess_pub <- clean_projects(u_sess_pub)
  cat(sprintf("  -> dropped %d orphan/worktree sessions; %d kept\n",
              nrow(u_sess) - nrow(u_sess_pub), nrow(u_sess_pub)))
  # Write to both locations: inst/extdata (commit) + vignettes/data (preview)
  write_json(u_sess_pub, file.path(extdata, "unified_sessions.json"), auto_unbox = TRUE)
  write_json(u_sess_pub, file.path(out_dir, "unified_sessions.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d sessions (written to inst/extdata + vignettes/data)\n", nrow(u_sess_pub)))

  u_costs <- dbReadTable(ucon, "costs") |>
    as_tibble() |>
    mutate(date = as.character(date),
           across(where(is.numeric), \(x) round(x, 2)))
  write_json(u_costs, file.path(extdata, "unified_costs.json"), auto_unbox = TRUE)
  write_json(u_costs, file.path(out_dir, "unified_costs.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d cost rows (written to inst/extdata + vignettes/data)\n", nrow(u_costs)))
} else {
  # CI fallback: copy from inst/extdata/ to vignettes/data/
  for (f in c("unified_sessions.json", "unified_costs.json")) {
    src <- file.path(extdata, f)
    dst <- file.path(out_dir, f)
    if (file.exists(src)) {
      file.copy(src, dst, overwrite = TRUE)
      cat(sprintf("  -> %s: copied from inst/extdata\n", f))
    } else if (!file.exists(dst)) {
      write_json(list(), dst)
      cat(sprintf("  -> %s: wrote empty (no source anywhere)\n", f))
    } else {
      cat(sprintf("  -> %s: preserved existing\n", f))
    }
  }
}

# --- 9. Estimated cost by project (session-weighted) -------------------------
cat("Estimating per-project costs using session duration weights...\n")
if (exists("u_sess") && nrow(u_sess) > 0 && exists("daily_rows") && nrow(daily_rows) > 0) {
  u_sess_df <- u_sess
  u_sess_df$date <- as.Date(u_sess_df$started_at)
  sess_daily <- aggregate(duration_min ~ date + project, data = u_sess_df, FUN = sum)
  total_daily <- aggregate(duration_min ~ date, data = sess_daily, FUN = sum)
  names(total_daily)[2] <- "total_min"

  cost_proj <- merge(sess_daily, total_daily, by = "date")
  cost_proj <- merge(cost_proj, daily_rows[, c("date", "totalCost")], by = "date")
  # Filter out rows where totalCost is NA or total_min is 0 (would produce NaN share)
  cost_proj <- cost_proj[!is.na(cost_proj$totalCost) & cost_proj$total_min > 0, ]
  cost_proj$share <- cost_proj$duration_min / cost_proj$total_min
  cost_proj$est_cost <- round(cost_proj$totalCost * cost_proj$share, 4)

  out_data <- cost_proj[, c("date", "project", "est_cost", "duration_min", "share")]
  # project here comes from unified_sessions which is in dash-form
  out_data$canonical_project <- canonicalize_session_project(out_data$project)
  # Re-aggregate after canonicalization: multiple raw aliases may collapse to same
  # canonical name on the same date, creating duplicate (canonical_project, date) rows (#108)
  out_data <- aggregate(
    cbind(est_cost, duration_min, share) ~ date + canonical_project,
    data = out_data[!is.na(out_data$canonical_project), ],
    FUN = sum
  )
  out_data$project <- out_data$canonical_project
  # Sanitize before public commit: replace raw project with canonical, drop orphans (#936)
  out_data <- sanitize_for_public(out_data)
  # Exclude confidential/demo projects from public output (#83 Phase B)
  out_data <- clean_projects(out_data)
  write_json(out_data, file.path(out_dir, "cost_by_project_estimated.json"), auto_unbox = TRUE)
  write_json(out_data, file.path(extdata, "cost_by_project_estimated.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d project-day cost estimates (written to inst/extdata + vignettes/data)\n", nrow(out_data)))

  # --- Tokens by project (same duration-weight approach as cost estimates) ------
  if (exists("daily_rows") && nrow(daily_rows) > 0) {
    cat("Estimating per-project tokens using session duration weights...\n")
    tok_proj <- merge(cost_proj,
                      daily_rows[, c("date", "totalTokens", "inputTokens",
                                     "outputTokens", "cacheCreation", "cacheRead")],
                      by = "date")
    tok_proj <- tok_proj[!is.na(tok_proj$totalTokens) & tok_proj$total_min > 0, ]
    tok_proj$est_total_tokens   <- round(tok_proj$totalTokens   * tok_proj$share, 0)
    tok_proj$est_input_tokens   <- round(tok_proj$inputTokens   * tok_proj$share, 0)
    tok_proj$est_output_tokens  <- round(tok_proj$outputTokens  * tok_proj$share, 0)
    tok_proj$est_cache_creation <- round(tok_proj$cacheCreation * tok_proj$share, 0)
    tok_proj$est_cache_read     <- round(tok_proj$cacheRead     * tok_proj$share, 0)
    out_tok <- tok_proj[, c("date", "project", "est_total_tokens", "est_input_tokens",
                             "est_output_tokens", "est_cache_creation", "est_cache_read")]
    out_tok$canonical_project <- canonicalize_session_project(out_tok$project)
    # Re-aggregate after canonicalization: multiple raw aliases may collapse to same
    # canonical name on the same date, creating duplicate (canonical_project, date) rows (#108)
    out_tok <- aggregate(
      cbind(est_total_tokens, est_input_tokens, est_output_tokens,
            est_cache_creation, est_cache_read) ~ date + canonical_project,
      data = out_tok[!is.na(out_tok$canonical_project), ],
      FUN = sum
    )
    out_tok$project <- out_tok$canonical_project
    out_tok <- sanitize_for_public(out_tok)
    # Exclude confidential/demo projects from public output (#83 Phase B)
    out_tok <- clean_projects(out_tok)
    write_json(out_tok, file.path(out_dir, "tokens_by_project.json"), auto_unbox = TRUE)
    write_json(out_tok, file.path(extdata, "tokens_by_project.json"), auto_unbox = TRUE)
    cat(sprintf("  -> %d project-day token estimates (written to inst/extdata + vignettes/data)\n",
                nrow(out_tok)))
  }
} else {
  # CI fallback: copy from inst/extdata/ or write empty
  f <- "cost_by_project_estimated.json"
  src <- file.path(extdata, f)
  dst <- file.path(out_dir, f)
  if (file.exists(src)) {
    file.copy(src, dst, overwrite = TRUE)
    cat(sprintf("  -> %s: copied from inst/extdata\n", f))
  } else if (!file.exists(dst)) {
    write_json(list(), dst)
    cat(sprintf("  -> %s: wrote empty (no source anywhere)\n", f))
  } else {
    cat(sprintf("  -> %s: preserved existing\n", f))
  }
  for (f in c("tokens_by_project.json")) {
    src <- file.path(extdata, f); dst <- file.path(out_dir, f)
    if (file.exists(src)) { file.copy(src, dst, overwrite = TRUE)
    } else { write_json(list(), dst) }
  }
}

# --- 10. Export prediction calibration data -----------------------------------
cat("Exporting prediction calibration data...\n")
pred_dir <- file.path(Sys.getenv("HOME"), ".claude/predictions")
pred_files <- list.files(pred_dir, pattern = "\\.jsonl$", full.names = TRUE)

if (length(pred_files) > 0) {
  # Read all JSONL lines, parse each as JSON
  all_lines <- unlist(lapply(pred_files, readLines))
  all_lines <- all_lines[nzchar(trimws(all_lines))]

  if (length(all_lines) > 0) {
    preds_list <- lapply(all_lines, function(l) {
      tryCatch(fromJSON(l), error = function(e) NULL)
    })
    preds_list <- Filter(Negate(is.null), preds_list)

    if (length(preds_list) > 0) {
      # Convert NULL to NA for nullable fields (outcome, outcome_recorded_at, outcome_notes)
      # This is required because JSON null becomes R NULL which can't be a tibble column
      preds_list <- lapply(preds_list, function(x) {
        if (is.null(x$outcome)) x$outcome <- NA
        if (is.null(x$outcome_recorded_at)) x$outcome_recorded_at <- NA_character_
        if (is.null(x$outcome_notes)) x$outcome_notes <- NA_character_
        x
      })
      preds_df <- bind_rows(lapply(preds_list, as_tibble))

      # Deduplicate: keep last record per prediction_id (outcome overwrites predict)
      preds_df <- preds_df |>
        group_by(prediction_id) |>
        filter(row_number() == n()) |>
        ungroup()

      # Resolved predictions (outcome recorded)
      resolved <- preds_df |> filter(!is.na(outcome))
      pending <- preds_df |> filter(is.na(outcome))

      # Convert outcome to numeric for Brier score
      if (nrow(resolved) > 0) {
        resolved <- resolved |>
          mutate(
            outcome_num = case_when(
              outcome == TRUE ~ 1, outcome == "true" ~ 1,
              outcome == "partial" ~ 0.5,
              TRUE ~ 0
            ),
            brier = (p_success - outcome_num)^2
          )
      }

      # Calibration buckets
      if (nrow(resolved) > 0) {
        resolved$bucket <- cut(resolved$p_success,
          breaks = c(0, 0.5, 0.7, 0.9, 1.01),
          labels = c("0-50%", "50-70%", "70-90%", "90-100%"),
          right = FALSE)
        cal_buckets <- resolved |>
          group_by(bucket) |>
          summarise(
            n = n(),
            mean_predicted = round(mean(p_success, na.rm = TRUE), 3),
            actual_accuracy = round(mean(outcome_num, na.rm = TRUE), 3),
            mean_brier = round(mean(brier, na.rm = TRUE), 4),
            .groups = "drop"
          )
      } else {
        cal_buckets <- tibble(bucket = character(0), n = integer(0),
          mean_predicted = numeric(0), actual_accuracy = numeric(0),
          mean_brier = numeric(0))
      }

      # Write outputs to both locations: inst/extdata (commit) + vignettes/data (preview)
      preds_out <- resolved |> mutate(across(where(is.numeric), ~round(.x, 4)))
      # Sanitize: drop project_slug column (raw filesystem path, e.g.
      # "-Users-johngavin-docs-gh-proj-data-weather-irish-buoy-network").
      # project_name is already the clean canonical identifier (#936).
      if ("project_slug" %in% names(preds_out)) preds_out$project_slug <- NULL
      write_json(preds_out, file.path(extdata, "predictions.json"), auto_unbox = TRUE)
      write_json(preds_out, file.path(out_dir, "predictions.json"), auto_unbox = TRUE)
      write_json(cal_buckets, file.path(extdata, "calibration_buckets.json"), auto_unbox = TRUE)
      write_json(cal_buckets, file.path(out_dir, "calibration_buckets.json"), auto_unbox = TRUE)

      cat(sprintf("  -> %d resolved, %d pending, %d buckets (written to inst/extdata + vignettes/data)\n",
        nrow(resolved), nrow(pending), nrow(cal_buckets)))
    } else {
      cat("  -> no valid JSONL records\n")
    }
  } else {
    cat("  -> JSONL files empty\n")
  }
} else {
  cat("  -> no prediction files found\n")
}
# CI fallback: copy prediction data from inst/extdata/ to vignettes/data/
for (f in c("predictions.json", "calibration_buckets.json")) {
  src <- file.path(extdata, f)
  dst <- file.path(out_dir, f)
  if (!file.exists(dst)) {
    if (file.exists(src)) {
      file.copy(src, dst, overwrite = TRUE)
      cat(sprintf("  -> %s: copied from inst/extdata\n", f))
    } else {
      write_json(list(), dst)
      cat(sprintf("  -> %s: wrote empty (no source anywhere)\n", f))
    }
  }
}

# --- 11. Generate API index.json ----------------------------------------------
cat("Generating index.json...\n")
api_index <- list(
  version  = "1.0.0",
  base_url = "https://johngavin.github.io/llmtelemetry/data",
  updated  = format(Sys.Date(), "%Y-%m-%d"),
  endpoints = list(
    list(
      path        = "/ccusage_daily.json",
      description = "Daily Claude API usage aggregated by date (last 90 days)",
      type        = "array",
      source      = "cmonitor-rs CLI (--view daily --output json --since 90d)",
      source_url  = "https://github.com/anthropics/cmonitor",
      schema      = list(
        date          = "string",
        inputTokens   = "integer",
        outputTokens  = "integer",
        cacheCreation = "integer",
        cacheRead     = "integer",
        totalTokens   = "integer",
        totalCost     = "number",
        modelsUsed    = "string"
      )
    ),
    list(
      path        = "/ccusage_sessions.json",
      description = "Claude API sessions (empty — cmonitor-rs has no session view)",
      type        = "array",
      source      = "cmonitor-rs CLI (no session data available)",
      source_url  = "https://github.com/anthropics/cmonitor",
      schema      = list()
    ),
    list(
      path        = "/ccusage_blocks.json",
      description = "Claude API usage grouped into contiguous time blocks",
      type        = "array",
      source      = "cmonitor-rs CLI (--view daily --output json --since 90d)",
      source_url  = "https://github.com/anthropics/cmonitor",
      schema      = list(
        startTime     = "string",
        endTime       = "string",
        actualEndTime = "string",
        entries       = "integer",
        inputTokens   = "integer",
        outputTokens  = "integer",
        cacheCreation = "integer",
        cacheRead     = "integer",
        totalTokens   = "integer",
        costUSD       = "number",
        models        = "string"
      )
    ),
    list(
      path        = "/gemini_daily.json",
      description = "Daily Gemini API usage with token and cost totals",
      type        = "array",
      source      = "Local Gemini session logs (~/.gemini/tmp/), DuckDB",
      source_url  = "https://ai.google.dev/",
      schema      = list(
        date          = "string",
        total_tokens  = "integer",
        total_cost    = "number",
        message_count = "integer"
      )
    ),
    list(
      path        = "/gemini_sessions.json",
      description = "Gemini API sessions with token and cost totals",
      type        = "array",
      source      = "Local Gemini session logs (~/.gemini/tmp/), DuckDB",
      source_url  = "https://ai.google.dev/",
      schema      = list(
        sessionId    = "string",
        project      = "string",
        total_tokens = "integer",
        total_cost   = "number",
        start_time   = "string",
        last_updated = "string"
      )
    ),
    list(
      path        = "/cmonitor_summary.json",
      description = "Aggregated Claude usage summary computed from cmonitor-rs blocks",
      type        = "object",
      source      = "cmonitor-rs CLI (--view daily --output json --since 90d)",
      source_url  = "https://github.com/anthropics/cmonitor",
      schema      = list(
        date_range   = "string",
        total_tokens = "integer",
        total_cost   = "number",
        entries      = "integer"
      )
    ),
    list(
      path        = "/git_commits.json",
      description = "Lines changed per git commit with additions and deletions",
      type        = "array",
      source      = "git log --numstat",
      source_url  = "https://git-scm.com/docs/git-log",
      schema      = list(
        hash          = "string",
        date          = "string",
        message       = "string",
        lines_added   = "integer",
        lines_deleted = "integer",
        lines_changed = "integer",
        files_changed = "integer"
      )
    ),
    list(
      path        = "/git_commits_by_project.json",
      description = "Per-commit data across all tracked projects (last 1 year)",
      type        = "array",
      source      = "git log --numstat per tracked repo",
      source_url  = "https://git-scm.com/docs/git-log",
      schema      = list(
        project       = "string",
        hash          = "string",
        date          = "string",
        message       = "string",
        lines_added   = "integer",
        lines_deleted = "integer",
        lines_changed = "integer",
        files_changed = "integer"
      )
    ),
    list(
      path        = "/unified_sessions.json",
      description = "Claude Code sessions from unified.duckdb hook telemetry",
      type        = "array",
      source      = "~/.claude/logs/unified.duckdb (sessions table)",
      schema      = list(
        session_id  = "string",
        project     = "string",
        started_at  = "string",
        ended_at    = "string",
        duration_min = "number"
      )
    ),
    list(
      path        = "/git_bus_factor.json",
      description = "Contributor commit counts for bus factor analysis",
      type        = "object",
      source      = "git shortlog -sn"
    ),
    list(
      path        = "/git_velocity.json",
      description = "Commits per month for velocity trend",
      type        = "array",
      source      = "git log --date=format:%Y-%m"
    ),
    list(
      path        = "/git_timing.json",
      description = "Commit counts by weekday and hour for timing heatmap",
      type        = "array",
      source      = "git log --date=format:%u_%H"
    ),
    list(
      path        = "/git_crisis.json",
      description = "Revert/hotfix/emergency commits in last year",
      type        = "array",
      source      = "git log --grep=revert|hotfix|emergency"
    ),
    list(
      path        = "/git_churn.json",
      description = "Top 20 most-modified files in last 6 months",
      type        = "array",
      source      = "git log --name-only | sort | uniq -c"
    ),
    list(
      path        = "/git_bugs.json",
      description = "Top 15 files touched by fix/bug commits in last 6 months",
      type        = "array",
      source      = "git log --grep=fix|bug --name-only | sort | uniq -c"
    ),
    list(
      path        = "/git_todo.json",
      description = "TODO/FIXME/HACK/XXX debt counts by file",
      type        = "array",
      source      = "grep -rn TODO|FIXME|HACK|XXX"
    ),
    list(
      path        = "/git_tags.json",
      description = "Release tags with dates",
      type        = "array",
      source      = "git tag -l --sort=-creatordate"
    ),
    list(
      path        = "/github_issue_events.json",
      description = "GitHub issue events for scope change tracking (Layer 4)",
      type        = "array",
      source      = "inst/scripts/poll_github_events.R (gh api via gh CLI)",
      schema      = list(
        event_id     = "number",
        project      = "string",
        event_type   = "string",
        issue_number = "number",
        issue_title  = "string",
        created_at   = "string (ISO 8601)",
        actor        = "string"
      )
    ),
    list(
      path        = "/unified_costs.json",
      description = "Daily costs with model breakdown from unified.duckdb",
      type        = "array",
      source      = "~/.claude/logs/unified.duckdb (costs table)",
      schema      = list(
        date        = "string",
        opus_cost   = "number",
        sonnet_cost = "number",
        haiku_cost  = "number",
        total_cost  = "number",
        opus_pct    = "number",
        sonnet_pct  = "number",
        haiku_pct   = "number"
      )
    ),
    list(
      path        = "/predictions.json",
      description = "Resolved predictions with outcomes for calibration analysis",
      type        = "array",
      source      = "~/.claude/predictions/*.jsonl"
    ),
    list(
      path        = "/calibration_buckets.json",
      description = "Calibration by confidence bucket (predicted vs actual accuracy)",
      type        = "array",
      source      = "Computed from predictions.json"
    ),
    list(
      path        = "/weekly_commits_by_project.json",
      description = "Weekly commit counts and lines changed per project (last 1 year)",
      type        = "array",
      source      = "Re-aggregated from git_commits_by_project.json",
      schema      = list(
        project             = "string",
        iso_week            = "string (YYYY-WW)",
        week_start_date     = "string",
        n_commits           = "integer",
        total_lines_changed = "integer"
      )
    ),
    list(
      path        = "/cost_per_commit.json",
      description = "Daily estimated cost and commit count per project with cost-per-commit",
      type        = "array",
      source      = "Join of cost_by_project_estimated.json x git_commits_by_project.json",
      schema      = list(
        project             = "string",
        date                = "string",
        daily_cost_usd      = "number",
        n_commits           = "integer",
        cost_per_commit_usd = "number"
      )
    ),
    list(
      path        = "/file_churn.json",
      description = "Top 50 files per project by lines changed (last 1 year)",
      type        = "array",
      source      = "git log --numstat --since='1 year ago' per tracked repo",
      schema      = list(
        project             = "string",
        file                = "string",
        n_commits           = "integer",
        total_lines_added   = "integer",
        total_lines_deleted = "integer",
        total_lines_changed = "integer",
        last_changed_date   = "string"
      )
    ),
    list(
      path        = "/change_coupling.json",
      description = "Co-changed file pairs per project (top 100 pairs, n_cochanges >= 3)",
      type        = "array",
      source      = "git log --name-only --diff-filter=AMR per tracked repo",
      schema      = list(
        project           = "string",
        file_a            = "string",
        file_b            = "string",
        n_cochanges       = "integer",
        weight_normalised = "number"
      )
    ),
    list(
      path        = "/roborev_summary.json",
      description = "Roborev code-review pipeline health: pulse KPIs + active loops",
      type        = "object",
      source      = "~/.claude/logs/unified.duckdb (roborev_review_lifecycle, roborev_loops)",
      schema      = list(
        pulse = list(
          n_open     = "integer",
          n_critical = "integer",
          n_high     = "integer",
          n_resolved = "integer",
          n_loops    = "integer",
          n_escalate = "integer",
          wasted_usd = "number"
        ),
        loops = "array of {tier, cycles, severity, primary_file, summary, first_seen, last_seen, estimated_wasted_usd}"
      )
    ),
    list(
      path        = "/roborev_repo_signal.json",
      description = "Per-repo roborev signal-to-noise ratio (#146 Q3): signal_ratio = passed/created, noise_ratio = failed/created",
      type        = "object",
      source      = "~/.claude/logs/unified.duckdb (roborev_daily_metrics, aggregated per repo)",
      schema      = list(
        repos = "array of {repo, n_created, n_passed, n_failed, n_autoclosed, n_parse_fail, signal_ratio, noise_ratio}",
        generated_at = "string (ISO 8601)"
      )
    ),
    list(
      path        = "/roborev_cost_per_finding.json",
      description = "Cost per finding actually resolved, per agent, over time (#146 Q1). cost_instrumented=false until total_cost_usd is populated.",
      type        = "object",
      source      = "~/.claude/logs/unified.duckdb (roborev_review_lifecycle + roborev_agent_performance)",
      schema      = list(
        series            = "array of {agent, date, pass_count, addressed_count, cost_usd_per_finding}",
        summary           = "array of {agent, total_pass, total_addressed, cost_usd_per_finding}",
        cost_instrumented = "boolean",
        generated_at      = "string (ISO 8601)"
      )
    ),
    list(
      path        = "/roborev_cost_per_agent.json",
      description = "Cheapest agent per resolved finding (#146 Q11). cost_instrumented=false until total_cost_usd is populated.",
      type        = "object",
      source      = "~/.claude/logs/unified.duckdb (roborev_review_lifecycle + roborev_agent_performance)",
      schema      = list(
        agents            = "array of {agent, total_pass, total_addressed, cost_usd_per_finding}",
        cost_instrumented = "boolean",
        generated_at      = "string (ISO 8601)"
      )
    ),
    list(
      path        = "/roborev_vs_session_cost.json",
      description = "Session execution cost vs roborev review cost, daily (#146 Q20). cost_instrumented=false until review costs are tracked.",
      type        = "object",
      source      = "~/.claude/logs/unified.duckdb (costs + roborev_agent_performance)",
      schema      = list(
        daily             = "array of {date, session_cost_usd, review_cost_usd, review_pct}",
        overall           = "object {session_total, review_total, review_pct}",
        cost_instrumented = "boolean",
        note              = "string",
        generated_at      = "string (ISO 8601)"
      )
    )
  )
)
write_json(api_index, file.path(out_dir, "index.json"), auto_unbox = TRUE, pretty = TRUE)
cat("  -> index.json written\n")

# --- 8b. Export git file growth from git pulse parquet -----------------------
cat("Exporting git file growth data...\n")
git_pulse_dir <- file.path(Sys.getenv("HOME"), ".claude/logs/git")
parquet_files <- list.files(git_pulse_dir, pattern = "\\.parquet$", full.names = TRUE)

if (length(parquet_files) > 0 && requireNamespace("arrow", quietly = TRUE)) {
  library(arrow)
  # Read all parquet files and combine
  all_pulse <- lapply(parquet_files, function(f) {
    tryCatch(read_parquet(f), error = function(e) NULL)
  })
  all_pulse <- do.call(rbind, Filter(Negate(is.null), all_pulse))

  if (!is.null(all_pulse) && nrow(all_pulse) > 0) {
    # Filter for 6-month file growth metrics (these are aggregates, not daily)
    file_metrics <- all_pulse[
      all_pulse$metric %in% c("files_added", "files_deleted", "files_net_growth") &
      all_pulse$period == "6mo",
    ]

    if (nrow(file_metrics) > 0) {
      # Convert value to numeric
      file_metrics$value <- as.numeric(file_metrics$value)

      # Use snapshot_date as the time axis, period_label indicates metric type
      # Pivot: one row per snapshot_date-project with columns for each metric type
      dates_projects <- unique(file_metrics[, c("snapshot_date", "project")])
      file_growth <- dates_projects
      names(file_growth)[1] <- "date"

      # Map period_label to metric type (growth=files_added, cleanup=files_deleted, net=files_net_growth)
      file_metrics$metric_type <- ifelse(file_metrics$period_label == "growth", "files_added",
                                  ifelse(file_metrics$period_label == "cleanup", "files_deleted",
                                  "files_net_growth"))

      # Add columns for each metric
      for (m in c("files_added", "files_deleted", "files_net_growth")) {
        metric_data <- file_metrics[file_metrics$metric_type == m, ]
        key <- paste(metric_data$snapshot_date, metric_data$project, sep = "___")
        vals <- setNames(metric_data$value, key)
        file_growth[[m]] <- vals[paste(file_growth$date, file_growth$project, sep = "___")]
        file_growth[[m]][is.na(file_growth[[m]])] <- 0
      }

      # Ensure numeric types
      file_growth$files_added <- as.integer(file_growth$files_added)
      file_growth$files_deleted <- as.integer(file_growth$files_deleted)
      file_growth$files_net_growth <- as.integer(file_growth$files_net_growth)

      # Sort by date
      file_growth <- file_growth[order(file_growth$date), ]

      write_json(file_growth, file.path(out_dir, "git_file_growth.json"), auto_unbox = TRUE)
      cat(sprintf("  -> %d date-project rows\n", nrow(file_growth)))
    } else {
      cat("  -> no file growth metrics found in parquet files\n")
      write_json(list(), file.path(out_dir, "git_file_growth.json"))
    }
  } else {
    cat("  -> could not read parquet files\n")
    write_json(list(), file.path(out_dir, "git_file_growth.json"))
  }
} else {
  if (length(parquet_files) > 0) {
    cat("  -> arrow package not available; falling back to inst/extdata/ snapshot\n")
  } else {
    cat("  -> no parquet files found\n")
  }
  # CI fallback: copy from inst/extdata/ if available
  extdata_fallback <- file.path(extdata, "git_file_growth.json")
  if (file.exists(extdata_fallback)) {
    fg_data <- fromJSON(extdata_fallback, simplifyDataFrame = TRUE)
    if (is.data.frame(fg_data) && nrow(fg_data) > 0) {
      file.copy(extdata_fallback, file.path(out_dir, "git_file_growth.json"), overwrite = TRUE)
      cat(sprintf("  -> CI fallback: copied %d rows from inst/extdata/\n", nrow(fg_data)))
    } else {
      write_json(list(), file.path(out_dir, "git_file_growth.json"))
    }
  } else {
    write_json(list(), file.path(out_dir, "git_file_growth.json"))
  }
}

# --- 10b. Build projects_master.json — stable canonical project list -----------
# Collects canonical project names + date ranges across ALL project-bearing
# exports. This list is time-filter-independent: the dashboard sidebar uses it
# as the stable universe of projects regardless of the selected date window.
cat("Building projects_master.json...\n")

# Helper to extract (canonical_project, date, source) rows from a committed JSON,
# reading canonical_project if present, otherwise deriving from 'project'.
# source_id is a short label for this data source (e.g. "unified_sessions").
extract_cp_dates <- function(json_path, date_col, proj_col = "project",
                              cp_col = "canonical_project",
                              dash_form = FALSE, source_id = basename(json_path)) {
  if (!file.exists(json_path)) return(NULL)
  df <- tryCatch(
    fromJSON(json_path, simplifyDataFrame = TRUE),
    error = function(e) NULL
  )
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(NULL)
  if (!date_col %in% names(df)) return(NULL)
  if (!proj_col %in% names(df)) return(NULL)

  # Derive canonical_project: prefer stored column; fall back to helper
  if (cp_col %in% names(df)) {
    cp <- df[[cp_col]]
  } else if (dash_form) {
    cp <- canonicalize_session_project(df[[proj_col]])
  } else {
    cp <- canonicalize_project(df[[proj_col]])
  }

  dates_chr <- as.character(df[[date_col]])
  data.frame(
    canonical_project = cp,
    date              = dates_chr,
    source            = source_id,
    stringsAsFactors  = FALSE
  )
}

pm_sources <- list(
  extract_cp_dates(file.path(extdata, "unified_sessions.json"),
                   date_col = "started_at", proj_col = "project",
                   dash_form = TRUE, source_id = "unified_sessions"),
  extract_cp_dates(file.path(extdata, "cost_by_project_estimated.json"),
                   date_col = "date", proj_col = "project",
                   dash_form = TRUE, source_id = "cost_by_project_estimated"),
  extract_cp_dates(file.path(extdata, "git_commits_by_project.json"),
                   date_col = "date", source_id = "git_commits_by_project"),
  extract_cp_dates(file.path(extdata, "weekly_commits_by_project.json"),
                   date_col = "week_start_date", source_id = "weekly_commits_by_project"),
  extract_cp_dates(file.path(extdata, "cost_per_commit.json"),
                   date_col = "date", source_id = "cost_per_commit"),
  extract_cp_dates(file.path(extdata, "file_churn.json"),
                   date_col = "last_changed_date", source_id = "file_churn")
)
# change_coupling.json has no date column; extract_cp_dates cannot handle it.
# Read it unconditionally (gated on file existence) so projects present ONLY
# in change_coupling.json are included in projects_master with n_sources >= 1.
# (fix for Issue #12 — dead branch via date_col = NA_character_)
cp_json_path <- file.path(extdata, "change_coupling.json")
pm_cp_only <- if (file.exists(cp_json_path)) {
  cp_df <- tryCatch(
    fromJSON(cp_json_path, simplifyDataFrame = TRUE),
    error = function(e) NULL
  )
  if (!is.null(cp_df) && is.data.frame(cp_df) && nrow(cp_df) > 0L) {
    # Prefer stored canonical_project column; fall back to deriving from project
    cp_vec <- if ("canonical_project" %in% names(cp_df)) {
      cp_df$canonical_project
    } else {
      canonicalize_project(cp_df$project)
    }
    data.frame(canonical_project = cp_vec, date = NA_character_,
               source = "change_coupling",
               stringsAsFactors = FALSE)
  } else {
    NULL
  }
} else {
  NULL
}
pm_sources <- c(pm_sources, list(pm_cp_only))

pm_all <- do.call(rbind, Filter(Negate(is.null), pm_sources))

if (!is.null(pm_all) && nrow(pm_all) > 0L) {
  pm_all <- pm_all[!is.na(pm_all$canonical_project) &
                     nzchar(pm_all$canonical_project), ]

  # Count distinct data sources per canonical_project (#903)
  source_counts <- tapply(
    pm_all$source,
    pm_all$canonical_project,
    function(srcs) length(unique(srcs))
  )

  # Date range (ignoring NA dates)
  pm_dates <- pm_all[!is.na(pm_all$date), ]
  date_min <- tapply(pm_dates$date, pm_dates$canonical_project, min)
  date_max <- tapply(pm_dates$date, pm_dates$canonical_project, max)

  all_cp <- sort(unique(pm_all$canonical_project), method = "radix")
  projects_master <- data.frame(
    canonical_project = all_cp,
    n_sources         = as.integer(source_counts[all_cp]),
    first_seen        = as.character(date_min[all_cp]),
    last_seen         = as.character(date_max[all_cp]),
    stringsAsFactors  = FALSE
  )
  # Truncate dates to YYYY-MM-DD (they may include time component)
  projects_master$first_seen <- substr(projects_master$first_seen, 1L, 10L)
  projects_master$last_seen  <- substr(projects_master$last_seen,  1L, 10L)

  # Exclude confidential/demo projects from public master list (#83 Phase B)
  projects_master <- clean_projects(projects_master, project_col = "canonical_project")

  write_json(projects_master, file.path(out_dir, "projects_master.json"),
             auto_unbox = TRUE)
  write_json(projects_master, file.path(extdata, "projects_master.json"),
             auto_unbox = TRUE)
  cat(sprintf("  -> %d canonical projects in projects_master.json\n",
              nrow(projects_master)))
} else {
  write_json(list(), file.path(out_dir, "projects_master.json"))
  write_json(list(), file.path(extdata, "projects_master.json"))
  cat("  -> no project data available, writing empty projects_master.json\n")
}

# --- 8c. Export roborev_summary.json (Reviews tab in dashboard) ---------------
# Codex pattern: when unified.duckdb is present (local runs), regenerate the
# committed source at inst/extdata/roborev_summary.json.  Always copy the
# committed source to vignettes/data/ so CI serves real data even without the DB.
#
# Contract (from dashboard_shinylive.qmd, rv_* reactives):
#   pulse: {n_open, n_critical, n_high, n_resolved, n_loops, n_escalate, wasted_usd}
#   loops: [{tier, cycles, severity, primary_file, summary,
#             first_seen, last_seen, estimated_wasted_usd}]
cat("Exporting roborev_summary.json...\n")
tryCatch({
  # Committed source (checked in; regenerated locally, copied by CI)
  roborev_src      <- file.path(extdata, "roborev_summary.json")
  # Output location served by the dashboard
  roborev_json_path <- file.path(out_dir, "roborev_summary.json")
  unified_db_path   <- file.path(Sys.getenv("HOME"), ".claude/logs/unified.duckdb")

  empty_roborev <- list(
    pulse = list(n_open = 0L, n_critical = 0L, n_high = 0L,
                 n_resolved = 0L, n_loops = 0L, n_escalate = 0L,
                 wasted_usd = 0),
    loops = list()
  )

  if (file.exists(unified_db_path)) {
    # Local run: read DB and (re)generate the committed source
    rcon <- dbConnect(duckdb(), dbdir = unified_db_path, read_only = TRUE)
    on.exit(tryCatch(dbDisconnect(rcon, shutdown = TRUE), error = function(e) NULL),
            add = TRUE)

    rbtbls <- dbListTables(rcon)

    # --- pulse from roborev_review_lifecycle ---
    if ("roborev_review_lifecycle" %in% rbtbls) {
      rl <- dbReadTable(rcon, "roborev_review_lifecycle")

      # "open" = no close_reason (closed_at is not populated in production)
      is_open <- is.na(rl$close_reason) | !nzchar(as.character(rl$close_reason))

      n_open     <- sum(is_open)
      n_resolved <- sum(!is_open)

      # severity_max values in production: "High", "Medium", "Low" (no "Critical")
      # Map: High -> critical KPI, Medium -> high KPI for dashboard display
      n_high     <- sum(!is.na(rl$severity_max) & rl$severity_max == "High"   & is_open)
      n_critical <- sum(!is.na(rl$severity_max) & rl$severity_max == "Medium" & is_open)
    } else {
      n_open <- n_resolved <- n_high <- n_critical <- 0L
      cat("  -> roborev_review_lifecycle not found; pulse will be zero\n")
    }

    # --- loops from roborev_loops (if present) ---
    if ("roborev_loops" %in% rbtbls) {
      loops_df    <- dbReadTable(rcon, "roborev_loops")
      n_loops     <- nrow(loops_df)
      n_escalate  <- sum(!is.na(loops_df$tier) & loops_df$tier == "escalate")
      wasted_usd  <- round(sum(loops_df$estimated_wasted_usd, na.rm = TRUE), 4)

      # Shape loops array for dashboard: keep only the columns the dashboard uses
      loop_cols <- c("tier", "cycles", "severity", "primary_file", "summary",
                     "first_seen", "last_seen", "estimated_wasted_usd")
      loops_export <- loops_df[, intersect(loop_cols, names(loops_df)), drop = FALSE]
      # Coerce timestamps to character for JSON serialisation
      for (col in c("first_seen", "last_seen")) {
        if (col %in% names(loops_export)) {
          loops_export[[col]] <- as.character(loops_export[[col]])
        }
      }
      loops_list <- lapply(seq_len(nrow(loops_export)), function(i) {
        as.list(loops_export[i, , drop = FALSE])
      })
    } else {
      n_loops    <- 0L
      n_escalate <- 0L
      wasted_usd <- 0
      loops_list <- list()
      cat("  -> roborev_loops not found; loops array will be empty\n")
    }

    roborev_summary <- list(
      pulse = list(
        n_open     = as.integer(n_open),
        n_critical = as.integer(n_critical),
        n_high     = as.integer(n_high),
        n_resolved = as.integer(n_resolved),
        n_loops    = as.integer(n_loops),
        n_escalate = as.integer(n_escalate),
        wasted_usd = wasted_usd
      ),
      loops = loops_list
    )

    # Write committed source (local regeneration)
    write_json(roborev_summary, roborev_src, auto_unbox = TRUE)
    cat(sprintf("  -> inst/extdata/roborev_summary.json written: pulse={n_open=%d, n_high=%d, n_resolved=%d, n_loops=%d}\n",
                n_open, n_high, n_resolved, n_loops))
  } else {
    cat("  -> unified.duckdb not found; skipping committed source regeneration\n")
  }

  # Always: copy committed source to vignettes/data/ (works in CI without the DB)
  if (file.exists(roborev_src)) {
    file.copy(roborev_src, roborev_json_path, overwrite = TRUE)
    cat(sprintf("  -> copied inst/extdata/roborev_summary.json -> vignettes/data/ (%d bytes)\n",
                file.info(roborev_src)$size))
  } else {
    # Neither DB nor committed source: write graceful empty (does not overwrite a present source)
    cat("  -> no committed source found; writing empty roborev_summary.json placeholder\n")
    write_json(empty_roborev, roborev_json_path, auto_unbox = TRUE)
  }
}, error = function(e) {
  cat(sprintf("  -> roborev_summary.json export error: %s\n", conditionMessage(e)))
  cat("  -> writing empty roborev_summary.json as fallback\n")
  write_json(
    list(
      pulse = list(n_open = 0L, n_critical = 0L, n_high = 0L,
                   n_resolved = 0L, n_loops = 0L, n_escalate = 0L,
                   wasted_usd = 0),
      loops = list()
    ),
    file.path(out_dir, "roborev_summary.json"),
    auto_unbox = TRUE
  )
})

# --- 8d. Export roborev_by_repo.json (per-repo review counts, #146) -----------
# Contract: [{repo, n_total, n_high, n_resolved, n_open}, ...] sorted by n_total desc.
# Codex pattern: regenerate from unified.duckdb when available, else copy committed
# inst/extdata/roborev_by_repo.json.  Always copy committed source to vignettes/data/.
cat("Exporting roborev_by_repo.json...\n")
tryCatch({
  roborev_by_repo_src  <- file.path(extdata, "roborev_by_repo.json")
  roborev_by_repo_dst  <- file.path(out_dir, "roborev_by_repo.json")

  if (file.exists(unified_db)) {
    rcon2 <- dbConnect(duckdb(), dbdir = unified_db, read_only = TRUE)
    on.exit(tryCatch(dbDisconnect(rcon2, shutdown = TRUE), error = function(e) NULL),
            add = TRUE)
    rbtbls2 <- dbListTables(rcon2)

    if ("roborev_review_lifecycle" %in% rbtbls2) {
      rl2 <- dbReadTable(rcon2, "roborev_review_lifecycle")

      # repo column: fall back to "project" if "repo" is absent
      repo_col <- if ("repo" %in% names(rl2)) "repo" else "project"

      if (repo_col %in% names(rl2)) {
        is_open2 <- is.na(rl2$close_reason) | !nzchar(as.character(rl2$close_reason))
        is_high  <- !is.na(rl2$severity_max) & rl2$severity_max %in% c("High", "Critical")

        rl2$repo_name   <- rl2[[repo_col]]
        rl2$is_open     <- as.integer(is_open2)
        rl2$is_high     <- as.integer(is_high)
        rl2$is_resolved <- as.integer(!is_open2)

        by_repo_agg <- aggregate(
          cbind(n_high     = is_high,
                n_resolved = is_resolved,
                n_open     = is_open) ~ repo_name,
          data = rl2,
          FUN  = sum
        )
        by_repo_agg$n_total <- by_repo_agg$n_resolved + by_repo_agg$n_open

        by_repo <- by_repo_agg
        names(by_repo)[names(by_repo) == "repo_name"] <- "repo"
        by_repo <- by_repo[order(-by_repo$n_total), ]
        # Keep columns in contract order: repo, n_total, n_high, n_resolved, n_open
        by_repo <- by_repo[, c("repo", "n_total", "n_high", "n_resolved", "n_open")]

        row.names(by_repo) <- NULL

        # Apply clean_projects to remove confidential repos and apply remap (#83 Phase B)
        by_repo <- clean_projects(by_repo, project_col = "repo")
        # Re-aggregate in case remap merged two rows (e.g. llmtelemetry-hook-sync -> llmtelemetry)
        by_repo <- aggregate(
          cbind(n_total, n_high, n_resolved, n_open) ~ repo,
          data = by_repo,
          FUN  = sum
        )
        by_repo <- by_repo[order(-by_repo$n_total), ]
        row.names(by_repo) <- NULL

        write_json(by_repo, roborev_by_repo_src, auto_unbox = TRUE)
        cat(sprintf("  -> inst/extdata/roborev_by_repo.json: %d repos\n", nrow(by_repo)))
      } else {
        cat("  -> roborev_review_lifecycle has no repo/project column; skipping\n")
      }
    } else {
      cat("  -> roborev_review_lifecycle not found; skipping roborev_by_repo\n")
    }
    tryCatch(dbDisconnect(rcon2, shutdown = TRUE), error = function(e) NULL)
  } else {
    cat("  -> unified.duckdb not found; will copy committed source\n")
  }

  # Always copy committed source to vignettes/data/
  if (file.exists(roborev_by_repo_src)) {
    file.copy(roborev_by_repo_src, roborev_by_repo_dst, overwrite = TRUE)
    cat(sprintf("  -> copied roborev_by_repo.json -> vignettes/data/ (%d bytes)\n",
                file.info(roborev_by_repo_src)$size))
  } else {
    write_json(list(), roborev_by_repo_dst, auto_unbox = TRUE)
    cat("  -> wrote empty roborev_by_repo.json (no source available)\n")
  }
}, error = function(e) {
  cat(sprintf("  -> roborev_by_repo.json export error: %s\n", conditionMessage(e)))
  write_json(list(), file.path(out_dir, "roborev_by_repo.json"), auto_unbox = TRUE)
})

# --- 8e. Export roborev_resolution.json (#146 Q5) ----------------------------
# Resolution rate = close_reason='manual' / total.
# Weekly trend: per week (created_at), resolved vs open counts.
# Privacy: clean_projects() on repo column before aggregating.
# Guard: if table/columns missing, write empty-but-valid JSON.
cat("Exporting roborev_resolution.json...\n")
tryCatch({
  roborev_res_src <- file.path(extdata, "roborev_resolution.json")
  roborev_res_dst <- file.path(out_dir, "roborev_resolution.json")

  empty_res <- list(
    overall = list(total = 0L, resolved = 0L, open = 0L, resolution_rate = 0),
    weekly  = list()
  )

  if (file.exists(unified_db)) {
    rcon3 <- dbConnect(duckdb(), dbdir = unified_db, read_only = TRUE)
    on.exit(tryCatch(dbDisconnect(rcon3, shutdown = TRUE), error = function(e) NULL),
            add = TRUE)
    rbtbls3 <- dbListTables(rcon3)

    if ("roborev_review_lifecycle" %in% rbtbls3) {
      rl3 <- dbReadTable(rcon3, "roborev_review_lifecycle") |> as_tibble()

      # Determine repo column
      repo_col3 <- if ("repo" %in% names(rl3)) "repo" else if ("project" %in% names(rl3)) "project" else NULL

      if (!is.null(repo_col3)) {
        # Rename to 'repo' for clean_projects (expects 'project' or use project_col=)
        names(rl3)[names(rl3) == repo_col3] <- "repo"

        # Apply privacy filter: clean_projects on repo column BEFORE aggregating
        rl3_clean <- clean_projects(rl3, project_col = "repo")

        # Assert 0 confidential repos in the output
        conf_lower  <- tolower(CONFIDENTIAL_PROJECTS)
        repos_clean <- tolower(rl3_clean$repo)
        base_repos  <- sub(
          "-(?:feat|fix|chore|docs|refactor|test|ci|perf|style|build|revert|worktree)-.*$",
          "", repos_clean, ignore.case = TRUE
        )
        n_conf <- sum(base_repos %in% conf_lower)
        if (n_conf > 0L) {
          stop(sprintf(
            "roborev_resolution: %d confidential repo row(s) remain after clean_projects — aborting",
            n_conf
          ))
        }
        cat(sprintf("  -> privacy assertion passed: 0 confidential repos in output\n"))

        is_resolved3 <- !is.na(rl3_clean$close_reason) & nzchar(as.character(rl3_clean$close_reason))
        n_total    <- nrow(rl3_clean)
        n_resolved <- sum(is_resolved3)
        n_open     <- n_total - n_resolved
        res_rate   <- if (n_total > 0L) round(n_resolved / n_total, 4) else 0

        # Weekly trend: group by week of created_at
        if ("created_at" %in% names(rl3_clean) && n_total > 0L) {
          rl3_clean$created_at_dt <- as.Date(as.POSIXct(rl3_clean$created_at, tz = "UTC"))
          # ISO week start (Monday)
          rl3_clean$week_start <- rl3_clean$created_at_dt -
            as.integer(format(rl3_clean$created_at_dt, "%u")) + 1L
          rl3_clean$week_start <- as.character(rl3_clean$week_start)

          weekly_agg <- rl3_clean |>
            group_by(week_start) |>
            summarise(
              resolved = sum(is_resolved3[row_number()]),
              open     = n() - sum(is_resolved3[row_number()]),
              total    = n(),
              .groups  = "drop"
            ) |>
            mutate(
              resolution_rate = if_else(total > 0L, round(resolved / total, 4), 0)
            ) |>
            arrange(week_start)
        } else {
          weekly_agg <- tibble(
            week_start = character(0), resolved = integer(0),
            open = integer(0), total = integer(0), resolution_rate = numeric(0)
          )
        }

        roborev_res <- list(
          overall = list(
            total           = as.integer(n_total),
            resolved        = as.integer(n_resolved),
            open            = as.integer(n_open),
            resolution_rate = res_rate
          ),
          weekly = lapply(seq_len(nrow(weekly_agg)), function(i) {
            as.list(weekly_agg[i, , drop = FALSE])
          })
        )

        write_json(roborev_res, roborev_res_src, auto_unbox = TRUE)
        cat(sprintf(
          "  -> roborev_resolution.json: total=%d, resolved=%d, rate=%.1f%%, weeks=%d\n",
          n_total, n_resolved, res_rate * 100, nrow(weekly_agg)
        ))
      } else {
        cat("  -> roborev_review_lifecycle has no repo/project column; writing empty\n")
        write_json(empty_res, roborev_res_src, auto_unbox = TRUE)
      }
      tryCatch(dbDisconnect(rcon3, shutdown = TRUE), error = function(e) NULL)
    } else {
      cat("  -> roborev_review_lifecycle not found; writing empty roborev_resolution.json\n")
      write_json(empty_res, roborev_res_src, auto_unbox = TRUE)
    }
  } else {
    cat("  -> unified.duckdb not found; will copy committed source\n")
  }

  # Always copy committed source to vignettes/data/
  if (file.exists(roborev_res_src)) {
    file.copy(roborev_res_src, roborev_res_dst, overwrite = TRUE)
    cat(sprintf("  -> copied roborev_resolution.json -> vignettes/data/ (%d bytes)\n",
                file.info(roborev_res_src)$size))
  } else {
    write_json(empty_res, roborev_res_dst, auto_unbox = TRUE)
    cat("  -> no committed source; wrote empty placeholder to vignettes/data/\n")
  }
}, error = function(e) {
  cat(sprintf("  -> roborev_resolution.json export error: %s\n", conditionMessage(e)))
  write_json(
    list(
      overall = list(total = 0L, resolved = 0L, open = 0L, resolution_rate = 0),
      weekly  = list()
    ),
    file.path(out_dir, "roborev_resolution.json"),
    auto_unbox = TRUE
  )
})

# --- 8f. Export roborev_agent_perf.json (#146 Q10) ----------------------------
# Per-agent pass rate: pass_count / n_runs, total reviews, cost_per_review if available.
# Source table: roborev_agent_performance (agent, model, n_runs, pass_count, fail_count,
#   error_count, total_cost_usd).
# Privacy: no repo/project column in this table — no clean_projects() needed.
#   Agent names (codex, claude-code, gemini) are not confidential.
# Guard: if table/columns missing, write empty-but-valid JSON and warn.
cat("Exporting roborev_agent_perf.json...\n")
tryCatch({
  ap_src <- file.path(extdata, "roborev_agent_perf.json")
  ap_dst <- file.path(out_dir, "roborev_agent_perf.json")

  empty_ap <- list(agents = list(), generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"))

  if (file.exists(unified_db)) {
    apcon <- dbConnect(duckdb(), dbdir = unified_db, read_only = TRUE)
    on.exit(tryCatch(dbDisconnect(apcon, shutdown = TRUE), error = function(e) NULL),
            add = TRUE)
    ap_tbls <- dbListTables(apcon)

    if ("roborev_agent_performance" %in% ap_tbls) {
      ap_raw <- dbReadTable(apcon, "roborev_agent_performance") |> as_tibble()

      # Validate required columns
      req_cols <- c("agent", "n_runs", "pass_count", "fail_count")
      missing_cols <- setdiff(req_cols, names(ap_raw))
      if (length(missing_cols) > 0L) {
        cat(sprintf("  -> roborev_agent_performance missing columns: %s; writing empty\n",
                    paste(missing_cols, collapse = ", ")))
        write_json(empty_ap, ap_src, auto_unbox = TRUE)
      } else {
        # Aggregate by agent (sum across dates)
        has_cost <- "total_cost_usd" %in% names(ap_raw)

        # Aggregate by agent across all dates.
        # total_cost_usd is all-NA in the current data (cost not logged by roborev pipeline).
        # Use any_nonzero_cost flag so sum(na.rm=TRUE)=0 is not mistaken for real zero cost.
        any_nonzero_cost <- has_cost &&
          any(!is.na(ap_raw$total_cost_usd) & ap_raw$total_cost_usd > 0, na.rm = TRUE)

        ap_agg <- ap_raw |>
          group_by(agent) |>
          summarise(
            n_runs      = sum(n_runs, na.rm = TRUE),
            pass_count  = sum(pass_count, na.rm = TRUE),
            fail_count  = sum(fail_count, na.rm = TRUE),
            error_count = sum(error_count, na.rm = TRUE),
            total_cost  = if (any_nonzero_cost) sum(total_cost_usd, na.rm = TRUE) else NA_real_,
            .groups     = "drop"
          ) |>
          mutate(
            pass_rate       = if_else(n_runs > 0L, round(pass_count / n_runs, 4), 0),
            cost_per_review = if_else(
              !is.na(total_cost) & n_runs > 0L,
              round(total_cost / n_runs, 6),
              NA_real_
            )
          ) |>
          arrange(desc(pass_rate))

        # Build output list
        agent_list <- lapply(seq_len(nrow(ap_agg)), function(i) {
          row <- ap_agg[i, , drop = FALSE]
          out <- list(
            agent       = row$agent,
            n_runs      = as.integer(row$n_runs),
            pass_count  = as.integer(row$pass_count),
            fail_count  = as.integer(row$fail_count),
            error_count = as.integer(row$error_count),
            pass_rate   = row$pass_rate
          )
          if (!is.na(row$cost_per_review)) {
            out$cost_per_review_usd <- row$cost_per_review
            out$total_cost_usd      <- round(row$total_cost, 4)
          }
          out
        })

        ap_out <- list(
          agents       = agent_list,
          generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
        )
        write_json(ap_out, ap_src, auto_unbox = TRUE)
        cat(sprintf(
          "  -> roborev_agent_perf.json: %d agents, runs=%d, top pass-rate=%.1f%% (%s)\n",
          nrow(ap_agg),
          sum(ap_agg$n_runs),
          max(ap_agg$pass_rate, na.rm = TRUE) * 100,
          ap_agg$agent[which.max(ap_agg$pass_rate)]
        ))
        cat("  -> privacy: no repo column in roborev_agent_performance — no filtering needed\n")
      }
      tryCatch(dbDisconnect(apcon, shutdown = TRUE), error = function(e) NULL)
    } else {
      cat("  -> roborev_agent_performance not found; writing empty roborev_agent_perf.json\n")
      write_json(empty_ap, ap_src, auto_unbox = TRUE)
    }
  } else {
    cat("  -> unified.duckdb not found; will copy committed source\n")
  }

  # Always copy committed source to vignettes/data/
  if (file.exists(ap_src)) {
    file.copy(ap_src, ap_dst, overwrite = TRUE)
    cat(sprintf("  -> copied roborev_agent_perf.json -> vignettes/data/ (%d bytes)\n",
                file.info(ap_src)$size))
  } else {
    write_json(empty_ap, ap_dst, auto_unbox = TRUE)
    cat("  -> no committed source; wrote empty placeholder to vignettes/data/\n")
  }
}, error = function(e) {
  cat(sprintf("  -> roborev_agent_perf.json export error: %s\n", conditionMessage(e)))
  write_json(
    list(agents = list(), generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")),
    file.path(out_dir, "roborev_agent_perf.json"),
    auto_unbox = TRUE
  )
})

# --- 8h. Export roborev_backlog_age.json (#146 Q15) ---------------------------
# Open-review age distribution: for every open review in ~/.roborev/reviews.db,
# compute hours_open = (now - enqueued_at) * 24 and bucket into:
#   0-1d / 1-3d / 3-7d / 7-14d / >14d
# Reads directly from reviews.db via sqlite3 CLI (no RSQLite dependency).
# Codex pattern: regenerate locally; always copy committed source to vignettes/data/.
# Privacy: confidential repos (mycare, crypto, solwatch, swarms) excluded.
cat("Exporting roborev_backlog_age.json...\n")
tryCatch({
  ba_src <- file.path(extdata, "roborev_backlog_age.json")
  ba_dst <- file.path(out_dir, "roborev_backlog_age.json")

  empty_ba <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    n_open       = 0L,
    median_hours = NULL,
    p90_hours    = NULL,
    max_hours    = 0,
    buckets      = list(
      list(label = "0-1d",  hours_min = 0L,   hours_max = 24L,  n = 0L),
      list(label = "1-3d",  hours_min = 24L,  hours_max = 72L,  n = 0L),
      list(label = "3-7d",  hours_min = 72L,  hours_max = 168L, n = 0L),
      list(label = "7-14d", hours_min = 168L, hours_max = 336L, n = 0L),
      list(label = ">14d",  hours_min = 336L, hours_max = NULL, n = 0L)
    ),
    top_oldest   = list()
  )

  reviews_db <- path.expand("~/.roborev/reviews.db")

  # Detect sqlite3 binary (try nix store paths then system PATH)
  sqlite3_candidates <- c(
    "/nix/store/0hzmsbn5h50x7iyz1h5svbr68hppyicy-sqlite-3.51.2-bin/bin/sqlite3",
    Sys.which("sqlite3")
  )
  sqlite3_bin <- sqlite3_candidates[nzchar(sqlite3_candidates) & file.exists(sqlite3_candidates)][1L]

  if (file.exists(reviews_db) && !is.na(sqlite3_bin) && nzchar(sqlite3_bin)) {
    sql_file_ba <- tempfile(fileext = ".sql")
    writeLines(c(
      "SELECT ROUND((julianday('now') - julianday(rj.enqueued_at)) * 24, 4) as hours_open,",
      "       repos.name as repo",
      "FROM reviews r",
      "LEFT JOIN review_jobs rj ON r.job_id = rj.id",
      "LEFT JOIN repos ON rj.repo_id = repos.id",
      "WHERE r.closed = 0",
      "  AND rj.enqueued_at IS NOT NULL",
      "ORDER BY hours_open;"
    ), sql_file_ba)

    rows_raw_ba <- system(
      paste0(sqlite3_bin, " -csv ", shQuote(reviews_db), " < ", shQuote(sql_file_ba)),
      intern = TRUE
    )
    unlink(sql_file_ba)

    if (length(rows_raw_ba) > 0L) {
      rows_df_ba <- tryCatch(
        read.csv(text = paste(rows_raw_ba, collapse = "\n"), header = FALSE,
                 col.names = c("hours_open", "repo"), stringsAsFactors = FALSE),
        error = function(e) NULL
      )

      if (!is.null(rows_df_ba) && nrow(rows_df_ba) > 0L) {
        rows_df_ba$hours_open <- as.numeric(rows_df_ba$hours_open)
        rows_df_ba <- rows_df_ba[!is.na(rows_df_ba$hours_open), ]

        # Privacy: exclude confidential repos
        rows_df_ba <- rows_df_ba[!tolower(rows_df_ba$repo) %in% tolower(EXCLUDED_DASHBOARD_PROJECTS), ]

        n_ba   <- nrow(rows_df_ba)
        hrs_ba <- rows_df_ba$hours_open

        breaks_ba <- c(0, 24, 72, 168, 336, Inf)
        labels_ba <- c("0-1d", "1-3d", "3-7d", "7-14d", ">14d")
        mins_ba   <- c(0L, 24L, 72L, 168L, 336L)
        maxs_ba   <- c(24L, 72L, 168L, 336L, NULL)

        bucket_n <- as.integer(table(
          cut(hrs_ba, breaks = breaks_ba, include.lowest = TRUE, right = FALSE, labels = labels_ba)
        ))

        buckets_ba <- lapply(seq_along(labels_ba), function(i) {
          list(
            label     = labels_ba[i],
            hours_min = mins_ba[i],
            hours_max = if (i < length(labels_ba)) as.integer(maxs_ba[[i]]) else NULL,
            n         = bucket_n[i]
          )
        })

        # Top 10 oldest open (confidential already excluded)
        top_ba <- rows_df_ba[order(-rows_df_ba$hours_open), ][seq_len(min(10L, n_ba)), ]
        top_list_ba <- lapply(seq_len(nrow(top_ba)), function(i) {
          list(repo = top_ba$repo[i], hours_open = round(top_ba$hours_open[i], 2))
        })

        ba_out <- list(
          generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
          n_open       = as.integer(n_ba),
          median_hours = if (n_ba > 0L) round(median(hrs_ba, na.rm = TRUE), 2) else NULL,
          p90_hours    = if (n_ba > 0L) round(quantile(hrs_ba, 0.90, na.rm = TRUE)[[1]], 2) else NULL,
          max_hours    = if (n_ba > 0L) round(max(hrs_ba, na.rm = TRUE), 2) else 0,
          buckets      = buckets_ba,
          top_oldest   = top_list_ba
        )

        write_json(ba_out, ba_src, auto_unbox = TRUE, null = "null")
        cat(sprintf(
          "  -> roborev_backlog_age.json: n_open=%d, median=%.1fh, p90=%.1fh, max=%.1fh\n",
          n_ba,
          if (is.null(ba_out$median_hours)) 0 else ba_out$median_hours,
          if (is.null(ba_out$p90_hours))    0 else ba_out$p90_hours,
          ba_out$max_hours
        ))
      } else {
        cat("  -> reviews.db query returned no parseable rows; writing empty\n")
        write_json(empty_ba, ba_src, auto_unbox = TRUE, null = "null")
      }
    } else {
      cat("  -> reviews.db: no open reviews found; writing empty\n")
      write_json(empty_ba, ba_src, auto_unbox = TRUE, null = "null")
    }
  } else {
    cat("  -> reviews.db or sqlite3 not found; will copy committed source\n")
  }

  # Always copy committed source to vignettes/data/
  if (file.exists(ba_src)) {
    file.copy(ba_src, ba_dst, overwrite = TRUE)
    cat(sprintf("  -> copied roborev_backlog_age.json -> vignettes/data/ (%d bytes)\n",
                file.info(ba_src)$size))
  } else {
    write_json(empty_ba, ba_dst, auto_unbox = TRUE, null = "null")
    cat("  -> no committed source; wrote empty placeholder to vignettes/data/\n")
  }
}, error = function(e) {
  cat(sprintf("  -> roborev_backlog_age.json export error: %s\n", conditionMessage(e)))
  write_json(
    list(
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      n_open = 0L, median_hours = NULL, p90_hours = NULL, max_hours = 0,
      buckets = list(), top_oldest = list()
    ),
    file.path(out_dir, "roborev_backlog_age.json"),
    auto_unbox = TRUE, null = "null"
  )
})

# --- 8g. Export data/roborev/latest.json (llm#287 Part B — resolution page) --
# Aggregated roborev resolution metrics across 1d/3d/7d/14d windows plus a
# 14-day daily time-series and 7d-vs-prior-7d trend deltas.
# Schema is documented in tests/testthat/fixtures/roborev_daily_sample.json.
# Codex pattern: regenerate from unified.duckdb when available; always copy
# committed source (inst/extdata/roborev_latest.json) to vignettes/data/roborev/.
cat("Exporting roborev/latest.json...\n")
tryCatch({
  roborev_latest_src <- file.path(extdata, "roborev_latest.json")
  roborev_latest_dir <- file.path(out_dir, "roborev")
  if (!dir.exists(roborev_latest_dir)) dir.create(roborev_latest_dir, recursive = TRUE)
  roborev_latest_dst <- file.path(roborev_latest_dir, "latest.json")

  # Helper: compute window aggregate from roborev_review_lifecycle
  compute_window <- function(rl_clean, days, label) {
    cutoff <- Sys.time() - days * 86400
    in_win <- as.POSIXct(rl_clean$created_at, tz = "UTC") >= cutoff
    w <- rl_clean[in_win, ]
    if (nrow(w) == 0L) {
      return(list(
        label = label,
        period_start = format(cutoff, "%Y-%m-%dT%H:%M:%SZ"),
        period_end   = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
        total_reviews = 0L, total_closed = 0L, resolution_rate = 0,
        verdict_counts = list(closed = 0L, open = 0L),
        severity_counts = list(critical = 0L, high = 0L, medium = 0L, low = 0L),
        median_hours_to_close = NULL, p90_hours_to_close = NULL,
        p99_hours_to_close = NULL,
        by_verdict = list(), outliers = list()
      ))
    }
    is_closed <- !is.na(w$close_reason) & nzchar(as.character(w$close_reason))
    total      <- nrow(w)
    n_closed   <- sum(is_closed)
    n_open     <- total - n_closed
    res_rate   <- if (total > 0L) round(n_closed / total, 4) else 0

    # Severity counts
    sev_col <- if ("severity_max" %in% names(w)) w$severity_max else rep(NA_character_, nrow(w))
    sev_counts <- list(
      critical = sum(!is.na(sev_col) & tolower(sev_col) == "critical"),
      high     = sum(!is.na(sev_col) & tolower(sev_col) == "high"),
      medium   = sum(!is.na(sev_col) & tolower(sev_col) == "medium"),
      low      = sum(!is.na(sev_col) & tolower(sev_col) == "low")
    )
    sev_counts <- lapply(sev_counts, as.integer)

    # by_verdict: simplified — fixed = has close_reason, wontfix if close_reason='wontfix',
    # open = no close_reason. This is best-effort; roborev doesn't log verdict subtypes yet.
    n_wontfix <- if ("close_reason" %in% names(w))
      sum(!is.na(w$close_reason) & tolower(as.character(w$close_reason)) == "wontfix") else 0L
    n_fixed   <- n_closed - as.integer(n_wontfix)
    by_verdict <- list(
      list(verdict = "fixed",   n = as.integer(n_fixed),
           pct = if (total > 0L) round(n_fixed / total, 4) else 0),
      list(verdict = "wontfix", n = as.integer(n_wontfix),
           pct = if (total > 0L) round(n_wontfix / total, 4) else 0),
      list(verdict = "open",    n = as.integer(n_open),
           pct = if (total > 0L) round(n_open / total, 4) else 0)
    )

    # Time-to-close percentiles (only for closed reviews with both created_at and closed_at)
    p50 <- p90 <- p99 <- NULL
    closed_rows <- w[is_closed, ]
    if (nrow(closed_rows) > 0L && "closed_at" %in% names(closed_rows) &&
        "created_at" %in% names(closed_rows)) {
      created_ts <- as.POSIXct(closed_rows$created_at, tz = "UTC")
      closed_ts  <- as.POSIXct(closed_rows$closed_at,  tz = "UTC")
      hours_open <- as.numeric(difftime(closed_ts, created_ts, units = "hours"))
      hours_open <- hours_open[!is.na(hours_open) & hours_open >= 0]
      if (length(hours_open) > 0L) {
        p50 <- round(quantile(hours_open, 0.50, na.rm = TRUE)[[1]], 2)
        p90 <- round(quantile(hours_open, 0.90, na.rm = TRUE)[[1]], 2)
        p99 <- round(quantile(hours_open, 0.99, na.rm = TRUE)[[1]], 2)
      }
    }

    # Outliers: open reviews sorted by time open DESC (top 10)
    open_rows <- w[!is_closed, ]
    outliers <- list()
    if (nrow(open_rows) > 0L && "created_at" %in% names(open_rows)) {
      open_rows$hours_open_val <- as.numeric(
        difftime(Sys.time(),
                 as.POSIXct(open_rows$created_at, tz = "UTC"),
                 units = "hours")
      )
      open_rows <- open_rows[order(-open_rows$hours_open_val), ]
      n_out <- min(10L, nrow(open_rows))
      file_col <- if ("primary_file" %in% names(open_rows)) "primary_file" else
                  if ("file" %in% names(open_rows)) "file" else NULL
      repo_col_out <- if ("repo" %in% names(open_rows)) "repo" else
                      if ("project" %in% names(open_rows)) "project" else NULL
      for (i in seq_len(n_out)) {
        row <- open_rows[i, ]
        entry <- list(
          review_id  = if ("id" %in% names(row)) as.character(row$id[[1]]) else paste0("row-", i),
          repo       = if (!is.null(repo_col_out)) as.character(row[[repo_col_out]][[1]]) else "",
          file       = if (!is.null(file_col)) as.character(row[[file_col]][[1]]) else "",
          severity   = if (!is.null(sev_col)) tolower(as.character(row$severity_max[[1]])) else "",
          hours_open = round(row$hours_open_val[[1]], 2),
          verdict    = "open",
          summary    = if ("summary" %in% names(row)) as.character(row$summary[[1]]) else ""
        )
        outliers <- c(outliers, list(entry))
      }
    }

    list(
      label          = label,
      period_start   = format(cutoff, "%Y-%m-%dT%H:%M:%SZ"),
      period_end     = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      total_reviews  = as.integer(total),
      total_closed   = as.integer(n_closed),
      resolution_rate = res_rate,
      verdict_counts  = list(closed = as.integer(n_closed), open = as.integer(n_open)),
      severity_counts = sev_counts,
      median_hours_to_close = p50,
      p90_hours_to_close    = p90,
      p99_hours_to_close    = p99,
      by_verdict = by_verdict,
      outliers   = outliers
    )
  }

  empty_latest <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    schema_version = "1.0",
    windows = list(`1d` = list(), `3d` = list(), `7d` = list(), `14d` = list()),
    trends = list(),
    time_series = list()
  )

  if (file.exists(unified_db)) {
    lcon <- dbConnect(duckdb(), dbdir = unified_db, read_only = TRUE)
    on.exit(tryCatch(dbDisconnect(lcon, shutdown = TRUE), error = function(e) NULL),
            add = TRUE)
    ltbls <- dbListTables(lcon)

    if ("roborev_review_lifecycle" %in% ltbls) {
      rl_latest <- dbReadTable(lcon, "roborev_review_lifecycle")

      # Privacy: apply clean_projects on repo/project column
      repo_col_l <- if ("repo" %in% names(rl_latest)) "repo" else
                    if ("project" %in% names(rl_latest)) "project" else NULL
      if (!is.null(repo_col_l)) {
        names(rl_latest)[names(rl_latest) == repo_col_l] <- "repo"
        rl_latest <- clean_projects(rl_latest, project_col = "repo")
      }

      # Per-window aggregates
      windows_out <- list(
        `1d`  = compute_window(rl_latest, 1,  "Last 1 day"),
        `3d`  = compute_window(rl_latest, 3,  "Last 3 days"),
        `7d`  = compute_window(rl_latest, 7,  "Last 7 days"),
        `14d` = compute_window(rl_latest, 14, "Last 14 days")
      )

      # 7d-vs-prior-7d trend deltas
      w7  <- compute_window(rl_latest, 7, "Last 7 days")
      # Prior 7d: created_at between 14d and 7d ago
      cutoff_7  <- Sys.time() - 7  * 86400
      cutoff_14 <- Sys.time() - 14 * 86400
      if ("created_at" %in% names(rl_latest)) {
        ca <- as.POSIXct(rl_latest$created_at, tz = "UTC")
        prior_mask <- ca >= cutoff_14 & ca < cutoff_7
        rl_prior   <- rl_latest[prior_mask, ]
        if (nrow(rl_prior) > 0L) {
          is_closed_prior <- !is.na(rl_prior$close_reason) & nzchar(as.character(rl_prior$close_reason))
          n_tot_p  <- nrow(rl_prior)
          n_cl_p   <- sum(is_closed_prior)
          rr_prior <- if (n_tot_p > 0L) n_cl_p / n_tot_p else 0

          # Median hours to close prior
          med_h_prior <- NULL
          closed_prior <- rl_prior[is_closed_prior, ]
          if (nrow(closed_prior) > 0L && "closed_at" %in% names(closed_prior)) {
            h_p <- as.numeric(difftime(
              as.POSIXct(closed_prior$closed_at,  tz = "UTC"),
              as.POSIXct(closed_prior$created_at, tz = "UTC"),
              units = "hours"
            ))
            h_p <- h_p[!is.na(h_p) & h_p >= 0]
            if (length(h_p) > 0L) med_h_prior <- median(h_p, na.rm = TRUE)
          }

          sev_col_p  <- if ("severity_max" %in% names(rl_prior)) rl_prior$severity_max else NULL
          n_crit_p   <- if (!is.null(sev_col_p)) sum(!is.na(sev_col_p) & tolower(sev_col_p) == "critical") else 0L
          n_high_p   <- if (!is.null(sev_col_p)) sum(!is.na(sev_col_p) & tolower(sev_col_p) == "high") else 0L

          # Current 7d
          sev_col_7 <- if ("severity_max" %in% names(rl_latest)) {
            rl_latest[as.POSIXct(rl_latest$created_at, tz = "UTC") >= cutoff_7, "severity_max"]
          } else NULL
          n_crit_7  <- if (!is.null(sev_col_7)) sum(!is.na(sev_col_7) & tolower(sev_col_7) == "critical") else 0L
          n_high_7  <- if (!is.null(sev_col_7)) sum(!is.na(sev_col_7) & tolower(sev_col_7) == "high") else 0L

          rr_curr <- if (!is.null(w7$resolution_rate)) w7$resolution_rate else 0
          med_curr <- w7$median_hours_to_close

          trends_out <- list(
            resolution_rate_delta_7d_vs_prior7d = round(rr_curr - rr_prior, 4),
            median_hours_delta_7d_vs_prior7d = if (!is.null(med_curr) && !is.null(med_h_prior))
              round(med_curr - med_h_prior, 2) else NULL,
            open_critical_delta_7d_vs_prior7d = as.integer(n_crit_7 - n_crit_p),
            open_high_delta_7d_vs_prior7d     = as.integer(n_high_7 - n_high_p)
          )
        } else {
          trends_out <- list(
            resolution_rate_delta_7d_vs_prior7d = 0,
            median_hours_delta_7d_vs_prior7d = NULL,
            open_critical_delta_7d_vs_prior7d = 0L,
            open_high_delta_7d_vs_prior7d     = 0L
          )
        }
      } else {
        trends_out <- list()
      }

      # Daily time-series: last 14 days, opened/closed/cumulative_open
      ts_list <- list()
      if ("created_at" %in% names(rl_latest)) {
        ts_dates <- seq(as.Date(Sys.time()) - 13L, as.Date(Sys.time()), by = "day")
        ts_dates_chr <- as.character(ts_dates)
        is_closed_all <- !is.na(rl_latest$close_reason) & nzchar(as.character(rl_latest$close_reason))
        created_dates <- as.Date(as.POSIXct(rl_latest$created_at, tz = "UTC"))

        # closed_at date; fall back to NA for open rows
        if ("closed_at" %in% names(rl_latest)) {
          closed_dates <- as.Date(as.POSIXct(rl_latest$closed_at, tz = "UTC"))
          closed_dates[!is_closed_all] <- NA
        } else {
          closed_dates <- rep(NA_integer_, nrow(rl_latest))
        }

        # Cumulative open as-of each date (reviews created before date and not closed before date)
        for (d_chr in ts_dates_chr) {
          d <- as.Date(d_chr)
          n_opened  <- sum(!is.na(created_dates) & created_dates == d)
          n_closed_d <- sum(!is.na(closed_dates) & closed_dates == d)
          # Reviews open AT END of this date: created <= d AND (not closed OR closed > d)
          cum_open  <- sum(
            !is.na(created_dates) & created_dates <= d &
            (is.na(closed_dates)  | closed_dates > d)
          )
          ts_list <- c(ts_list, list(list(
            date            = d_chr,
            opened          = as.integer(n_opened),
            closed          = as.integer(n_closed_d),
            cumulative_open = as.integer(cum_open)
          )))
        }
      }

      roborev_latest <- list(
        generated_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
        schema_version = "1.0",
        windows        = windows_out,
        trends         = trends_out,
        time_series    = ts_list
      )

      write_json(roborev_latest, roborev_latest_src, auto_unbox = TRUE, null = "null")
      cat(sprintf("  -> inst/extdata/roborev_latest.json written (7d: total=%d, closed=%d)\n",
                  windows_out[["7d"]]$total_reviews,
                  windows_out[["7d"]]$total_closed))
      tryCatch(dbDisconnect(lcon, shutdown = TRUE), error = function(e) NULL)
    } else {
      cat("  -> roborev_review_lifecycle not found; using fixture as fallback\n")
      # Copy sample fixture as fallback so the page renders in dev
      fixture_path_l <- file.path(
        Sys.getenv("HOME"),
        "docs_gh/llmtelemetry/tests/testthat/fixtures/roborev_daily_sample.json"
      )
      if (!file.exists(fixture_path_l)) {
        fixture_path_l <- file.path(
          dirname(dirname(extdata)), "tests", "testthat", "fixtures",
          "roborev_daily_sample.json"
        )
      }
      if (file.exists(fixture_path_l)) {
        file.copy(fixture_path_l, roborev_latest_src, overwrite = TRUE)
        cat("  -> copied fixture to inst/extdata/roborev_latest.json\n")
      } else {
        write_json(empty_latest, roborev_latest_src, auto_unbox = TRUE)
        cat("  -> wrote empty placeholder to inst/extdata/roborev_latest.json\n")
      }
    }
  } else {
    cat("  -> unified.duckdb not found; will copy committed source\n")
  }

  # Always copy committed source to vignettes/data/roborev/
  if (file.exists(roborev_latest_src)) {
    file.copy(roborev_latest_src, roborev_latest_dst, overwrite = TRUE)
    cat(sprintf("  -> copied inst/extdata/roborev_latest.json -> vignettes/data/roborev/latest.json (%d bytes)\n",
                file.info(roborev_latest_src)$size))
  } else {
    write_json(empty_latest, roborev_latest_dst, auto_unbox = TRUE)
    cat("  -> no committed source; wrote empty placeholder to vignettes/data/roborev/latest.json\n")
  }
}, error = function(e) {
  cat(sprintf("  -> roborev/latest.json export error: %s\n", conditionMessage(e)))
  roborev_latest_dir <- file.path(out_dir, "roborev")
  if (!dir.exists(roborev_latest_dir)) dir.create(roborev_latest_dir, recursive = TRUE)
  write_json(
    list(
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      schema_version = "1.0",
      windows = list(`1d` = list(), `3d` = list(), `7d` = list(), `14d` = list()),
      trends = list(),
      time_series = list()
    ),
    file.path(out_dir, "roborev", "latest.json"),
    auto_unbox = TRUE, null = "null"
  )
})

# --- 8i. Export roborev_repo_signal.json (#146 Q3 — per-repo signal-to-noise) -
# Signal ratio = reviews_passed / max(1, reviews_created) per repo.
# Noise ratio  = reviews_failed / max(1, reviews_created) per repo.
# Source table: roborev_daily_metrics (date, repo, reviews_created, reviews_passed,
#   reviews_failed, reviews_autoclosed_severity, reviews_autoclosed_age, parse_fail_count).
# Privacy: clean_projects() on repo column before writing.
# Guard: if table/columns missing, write empty-but-valid JSON and warn.
cat("Exporting roborev_repo_signal.json (#146 Q3)...\n")
tryCatch({
  rrs_src <- file.path(extdata, "roborev_repo_signal.json")
  rrs_dst <- file.path(out_dir, "roborev_repo_signal.json")

  empty_rrs <- list(
    repos        = list(),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  )

  if (file.exists(unified_db)) {
    rrs_con <- dbConnect(duckdb(), dbdir = unified_db, read_only = TRUE)
    on.exit(tryCatch(dbDisconnect(rrs_con, shutdown = TRUE), error = function(e) NULL),
            add = TRUE)
    rrs_tbls <- dbListTables(rrs_con)

    if ("roborev_daily_metrics" %in% rrs_tbls) {
      dm_raw <- dbReadTable(rrs_con, "roborev_daily_metrics") |> as_tibble()

      req_dm <- c("repo", "reviews_created", "reviews_passed", "reviews_failed")
      miss_dm <- setdiff(req_dm, names(dm_raw))
      if (length(miss_dm) > 0L) {
        cat(sprintf("  -> roborev_daily_metrics missing columns: %s; writing empty\n",
                    paste(miss_dm, collapse = ", ")))
        write_json(empty_rrs, rrs_src, auto_unbox = TRUE)
      } else {
        # Aggregate per repo across all dates
        has_autoclosed <- all(c("reviews_autoclosed_severity", "reviews_autoclosed_age") %in% names(dm_raw))
        has_parsefail  <- "parse_fail_count" %in% names(dm_raw)

        by_repo_dm <- dm_raw |>
          group_by(repo) |>
          summarise(
            n_created   = sum(reviews_created,   na.rm = TRUE),
            n_passed    = sum(reviews_passed,     na.rm = TRUE),
            n_failed    = sum(reviews_failed,     na.rm = TRUE),
            n_autoclosed = if (has_autoclosed)
                             sum(reviews_autoclosed_severity + reviews_autoclosed_age, na.rm = TRUE)
                           else 0L,
            n_parse_fail = if (has_parsefail)
                             sum(parse_fail_count, na.rm = TRUE)
                           else 0L,
            .groups     = "drop"
          ) |>
          mutate(
            # Signal ratio: proportion of reviews that produced actionable findings
            # (passed = fixer ran, findings were real). Exclude autoclosed reviews
            # (noise suppressed by the pipeline before human review).
            signal_ratio = round(n_passed  / pmax(1L, n_created), 4),
            noise_ratio  = round(n_failed  / pmax(1L, n_created), 4)
          ) |>
          arrange(desc(signal_ratio))

        # Apply privacy filter: clean_projects on repo column
        by_repo_dm <- clean_projects(as.data.frame(by_repo_dm), project_col = "repo")

        # Re-aggregate after remap (e.g. llmtelemetry-hook-sync → llmtelemetry)
        by_repo_dm <- aggregate(
          cbind(n_created, n_passed, n_failed, n_autoclosed, n_parse_fail) ~ repo,
          data = by_repo_dm,
          FUN  = sum
        )
        by_repo_dm$signal_ratio <- round(by_repo_dm$n_passed / pmax(1L, by_repo_dm$n_created), 4)
        by_repo_dm$noise_ratio  <- round(by_repo_dm$n_failed / pmax(1L, by_repo_dm$n_created), 4)
        by_repo_dm <- by_repo_dm[order(-by_repo_dm$signal_ratio), ]

        # Assert 0 confidential repos in output
        conf_lower_dm <- tolower(CONFIDENTIAL_PROJECTS)
        base_repos_dm <- tolower(sub(
          "-(?:feat|fix|chore|docs|refactor|test|ci|perf|style|build|revert|worktree)-.*$",
          "", by_repo_dm$repo, ignore.case = TRUE
        ))
        n_conf_dm <- sum(base_repos_dm %in% conf_lower_dm)
        if (n_conf_dm > 0L) {
          stop(sprintf(
            "roborev_repo_signal: %d confidential repo row(s) remain after clean_projects — aborting",
            n_conf_dm
          ))
        }

        # Remove noise repos (file* hash repos, bare file-id artefacts)
        by_repo_dm <- by_repo_dm[!grepl("^file[0-9a-f]{10,}", by_repo_dm$repo), ]

        rrs_out <- list(
          repos = lapply(seq_len(nrow(by_repo_dm)), function(i) {
            r <- by_repo_dm[i, , drop = FALSE]
            list(
              repo         = r$repo,
              n_created    = as.integer(r$n_created),
              n_passed     = as.integer(r$n_passed),
              n_failed     = as.integer(r$n_failed),
              n_autoclosed = as.integer(r$n_autoclosed),
              n_parse_fail = as.integer(r$n_parse_fail),
              signal_ratio = r$signal_ratio,
              noise_ratio  = r$noise_ratio
            )
          }),
          generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
        )
        write_json(rrs_out, rrs_src, auto_unbox = TRUE)
        cat(sprintf(
          "  -> roborev_repo_signal.json: %d repos (top signal: %s %.1f%%)\n",
          nrow(by_repo_dm),
          if (nrow(by_repo_dm) > 0L) by_repo_dm$repo[1L] else "n/a",
          if (nrow(by_repo_dm) > 0L) by_repo_dm$signal_ratio[1L] * 100 else 0
        ))
      }
      tryCatch(dbDisconnect(rrs_con, shutdown = TRUE), error = function(e) NULL)
    } else {
      cat("  -> roborev_daily_metrics not found; writing empty roborev_repo_signal.json\n")
      write_json(empty_rrs, rrs_src, auto_unbox = TRUE)
    }
  } else {
    cat("  -> unified.duckdb not found; will copy committed source\n")
  }

  # Always copy committed source to vignettes/data/
  if (file.exists(rrs_src)) {
    file.copy(rrs_src, rrs_dst, overwrite = TRUE)
    cat(sprintf("  -> copied roborev_repo_signal.json -> vignettes/data/ (%d bytes)\n",
                file.info(rrs_src)$size))
  } else {
    write_json(empty_rrs, rrs_dst, auto_unbox = TRUE)
    cat("  -> no committed source; wrote empty placeholder to vignettes/data/\n")
  }
}, error = function(e) {
  cat(sprintf("  -> roborev_repo_signal.json export error: %s\n", conditionMessage(e)))
  write_json(
    list(repos = list(), generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")),
    file.path(out_dir, "roborev_repo_signal.json"),
    auto_unbox = TRUE
  )
})

# --- 8n. Export roborev_cost_per_finding.json (#146 Q1) ----------------------
# Cost per finding actually resolved (USD), per agent, over time.
# Numerator proxy: pass_count per agent per day (total_cost_usd is not yet
#   instrumented in roborev_agent_performance — all NULL as of 2026-05-30).
# Denominator: close_reason='manual' count from roborev_review_lifecycle.
# NOTE: Dollar cost column is always NA until cost instrumentation lands.
#   pass_count is used as the actionable-findings proxy.
# Contract: {
#   series: [{agent, date, pass_count, addressed_count, cost_usd_per_finding}],
#   summary: [{agent, total_pass, total_addressed, cost_usd_per_finding}],
#   cost_instrumented: false,
#   generated_at: "ISO8601"
# }
cat("Exporting roborev_cost_per_finding.json (#146 Q1)...\n")
tryCatch({
  cpf_src <- file.path(extdata, "roborev_cost_per_finding.json")
  cpf_dst <- file.path(out_dir, "roborev_cost_per_finding.json")

  empty_cpf <- list(
    series             = list(),
    summary            = list(),
    cost_instrumented  = FALSE,
    generated_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  )

  if (file.exists(unified_db)) {
    cpf_con <- dbConnect(duckdb(), dbdir = unified_db, read_only = TRUE)
    on.exit(tryCatch(dbDisconnect(cpf_con, shutdown = TRUE), error = function(e) NULL),
            add = TRUE)
    cpf_tbls <- dbListTables(cpf_con)

    has_lifecycle <- "roborev_review_lifecycle" %in% cpf_tbls
    has_perf      <- "roborev_agent_performance" %in% cpf_tbls

    if (has_lifecycle && has_perf) {
      lifecycle_raw <- dbReadTable(cpf_con, "roborev_review_lifecycle") |> as_tibble()
      perf_raw      <- dbReadTable(cpf_con, "roborev_agent_performance") |> as_tibble()

      # --- per-agent per-day addressed count from lifecycle ---
      req_lc <- c("agent", "created_at", "close_reason")
      if (!all(req_lc %in% names(lifecycle_raw))) {
        cat(sprintf("  -> roborev_review_lifecycle missing cols: %s; writing empty\n",
                    paste(setdiff(req_lc, names(lifecycle_raw)), collapse = ", ")))
        write_json(empty_cpf, cpf_src, auto_unbox = TRUE)
      } else {
        # Addressed = close_reason='manual' (human manually closed after fixer acted)
        lifecycle_raw$date_str <- as.character(as.Date(
          as.POSIXct(lifecycle_raw$created_at, tz = "UTC")
        ))
        addressed_by_day <- lifecycle_raw |>
          filter(!is.na(agent)) |>
          group_by(agent, date = date_str) |>
          summarise(
            addressed_count = sum(close_reason == "manual", na.rm = TRUE),
            .groups = "drop"
          )

        # --- per-agent per-day pass_count from performance (proxy for findings) ---
        req_perf <- c("agent", "date", "pass_count")
        if (!all(req_perf %in% names(perf_raw))) {
          cat(sprintf("  -> roborev_agent_performance missing cols: %s\n",
                      paste(setdiff(req_perf, names(perf_raw)), collapse = ", ")))
          write_json(empty_cpf, cpf_src, auto_unbox = TRUE)
        } else {
          perf_by_day <- perf_raw |>
            filter(!is.na(agent)) |>
            mutate(date = as.character(date)) |>
            group_by(agent, date) |>
            summarise(
              pass_count     = sum(pass_count, na.rm = TRUE),
              total_cost_usd = if ("total_cost_usd" %in% names(perf_raw))
                                 sum(total_cost_usd, na.rm = TRUE)
                               else NA_real_,
              .groups = "drop"
            )

          # Join lifecycle addressed counts with performance pass counts
          series_df <- perf_by_day |>
            left_join(addressed_by_day, by = c("agent", "date")) |>
            mutate(
              addressed_count      = coalesce(addressed_count, 0L),
              cost_usd_per_finding = ifelse(
                !is.na(total_cost_usd) & total_cost_usd > 0 & addressed_count > 0,
                total_cost_usd / addressed_count,
                NA_real_
              )
            ) |>
            arrange(agent, date)

          # Apply privacy filter on agent column (no project data here, but defensive)
          # Agent names are not project names — no clean_projects() needed.

          # Summary: aggregate across all dates
          summary_df <- series_df |>
            group_by(agent) |>
            summarise(
              total_pass      = sum(pass_count,      na.rm = TRUE),
              total_addressed = sum(addressed_count, na.rm = TRUE),
              total_cost_usd  = if (all(is.na(total_cost_usd))) NA_real_
                                else sum(total_cost_usd, na.rm = TRUE),
              .groups = "drop"
            ) |>
            mutate(
              cost_usd_per_finding = ifelse(
                !is.na(total_cost_usd) & total_cost_usd > 0 & total_addressed > 0,
                total_cost_usd / total_addressed,
                NA_real_
              )
            ) |>
            arrange(desc(total_addressed))

          cpf_out <- list(
            series = lapply(seq_len(nrow(series_df)), function(i) {
              r <- series_df[i, , drop = FALSE]
              list(
                agent               = r$agent,
                date                = r$date,
                pass_count          = as.integer(r$pass_count),
                addressed_count     = as.integer(r$addressed_count),
                cost_usd_per_finding = if (is.na(r$cost_usd_per_finding)) NULL
                                       else r$cost_usd_per_finding
              )
            }),
            summary = lapply(seq_len(nrow(summary_df)), function(i) {
              r <- summary_df[i, , drop = FALSE]
              list(
                agent                = r$agent,
                total_pass           = as.integer(r$total_pass),
                total_addressed      = as.integer(r$total_addressed),
                cost_usd_per_finding = if (is.na(r$cost_usd_per_finding)) NULL
                                       else r$cost_usd_per_finding
              )
            }),
            cost_instrumented = FALSE,
            generated_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
          )
          write_json(cpf_out, cpf_src, auto_unbox = TRUE)
          cat(sprintf(
            "  -> roborev_cost_per_finding.json: %d agent-day rows, %d agents\n",
            nrow(series_df), nrow(summary_df)
          ))
        }
      }
      tryCatch(dbDisconnect(cpf_con, shutdown = TRUE), error = function(e) NULL)
    } else {
      missing_t <- paste(c(
        if (!has_lifecycle) "roborev_review_lifecycle",
        if (!has_perf)      "roborev_agent_performance"
      ), collapse = ", ")
      cat(sprintf("  -> tables not found (%s); writing empty\n", missing_t))
      write_json(empty_cpf, cpf_src, auto_unbox = TRUE)
    }
  } else {
    cat("  -> unified.duckdb not found; will copy committed source\n")
  }

  if (file.exists(cpf_src)) {
    file.copy(cpf_src, cpf_dst, overwrite = TRUE)
    cat(sprintf("  -> copied roborev_cost_per_finding.json -> vignettes/data/ (%d bytes)\n",
                file.info(cpf_src)$size))
  } else {
    write_json(empty_cpf, cpf_dst, auto_unbox = TRUE)
    cat("  -> no committed source; wrote empty placeholder to vignettes/data/\n")
  }
}, error = function(e) {
  cat(sprintf("  -> roborev_cost_per_finding.json export error: %s\n", conditionMessage(e)))
  write_json(
    list(series = list(), summary = list(), cost_instrumented = FALSE,
         generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")),
    file.path(out_dir, "roborev_cost_per_finding.json"),
    auto_unbox = TRUE
  )
})

# --- 8o. Export roborev_cost_per_agent.json (#146 Q11) -----------------------
# Cheapest agent per resolved finding — aggregate view (no time series).
# Derived from roborev_cost_per_finding.json summary block.
# Contract: {agents:[{agent,total_pass,total_addressed,cost_usd_per_finding}],
#   cost_instrumented:false, generated_at:"ISO8601"}
cat("Exporting roborev_cost_per_agent.json (#146 Q11)...\n")
tryCatch({
  cpa_src <- file.path(extdata, "roborev_cost_per_agent.json")
  cpa_dst <- file.path(out_dir, "roborev_cost_per_agent.json")

  empty_cpa <- list(
    agents            = list(),
    cost_instrumented = FALSE,
    generated_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  )

  # Derive from unified.duckdb (same query as 8n but without the time series)
  if (file.exists(unified_db)) {
    cpa_con <- dbConnect(duckdb(), dbdir = unified_db, read_only = TRUE)
    on.exit(tryCatch(dbDisconnect(cpa_con, shutdown = TRUE), error = function(e) NULL),
            add = TRUE)
    cpa_tbls <- dbListTables(cpa_con)

    has_lc_cpa <- "roborev_review_lifecycle" %in% cpa_tbls
    has_perf_cpa <- "roborev_agent_performance" %in% cpa_tbls

    if (has_lc_cpa && has_perf_cpa) {
      lc_cpa   <- dbReadTable(cpa_con, "roborev_review_lifecycle") |> as_tibble()
      perf_cpa <- dbReadTable(cpa_con, "roborev_agent_performance") |> as_tibble()

      req_lc_cpa <- c("agent", "close_reason")
      req_perf_cpa <- c("agent", "pass_count")

      if (!all(req_lc_cpa %in% names(lc_cpa)) ||
          !all(req_perf_cpa %in% names(perf_cpa))) {
        cat("  -> required columns missing; writing empty roborev_cost_per_agent.json\n")
        write_json(empty_cpa, cpa_src, auto_unbox = TRUE)
      } else {
        # Addressed per agent from lifecycle
        addressed_cpa <- lc_cpa |>
          filter(!is.na(agent)) |>
          group_by(agent) |>
          summarise(total_addressed = sum(close_reason == "manual", na.rm = TRUE),
                    .groups = "drop")

        # Pass count + cost per agent from performance
        perf_agg_cpa <- perf_cpa |>
          filter(!is.na(agent)) |>
          group_by(agent) |>
          summarise(
            total_pass     = sum(pass_count, na.rm = TRUE),
            total_cost_usd = if ("total_cost_usd" %in% names(perf_cpa))
                               sum(total_cost_usd, na.rm = TRUE)
                             else NA_real_,
            .groups = "drop"
          )

        cpa_df <- perf_agg_cpa |>
          left_join(addressed_cpa, by = "agent") |>
          mutate(
            total_addressed      = coalesce(total_addressed, 0L),
            cost_usd_per_finding = ifelse(
              !is.na(total_cost_usd) & total_cost_usd > 0 & total_addressed > 0,
              total_cost_usd / total_addressed,
              NA_real_
            )
          ) |>
          arrange(cost_usd_per_finding, desc(total_addressed))  # cheapest first

        cpa_out <- list(
          agents = lapply(seq_len(nrow(cpa_df)), function(i) {
            r <- cpa_df[i, , drop = FALSE]
            list(
              agent                = r$agent,
              total_pass           = as.integer(r$total_pass),
              total_addressed      = as.integer(r$total_addressed),
              cost_usd_per_finding = if (is.na(r$cost_usd_per_finding)) NULL
                                     else r$cost_usd_per_finding
            )
          }),
          cost_instrumented = FALSE,
          generated_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
        )
        write_json(cpa_out, cpa_src, auto_unbox = TRUE)
        cat(sprintf("  -> roborev_cost_per_agent.json: %d agents\n", nrow(cpa_df)))
        tryCatch(dbDisconnect(cpa_con, shutdown = TRUE), error = function(e) NULL)
      }
    } else {
      cat("  -> required tables missing; writing empty roborev_cost_per_agent.json\n")
      write_json(empty_cpa, cpa_src, auto_unbox = TRUE)
    }
  } else {
    cat("  -> unified.duckdb not found; will copy committed source\n")
  }

  if (file.exists(cpa_src)) {
    file.copy(cpa_src, cpa_dst, overwrite = TRUE)
    cat(sprintf("  -> copied roborev_cost_per_agent.json -> vignettes/data/ (%d bytes)\n",
                file.info(cpa_src)$size))
  } else {
    write_json(empty_cpa, cpa_dst, auto_unbox = TRUE)
    cat("  -> no committed source; wrote empty placeholder to vignettes/data/\n")
  }
}, error = function(e) {
  cat(sprintf("  -> roborev_cost_per_agent.json export error: %s\n", conditionMessage(e)))
  write_json(
    list(agents = list(), cost_instrumented = FALSE,
         generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")),
    file.path(out_dir, "roborev_cost_per_agent.json"),
    auto_unbox = TRUE
  )
})

# --- 8p. Export roborev_vs_session_cost.json (#146 Q20) ----------------------
# Session execution cost vs roborev review cost, daily, over time.
# Session cost proxy: unified_costs.json total_cost (claude API daily cost).
# Review cost proxy: roborev_agent_performance.total_cost_usd (NULL until
#   cost instrumentation lands — column will be zero-filled with a note).
# Healthy ratio: roborev review cost should be <10-20% of session cost.
# Contract: {
#   daily: [{date, session_cost_usd, review_cost_usd, review_pct}],
#   overall: {session_total, review_total, review_pct},
#   cost_instrumented: false,
#   note: "Review costs not yet instrumented ...",
#   generated_at: "ISO8601"
# }
cat("Exporting roborev_vs_session_cost.json (#146 Q20)...\n")
tryCatch({
  vsr_src <- file.path(extdata, "roborev_vs_session_cost.json")
  vsr_dst <- file.path(out_dir, "roborev_vs_session_cost.json")

  empty_vsr <- list(
    daily             = list(),
    overall           = list(session_total = 0, review_total = 0, review_pct = 0),
    cost_instrumented = FALSE,
    note              = paste0(
      "Review costs (roborev_agent_performance.total_cost_usd) are not yet ",
      "instrumented — all NULL as of 2026-05-30. Session costs are the total ",
      "Claude API daily cost from unified_costs.json. The review_cost_usd column ",
      "is zero-filled until instrumentation lands."
    ),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  )

  if (file.exists(unified_db)) {
    vsr_con <- dbConnect(duckdb(), dbdir = unified_db, read_only = TRUE)
    on.exit(tryCatch(dbDisconnect(vsr_con, shutdown = TRUE), error = function(e) NULL),
            add = TRUE)
    vsr_tbls <- dbListTables(vsr_con)

    # Session cost: daily total from unified_costs (date, total_cost)
    has_costs_tbl <- "costs" %in% vsr_tbls
    has_perf_vsr  <- "roborev_agent_performance" %in% vsr_tbls

    if (has_costs_tbl) {
      costs_raw_vsr <- dbReadTable(vsr_con, "costs") |> as_tibble()
      # unified_costs has columns: date, opus_cost, sonnet_cost, haiku_cost, total_cost
      req_c <- c("date", "total_cost")
      if (!all(req_c %in% names(costs_raw_vsr))) {
        cat(sprintf("  -> costs table missing cols %s; writing empty\n",
                    paste(setdiff(req_c, names(costs_raw_vsr)), collapse = ", ")))
        write_json(empty_vsr, vsr_src, auto_unbox = TRUE)
      } else {
        session_by_day <- costs_raw_vsr |>
          mutate(date = as.character(as.Date(date))) |>
          group_by(date) |>
          summarise(session_cost_usd = sum(total_cost, na.rm = TRUE), .groups = "drop")

        # Review cost: from roborev_agent_performance.total_cost_usd (all NULL currently)
        if (has_perf_vsr) {
          perf_vsr <- dbReadTable(vsr_con, "roborev_agent_performance") |> as_tibble()
          review_by_day <- if ("total_cost_usd" %in% names(perf_vsr)) {
            perf_vsr |>
              mutate(date = as.character(date)) |>
              group_by(date) |>
              summarise(review_cost_usd = sum(total_cost_usd, na.rm = TRUE), .groups = "drop")
          } else {
            tibble(date = character(0), review_cost_usd = numeric(0))
          }
        } else {
          review_by_day <- tibble(date = character(0), review_cost_usd = numeric(0))
        }

        # Join session + review cost by date (left join on session dates)
        daily_vsr <- session_by_day |>
          left_join(review_by_day, by = "date") |>
          mutate(
            review_cost_usd = coalesce(review_cost_usd, 0.0),
            review_pct      = ifelse(
              session_cost_usd > 0,
              round(review_cost_usd / session_cost_usd * 100, 2),
              0.0
            )
          ) |>
          arrange(date)

        overall_session <- sum(daily_vsr$session_cost_usd, na.rm = TRUE)
        overall_review  <- sum(daily_vsr$review_cost_usd,  na.rm = TRUE)
        overall_pct     <- if (overall_session > 0) {
          round(overall_review / overall_session * 100, 2)
        } else 0.0

        vsr_out <- list(
          daily = lapply(seq_len(nrow(daily_vsr)), function(i) {
            r <- daily_vsr[i, , drop = FALSE]
            list(
              date             = r$date,
              session_cost_usd = round(as.numeric(r$session_cost_usd), 4),
              review_cost_usd  = round(as.numeric(r$review_cost_usd),  4),
              review_pct       = r$review_pct
            )
          }),
          overall = list(
            session_total = round(overall_session, 4),
            review_total  = round(overall_review,  4),
            review_pct    = overall_pct
          ),
          cost_instrumented = FALSE,
          note = paste0(
            "Review costs (roborev_agent_performance.total_cost_usd) are not yet ",
            "instrumented — all NULL as of 2026-05-30. Session costs are the total ",
            "Claude API daily cost from unified_costs (opus+sonnet+haiku). The ",
            "review_cost_usd column is zero-filled until instrumentation lands."
          ),
          generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
        )
        write_json(vsr_out, vsr_src, auto_unbox = TRUE)
        cat(sprintf(
          "  -> roborev_vs_session_cost.json: %d daily rows, session=$%.2f, review=$%.4f\n",
          nrow(daily_vsr), overall_session, overall_review
        ))
        tryCatch(dbDisconnect(vsr_con, shutdown = TRUE), error = function(e) NULL)
      }
    } else {
      cat("  -> costs table not found; writing empty roborev_vs_session_cost.json\n")
      write_json(empty_vsr, vsr_src, auto_unbox = TRUE)
    }
  } else {
    cat("  -> unified.duckdb not found; will copy committed source\n")
  }

  if (file.exists(vsr_src)) {
    file.copy(vsr_src, vsr_dst, overwrite = TRUE)
    cat(sprintf("  -> copied roborev_vs_session_cost.json -> vignettes/data/ (%d bytes)\n",
                file.info(vsr_src)$size))
  } else {
    write_json(empty_vsr, vsr_dst, auto_unbox = TRUE)
    cat("  -> no committed source; wrote empty placeholder to vignettes/data/\n")
  }
}, error = function(e) {
  cat(sprintf("  -> roborev_vs_session_cost.json export error: %s\n", conditionMessage(e)))
  write_json(
    list(daily = list(),
         overall = list(session_total = 0, review_total = 0, review_pct = 0),
         cost_instrumented = FALSE,
         note = "Export error — see server logs",
         generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")),
    file.path(out_dir, "roborev_vs_session_cost.json"),
    auto_unbox = TRUE
  )
})

# --- 9. QA validation — fail early on empty critical data ---------------------
cat("\n=== Data QA Validation ===\n")
# ccusage_blocks is intentionally excluded from critical_files: the CI fallback
# writes an empty array [] when cmonitor-rs is unavailable (issue #141).
# Schema validation for ccusage_blocks (valid JSON array, length 0 allowed) runs
# separately below.
critical_files <- c("ccusage_daily", "git_commits",
                     "git_bus_factor", "git_velocity", "git_timing",
                     "git_churn", "git_bugs")
qa_errors <- 0L
count_json_rows <- function(path) {
  d <- fromJSON(path, simplifyDataFrame = TRUE)
  if (is.data.frame(d)) return(nrow(d))
  if (is.list(d)) {
    # Nested: sum rows of all data.frame children (e.g. bus_factor$all_time)
    total <- 0L
    for (v in d) {
      if (is.data.frame(v)) total <- total + nrow(v)
      else if (is.list(v)) {
        for (vv in v) {
          if (is.data.frame(vv)) total <- total + nrow(vv)
        }
      }
    }
    return(total)
  }
  0L
}
for (f in critical_files) {
  path <- file.path(out_dir, paste0(f, ".json"))
  if (!file.exists(path)) {
    cat(sprintf("  QA ERROR: %s.json missing\n", f))
    qa_errors <- qa_errors + 1L
    next
  }
  n <- count_json_rows(path)
  if (n == 0L) {
    cat(sprintf("  QA ERROR: %s.json has 0 rows — chart will show 'No data available'\n", f))
    qa_errors <- qa_errors + 1L
  } else {
    cat(sprintf("  QA OK: %s.json (%d rows)\n", f, n))
  }
}
if (qa_errors > 0L) {
  stop(sprintf("QA FAILED: %d critical data files are empty", qa_errors))
}
cat("QA passed: all critical data files have rows\n\n")

# Schema-only validation for optional files: valid JSON array, length 0 allowed.
# ccusage_blocks.json is written as [] by the CI fallback (no cmonitor-rs).
optional_schema_files <- c("ccusage_blocks")
for (f in optional_schema_files) {
  path <- file.path(out_dir, paste0(f, ".json"))
  if (!file.exists(path)) {
    cat(sprintf("  QA WARN: %s.json missing (optional)\n", f))
    next
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE, simplifyDataFrame = FALSE,
                      simplifyMatrix = FALSE),
    error = function(e) {
      stop(sprintf("QA FAILED: %s.json is not valid JSON: %s", f, conditionMessage(e)))
    }
  )
  # Must be a JSON array: an unnamed (plain) list. Named lists are JSON objects;
  # scalars are not lists at all. Both must be rejected (#141 round-3, #154).
  # Use !is.null(names(parsed)) rather than length(names(parsed)) > 0L:
  # jsonlite parses {} as a named list with names = character(0) (length 0 but
  # NOT NULL), so the length-based guard incorrectly accepts {} as a valid array.
  if (!is.list(parsed) || !is.null(names(parsed))) {
    stop(sprintf(
      "QA FAILED: %s.json must be a JSON array but got %s",
      f, if (!is.list(parsed)) class(parsed)[1L] else "JSON object (named list)"
    ))
  }
  n_opt <- length(parsed)
  cat(sprintf("  QA OK (optional): %s.json (%d rows — empty array is valid)\n", f, n_opt))
}

cat("Done. Output in", out_dir, "\n")
