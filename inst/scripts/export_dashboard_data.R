#!/usr/bin/env Rscript
# Export dashboard data: flatten/trim raw data to browser-friendly JSON
# Run: Rscript inst/scripts/export_dashboard_data.R
# Data source: cmonitor-rs CLI for Claude usage, DuckDB for Gemini

library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(DBI)
library(duckdb)

pkg_root <- here::here()
extdata  <- file.path(pkg_root, "inst", "extdata")
out_dir  <- file.path(pkg_root, "vignettes", "data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

cmonitor_bin <- "/Users/johngavin/.cargo/bin/cmonitor-rs"

# Helper: convert cmonitor-rs time array to "YYYY-MM-DD HH:MM:SS" string
# t = c(year, day_of_year, hour, min, sec, nanosec, ...)
parse_cmonitor_time <- function(t) {
  if (is.null(t) || length(t) < 5) return(NA_character_)
  origin <- as.Date(paste0(t[1], "-01-01"))
  d <- origin + (t[2] - 1L)
  sprintf("%s %02d:%02d:%02d", d, t[3], t[4], t[5])
}

# Helper: shorten project path to last meaningful component (kept for legacy use)
shorten_project <- function(x) {
  x |>
    gsub("^-Users-johngavin-docs[-_]gh-", "", x = _) |>
    gsub("^llm-", "", x = _) |>
    gsub("^proj-", "", x = _) |>
    gsub("-", "/", x = _)
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
  cat(sprintf("  -> %d active blocks\n", nrow(blk_rows)))

  # --- 3b. Per-model daily breakdown from model_stats -------------------------
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
  # Map source files to expected output names (dashboard expects these exact names)
  fallback_map <- list(
    "ccusage_daily_all.json"   = "ccusage_daily.json",
    "ccusage_session_all.json" = "ccusage_sessions.json",
    "ccusage_blocks_all.json"  = "ccusage_blocks.json"
  )
  for (f in names(fallback_map)) {
    src <- file.path(extdata, f)
    dst <- file.path(out_dir, fallback_map[[f]])
    if (file.exists(src)) {
      file.copy(src, dst, overwrite = TRUE)
      cat(sprintf("  -> copied %s\n", f))
    } else if (!file.exists(dst)) {
      write_json(list(), dst, auto_unbox = TRUE)
      cat(sprintf("  -> %s not found, wrote empty\n", f))
    } else {
      cat(sprintf("  -> using existing %s\n", basename(dst)))
    }
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

# --- 7. Generate API index.json -----------------------------------------------
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
    )
  )
)
write_json(api_index, file.path(out_dir, "index.json"), auto_unbox = TRUE, pretty = TRUE)
cat("  -> index.json written\n")

cat("Done. Output in", out_dir, "\n")
