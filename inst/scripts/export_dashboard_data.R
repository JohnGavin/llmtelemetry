#!/usr/bin/env Rscript
# Export dashboard data: flatten/trim raw data to browser-friendly JSON
# Run: Rscript inst/scripts/export_dashboard_data.R

library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(DBI)
library(duckdb)

pkg_root <- here::here()
extdata  <- file.path(pkg_root, "inst", "extdata")
out_dir  <- file.path(pkg_root, "vignettes", "data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Helper: shorten project path to last meaningful component
shorten_project <- function(x) {
  x |>
    gsub("^-Users-johngavin-docs[-_]gh-", "", x = _) |>
    gsub("^llm-", "", x = _) |>
    gsub("^proj-", "", x = _) |>
    gsub("-", "/", x = _)
}

# --- 1. Flatten ccusage daily -------------------------------------------------
cat("Exporting ccusage daily...\n")
raw <- fromJSON(file.path(extdata, "ccusage_daily_all.json"), simplifyDataFrame = FALSE)
daily_rows <- lapply(names(raw$projects), function(proj) {
  entries <- raw$projects[[proj]]
  lapply(entries, function(e) {
    tibble(
      project       = shorten_project(proj),
      date          = e$date,
      inputTokens   = e$inputTokens,
      outputTokens  = e$outputTokens,
      cacheCreation = e$cacheCreationTokens,
      cacheRead     = e$cacheReadTokens,
      totalTokens   = e$totalTokens,
      totalCost     = round(e$totalCost, 4),
      modelsUsed    = paste(e$modelsUsed, collapse = ", ")
    )
  }) |> bind_rows()
}) |> bind_rows() |> arrange(date, project)
write_json(daily_rows, file.path(out_dir, "ccusage_daily.json"), auto_unbox = TRUE)
cat(sprintf("  -> %d rows\n", nrow(daily_rows)))

# --- 2. Trim ccusage sessions -------------------------------------------------
cat("Exporting ccusage sessions...\n")
raw_sess <- fromJSON(file.path(extdata, "ccusage_session_all.json"), simplifyDataFrame = FALSE)
sess_rows <- lapply(raw_sess$sessions, function(s) {
  tibble(
    sessionId     = shorten_project(s$sessionId),
    inputTokens   = s$inputTokens,
    outputTokens  = s$outputTokens,
    cacheCreation = s$cacheCreationTokens,
    cacheRead     = s$cacheReadTokens,
    totalTokens   = s$totalTokens,
    totalCost     = round(s$totalCost, 4),
    lastActivity  = s$lastActivity,
    modelsUsed    = paste(s$modelsUsed, collapse = ", ")
  )
}) |> bind_rows() |> arrange(desc(totalCost))
write_json(sess_rows, file.path(out_dir, "ccusage_sessions.json"), auto_unbox = TRUE)
cat(sprintf("  -> %d sessions\n", nrow(sess_rows)))

# --- 3. Filter ccusage blocks (active only) -----------------------------------
cat("Exporting ccusage blocks...\n")
raw_blk <- fromJSON(file.path(extdata, "ccusage_blocks_all.json"), simplifyDataFrame = FALSE)
blk_rows <- lapply(raw_blk$blocks, function(b) {
  if (isTRUE(b$isGap) || is.null(b$costUSD) || b$costUSD <= 0) return(NULL)
  tc <- b$tokenCounts
  tibble(
    startTime     = b$startTime,
    endTime       = b$endTime,
    actualEndTime = b$actualEndTime %||% NA_character_,
    entries       = b$entries,
    inputTokens   = tc$inputTokens,
    outputTokens  = tc$outputTokens,
    cacheCreation = tc$cacheCreationInputTokens,
    cacheRead     = tc$cacheReadInputTokens,
    totalTokens   = b$totalTokens,
    costUSD       = round(b$costUSD, 4),
    models        = paste(setdiff(b$models, "<synthetic>"), collapse = ", ")
  )
}) |> bind_rows() |> arrange(startTime)
write_json(blk_rows, file.path(out_dir, "ccusage_blocks.json"), auto_unbox = TRUE)
cat(sprintf("  -> %d active blocks\n", nrow(blk_rows)))

# --- 4. Export Gemini from DuckDB ---------------------------------------------
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

# --- 5. Extract cmonitor summary ----------------------------------------------
cat("Exporting cmonitor summary...\n")
cmon_file <- file.path(extdata, "cmonitor_daily.txt")
if (file.exists(cmon_file)) {
  txt <- readLines(cmon_file, warn = FALSE)
  clean <- gsub("\033\\[[0-9;]*m", "", txt)  # strip ANSI codes

  extract_num <- function(pattern) {
    m <- regmatches(clean, regexpr(pattern, clean, perl = TRUE))
    if (length(m) == 0) return(NA_real_)
    as.numeric(gsub("[^0-9.]", "", m[1]))
  }

  cmon_summary <- list(
    date_range   = {
      m <- regmatches(clean, regexpr("\\d{4}-\\d{2}-\\d{2} to \\d{4}-\\d{2}-\\d{2}", clean))
      if (length(m) > 0) m[1] else NA_character_
    },
    total_tokens = extract_num("Total Tokens:\\s*[0-9,]+"),
    total_cost   = extract_num("Total Cost:\\s*\\$[0-9,.]+"),
    entries      = extract_num("Entries:\\s*[0-9,]+")
  )
  write_json(cmon_summary, file.path(out_dir, "cmonitor_summary.json"), auto_unbox = TRUE)
  cat(sprintf("  -> cost=$%.2f, tokens=%s\n",
    cmon_summary$total_cost, format(cmon_summary$total_tokens, big.mark = ",")))
} else {
  cat("  -> cmonitor_daily.txt not found, skipping\n")
  write_json(list(), file.path(out_dir, "cmonitor_summary.json"))
}

# --- 6. Extract git commit stats -----------------------------------------------
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
      description = "Daily Claude API usage aggregated by project",
      type        = "array",
      source      = "ccusage CLI (npx ccusage daily --json --instances)",
      source_url  = "https://www.npmjs.com/package/ccusage",
      schema      = list(
        project       = "string",
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
      description = "Claude API sessions with token and cost totals",
      type        = "array",
      source      = "ccusage CLI (npx ccusage session --json --instances)",
      source_url  = "https://www.npmjs.com/package/ccusage",
      schema      = list(
        sessionId     = "string",
        inputTokens   = "integer",
        outputTokens  = "integer",
        cacheCreation = "integer",
        cacheRead     = "integer",
        totalTokens   = "integer",
        totalCost     = "number",
        lastActivity  = "string",
        modelsUsed    = "string"
      )
    ),
    list(
      path        = "/ccusage_blocks.json",
      description = "Claude API usage grouped into contiguous time blocks",
      type        = "array",
      source      = "ccusage CLI (npx ccusage blocks --json --instances)",
      source_url  = "https://www.npmjs.com/package/ccusage",
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
      description = "Aggregated Anthropic cmonitor usage summary",
      type        = "object",
      source      = "Anthropic cmonitor CLI text output",
      source_url  = "https://docs.anthropic.com/",
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
