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

  gem_daily <- tbl(con, "daily_usage") |> collect() |>
    mutate(date = as.character(date), total_cost = round(total_cost, 4))
  write_json(gem_daily, file.path(out_dir, "gemini_daily.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d daily rows\n", nrow(gem_daily)))

  gem_sess <- tbl(con, "sessions_summary") |> collect() |>
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

cat("Done. Output in", out_dir, "\n")
