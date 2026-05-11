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

  u_sess <- dbReadTable(ucon, "sessions") |>
    as_tibble() |>
    mutate(
      started_at = as.character(started_at),
      ended_at = as.character(ended_at),
      # Guard against negative durations (timezone issues)
      duration_min = pmax(0, round(duration_min, 1), na.rm = TRUE)
    ) |>
    select(session_id, project, started_at, ended_at, duration_min) |>
    arrange(desc(started_at))
  # Write to both locations: inst/extdata (commit) + vignettes/data (preview)
  write_json(u_sess, file.path(extdata, "unified_sessions.json"), auto_unbox = TRUE)
  write_json(u_sess, file.path(out_dir, "unified_sessions.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d sessions (written to inst/extdata + vignettes/data)\n", nrow(u_sess)))

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
  write_json(out_data, file.path(out_dir, "cost_by_project_estimated.json"), auto_unbox = TRUE)
  write_json(out_data, file.path(extdata, "cost_by_project_estimated.json"), auto_unbox = TRUE)
  cat(sprintf("  -> %d project-day cost estimates (written to inst/extdata + vignettes/data)\n", nrow(out_data)))
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
    )
  )
)
write_json(api_index, file.path(out_dir, "index.json"), auto_unbox = TRUE, pretty = TRUE)
cat("  -> index.json written\n")

# --- 8b. Export git file growth from git pulse parquet -----------------------
cat("Exporting git file growth data...\n")
git_pulse_dir <- file.path(Sys.getenv("HOME"), ".claude/logs/git")
parquet_files <- list.files(git_pulse_dir, pattern = "\\.parquet$", full.names = TRUE)

if (length(parquet_files) > 0) {
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
  cat("  -> no parquet files found\n")
  write_json(list(), file.path(out_dir, "git_file_growth.json"))
}

# --- 9. QA validation — fail early on empty critical data ---------------------
cat("\n=== Data QA Validation ===\n")
critical_files <- c("ccusage_daily", "ccusage_blocks", "git_commits",
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

cat("Done. Output in", out_dir, "\n")
