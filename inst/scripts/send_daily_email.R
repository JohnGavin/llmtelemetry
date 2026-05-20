# send_daily_email.R
# Send daily LLM usage report via Gmail
# Called from GitHub Actions workflow: .github/workflows/daily-llm-report.yaml

library(blastula)
library(dplyr)
library(tibble)
library(jsonlite)
library(purrr)
library(lubridate)
library(DBI)
library(duckdb)
library(here)

# Load cached ccusage data from inst/extdata/
load_cached <- function(type) {
  path <- here::here(sprintf("inst/extdata/ccusage_%s_all.json", type))
  if (!file.exists(path)) {
    message(sprintf("Cache file not found: %s", path))
    return(NULL)
  }
  tryCatch({
    fromJSON(path)
  }, error = function(e) {
    message(sprintf("Failed to parse %s: %s", path, e$message))
    NULL
  })
}

daily_raw <- load_cached("daily")
session_raw <- load_cached("session")
blocks_raw <- load_cached("blocks")

# Load codex data (graceful: may be empty/missing initially)
codex_d <- tryCatch(
  jsonlite::fromJSON(here::here("inst/extdata/codex_daily.json")),
  error = function(e) NULL
)
codex_s <- tryCatch(
  jsonlite::fromJSON(here::here("inst/extdata/codex_sessions.json")),
  error = function(e) NULL
)
# Normalise to tibble (fromJSON returns data.frame for arrays)
if (!is.null(codex_d) && (is.data.frame(codex_d) || is.list(codex_d))) {
  codex_d <- tryCatch(as_tibble(codex_d), error = function(e) NULL)
}
if (!is.null(codex_s) && (is.data.frame(codex_s) || is.list(codex_s))) {
  codex_s <- tryCatch(as_tibble(codex_s), error = function(e) NULL)
}
# Empty tibbles count as no data
has_codex <- !is.null(codex_d) && nrow(codex_d) > 0

has_data <- !is.null(daily_raw) || !is.null(session_raw)

# Helper: normalize to character vector (handles NULL, list, character)
normalize_to_char_vec <- function(x) {
  switch(class(x)[1],
    "NULL" = character(0),
    "list" = as.character(unlist(x)),
    as.character(x)
  ) |> (\(v) if (length(v) == 0) character(0) else v)()
}

# Parse daily data
parse_daily <- function(json) {
  if (is.null(json$projects)) return(NULL)
  names(json$projects) |>
    map_dfr(\(p) {
      d <- json$projects[[p]]
      if (is.null(d) || length(d) == 0) return(NULL)
      as_tibble(d) |>
        mutate(
          project = p,
          across(any_of("modelsUsed"), ~ map(.x, normalize_to_char_vec))
        )
    })
}

daily_data <- parse_daily(daily_raw)
session_data <- if (!is.null(session_raw$sessions)) {
  as_tibble(session_raw$sessions)
} else NULL

today <- Sys.Date()

# Helper for safe date conversion
safe_date <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA)
  tryCatch(as.Date(x), error = function(e) NA)
}

# Helper for formatting
dollar <- function(x) {
  if (is.na(x) || is.null(x) || is.nan(x)) return("-")
  sprintf("$%.2f", x)
}
comma <- function(x) {
  if (is.na(x) || is.null(x) || is.nan(x)) return("-")
  format(x, big.mark = ",", scientific = FALSE)
}
millions <- function(x) {
  if (is.na(x) || is.null(x) || is.nan(x)) return("-")
  sprintf("%.1fM", x / 1e6)
}
format_hhmm <- function(mins) sprintf("%02d:%02d", as.integer(mins %/% 60), as.integer(mins %% 60))

# Dark mode color palette
dark_bg <- "#1a1a2e"
dark_card <- "#16213e"
dark_row_alt <- "#0f3460"
dark_text <- "#e8e8e8"
dark_muted <- "#a0a0a0"
dark_border <- "#2a2a4a"
accent_green <- "#00d26a"
accent_blue <- "#4fc3f7"
accent_purple <- "#bb86fc"
accent_orange <- "#ff9800"

# Get cache timestamp
cache_time <- if (!is.null(session_raw$generatedAt)) {
  session_raw$generatedAt
} else if (file.exists("inst/extdata/ccusage_session_all.json")) {
  format(file.info("inst/extdata/ccusage_session_all.json")$mtime, "%Y-%m-%d %H:%M:%S")
} else {
  "Unknown"
}

# Check for stale data
if (cache_time != "Unknown") {
  cache_ts <- tryCatch(as.POSIXct(cache_time), error = function(e) NA)
  if (!is.na(cache_ts) && difftime(Sys.time(), cache_ts, units = "hours") > 24) {
    message(sprintf("WARNING: Cached data is stale (generated at %s). Report may be outdated.", cache_time))
  }
}

if (!has_data) {
  email_body <- sprintf('\n<div style="background-color: %s; color: %s; padding: 20px; font-family: -apple-system, BlinkMacSystemFont, sans-serif;">
<h2 style="color: %s; margin-bottom: 5px;">LLM Usage Report - %s</h2>
<div style="background-color: #3d2c00; border: 1px solid #ffc107; padding: 15px; border-radius: 5px; color: #ffd54f;">
  <strong>No cached data available</strong>
  <p>Run locally: <code style="background-color: %s; padding: 2px 6px; border-radius: 3px;">Rscript R/scripts/refresh_ccusage_cache.R</code></p>
</div>
</div>
', dark_bg, dark_text, accent_orange, today, dark_card)
} else {
  # Calculate weekly stats (1-4 weeks back)
    calc_weekly <- function(weeks_back) {
      if (is.null(daily_data) || nrow(daily_data) == 0 || !"date" %in% names(daily_data)) {
        return(list(cost = 0, tokens = 0))
      }
      end_date <- today - (weeks_back - 1) * 7
      start_date <- end_date - 6
      weekly <- daily_data |>
        filter(safe_date(.data$date) >= start_date, safe_date(.data$date) <= end_date)
      list(
        cost = sum(weekly$totalCost, na.rm = TRUE),
        tokens = sum(weekly$totalTokens, na.rm = TRUE)
      )
    }

  week1 <- calc_weekly(1)
  week2 <- calc_weekly(2)
  week3 <- calc_weekly(3)
  week4 <- calc_weekly(4)

  # --- 1. Calculate ccusage stats ---
  total_cost <- if (!is.null(daily_data)) sum(daily_data$totalCost, na.rm = TRUE) else 0
  total_tokens <- if (!is.null(daily_data)) sum(daily_data$totalTokens, na.rm = TRUE) else 0
  n_sessions <- if (!is.null(session_data)) nrow(session_data) else 0
  
  cc_start <- if (!is.null(daily_data) && nrow(daily_data) > 0) min(safe_date(daily_data$date), na.rm=TRUE) else NA
  cc_end <- if (!is.null(daily_data) && nrow(daily_data) > 0) max(safe_date(daily_data$date), na.rm=TRUE) else NA
  cc_days <- if (!is.na(cc_start) && !is.na(cc_end)) as.numeric(cc_end - cc_start) + 1 else 0
  cc_entries <- if (!is.null(blocks_raw$blocks)) nrow(blocks_raw$blocks) else NA
  
  cc_cost_day <- if (cc_days > 0) total_cost / cc_days else NA
  cc_tok_day <- if (cc_days > 0) total_tokens / cc_days else NA

  # --- 2. Calculate cmonitor stats ---
  cm_cost <- NA; cm_tokens <- NA; cm_entries <- NA; cm_days <- NA; cm_start <- NA; cm_end <- NA
  cm_cost_day <- NA; cm_tok_day <- NA
  
  cmonitor_path <- here::here("inst/extdata/cmonitor_daily.txt")
  if (file.exists(cmonitor_path)) {
    lines <- readLines(cmonitor_path, warn = FALSE)
    lines <- lines[!grepl("Setup complete|Terminal wrapper|RSTUDIO_TERM_EXEC|unpacking", lines)]
    lines <- gsub("\033\\[[0-9;]*[a-zA-Z]", "", lines)
    full_text <- paste(lines, collapse = "\n")
    
    extract <- function(pattern, text) {
      m <- regexec(pattern, text)
      if (m[[1]][1] == -1) return(NULL)
      regmatches(text, m)[[1]][2]
    }
    
    cost_val <- extract("Total Cost:\\s*\\$([0-9,.]+)", full_text)
    tokens_val <- extract("Total Tokens:\\s*([0-9,]+)", full_text)
    entries_val <- extract("Entries:\\s*([0-9,]+)", full_text)
    period_val <- extract("Daily Usage Summary\\s*-\\s*([0-9-]+\\s*to\\s*[0-9-]+)", full_text)
    
    if (!is.null(cost_val)) cm_cost <- as.numeric(gsub("[$,]", "", cost_val))
    if (!is.null(tokens_val)) cm_tokens <- as.numeric(gsub("[,]", "", tokens_val))
    if (!is.null(entries_val)) cm_entries <- as.numeric(gsub("[,]", "", entries_val))
      if (!is.null(period_val)) {
        dates <- strsplit(period_val, " to ")[[1]]
        if (length(dates) >= 2) {
          cm_start <- safe_date(dates[1])
          cm_end <- safe_date(dates[2])
          if (!is.na(cm_start) && !is.na(cm_end)) {
            cm_days <- as.numeric(cm_end - cm_start) + 1
            if (cm_days > 0) {
              cm_cost_day <- cm_cost / cm_days
              cm_tok_day <- cm_tokens / cm_days
            }
          }
        }
      }
  }

  # --- 3. Calculate Gemini stats ---
  gm_cost <- NA; gm_tokens <- NA; gm_entries <- NA; gm_days <- NA; gm_start <- NA; gm_end <- NA
  gm_cost_day <- NA; gm_tok_day <- NA
  gm_sessions_count <- 0
  
  gm_db_path <- here::here("inst/extdata/gemini_usage.duckdb")
  if (file.exists(gm_db_path)) {
    gm_con <- dbConnect(duckdb(), dbdir = gm_db_path, read_only = TRUE)
    gm_stats <- dbGetQuery(gm_con, "
      SELECT 
        SUM(total_cost) as cost, 
        SUM(total_tokens) as tokens, 
        SUM(message_count) as entries,
        MIN(date) as start_date,
        MAX(date) as end_date
      FROM daily_usage
    ")
    
          if (!is.null(gm_stats$cost)) {
            gm_cost <- gm_stats$cost
            gm_tokens <- gm_stats$tokens
            gm_entries <- gm_stats$entries
            gm_start <- safe_date(gm_stats$start_date)
            gm_end <- safe_date(gm_stats$end_date)
            
            if (!is.na(gm_start) && !is.na(gm_end)) {
              gm_days <- as.numeric(gm_end - gm_start) + 1
              if (gm_days > 0) {
                gm_cost_day <- gm_cost / gm_days
                gm_tok_day <- gm_tokens / gm_days
              }
            }
          }    
    # Get recent Gemini sessions
    gm_sessions <- dbGetQuery(gm_con, "
      SELECT sessionId, total_cost, total_tokens, start_time 
      FROM sessions_summary 
      ORDER BY last_updated DESC LIMIT 5
    ")
    gm_sessions_count <- dbGetQuery(gm_con, "SELECT COUNT(*) FROM sessions_summary")[[1]]
    dbDisconnect(gm_con, shutdown = TRUE)
  }

  # Build email - Dark mode with Merged Summary Table
  # Break into chunks to avoid sprintf 100-argument limit
  
  # --- Build Time Block Activity HTML (shown first) ---
  blocks_html <- ""
  daily_model_html <- ""
  if (!is.null(blocks_raw) && !is.null(blocks_raw$blocks)) {
    blocks_df <- as_tibble(blocks_raw$blocks) |>
      mutate(
        start = ymd_hms(startTime),
        end = ymd_hms(actualEndTime),
        duration_mins = as.numeric(difftime(end, start, units = "mins")),
        duration_hrs = duration_mins / 60,
        date = as.Date(start),
        cost_per_hr = ifelse(duration_hrs > 0, costUSD / duration_hrs, 0),
        tokens_per_hr = ifelse(duration_hrs > 0, totalTokens / duration_hrs, 0),
        models_list = map(models, normalize_to_char_vec)
      ) |>
      filter(!is.na(end), costUSD > 0)

    recent_days <- blocks_df |>
      group_by(date) |> summarise(n = n(), .groups = "drop") |>
      arrange(desc(date)) |> head(5) |> pull(date)

    if (length(recent_days) > 0) {
      activity_df <- blocks_df |> filter(date %in% recent_days) |> arrange(desc(end))

      if (nrow(activity_df) > 0) {
        blocks_html <- sprintf('\n<h3 style="color: %s;">Time Block Activity (Last 5 Days)</h3>
<table style="border-collapse: collapse; width: 100%%;">
  <tr style="background-color: %s;">
    <th style="padding: 6px; border: 1px solid %s; font-size: 11px; color: white;">Start</th>
    <th style="padding: 6px; border: 1px solid %s; font-size: 11px; color: white;">End</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Duration</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Cost</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">$/hr</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Tokens</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Tok/hr</th>
  </tr>', accent_orange, "#607D8B",
              dark_border, dark_border, dark_border, dark_border, dark_border, dark_border, dark_border)

        day_summary <- activity_df |>
          group_by(date) |>
          summarise(n_blocks = n(), total_mins = sum(duration_mins, na.rm = TRUE),
            total_cost = sum(costUSD, na.rm = TRUE), total_tokens = sum(totalTokens, na.rm = TRUE),
            .groups = "drop")

        sorted_dates <- sort(unique(activity_df$date), decreasing = TRUE)
        for (di in seq_along(sorted_dates)) {
          d <- sorted_dates[di]
          ds <- day_summary[day_summary$date == d, ]
          d_nblocks <- as.integer(ds$n_blocks[1]); d_mins <- as.numeric(ds$total_mins[1])
          d_cost <- as.numeric(ds$total_cost[1]); d_tokens <- as.numeric(ds$total_tokens[1])
          d_cost_hr <- if (d_mins > 0) d_cost / (d_mins / 60) else 0
          day_label <- format(d, "%a %Y-%m-%d")
          blocks_html <- paste0(blocks_html, sprintf('\n  <tr style="background-color: #1a3a5c; font-weight: bold;">
    <td colspan="2" style="padding: 8px; border: 1px solid %s; font-size: 12px; color: %s;">%s (%d blocks)</td>
    <td style="padding: 8px; border: 1px solid %s; text-align: right; font-size: 12px; color: %s;">%s</td>
    <td style="padding: 8px; border: 1px solid %s; text-align: right; font-size: 12px; color: %s;">%s</td>
    <td style="padding: 8px; border: 1px solid %s; text-align: right; font-size: 12px; color: %s;">%s</td>
    <td style="padding: 8px; border: 1px solid %s; text-align: right; font-size: 12px; color: %s;">%s</td>
    <td style="padding: 8px; border: 1px solid %s;"></td>
  </tr>',
            dark_border, "#ffffff", day_label, d_nblocks,
            dark_border, "#ffffff", format_hhmm(d_mins),
            dark_border, accent_green, dollar(d_cost),
            dark_border, "#ffffff", dollar(d_cost_hr),
            dark_border, accent_blue, comma(d_tokens), dark_border))

          day_blocks <- activity_df |> filter(date == d) |> arrange(desc(end))
          for (i in seq_len(nrow(day_blocks))) {
            bg <- if (i %% 2 == 0) dark_row_alt else dark_card
            blocks_html <- paste0(blocks_html, sprintf('\n  <tr style="background-color: %s;">
    <td style="padding: 4px 6px; border: 1px solid %s; font-size: 10px; color: %s; padding-left: 20px;">%s</td>
    <td style="padding: 4px 6px; border: 1px solid %s; font-size: 10px; color: %s;">%s</td>
    <td style="padding: 4px 6px; border: 1px solid %s; text-align: right; font-size: 10px; color: %s;">%s</td>
    <td style="padding: 4px 6px; border: 1px solid %s; text-align: right; font-size: 10px; color: %s;">%s</td>
    <td style="padding: 4px 6px; border: 1px solid %s; text-align: right; font-size: 10px; color: %s;">%s</td>
    <td style="padding: 4px 6px; border: 1px solid %s; text-align: right; font-size: 10px; color: %s;">%s</td>
    <td style="padding: 4px 6px; border: 1px solid %s; text-align: right; font-size: 10px; color: %s;">%s</td>
  </tr>',
              bg,
              dark_border, dark_muted, format(day_blocks$start[i], "%H:%M"),
              dark_border, dark_muted, format(day_blocks$end[i], "%H:%M"),
              dark_border, dark_text, format_hhmm(day_blocks$duration_mins[i]),
              dark_border, accent_green, dollar(day_blocks$costUSD[i]),
              dark_border, dark_text, dollar(day_blocks$cost_per_hr[i]),
              dark_border, accent_blue, comma(day_blocks$totalTokens[i]),
              dark_border, dark_text, comma(round(day_blocks$tokens_per_hr[i]))))
          }
        }
        blocks_html <- paste0(blocks_html, "</table>",
          sprintf("<!-- QA:blocks_grouped_by_day=%d -->", length(sorted_dates)),
          sprintf("<!-- QA:blocks_total=%d -->", nrow(activity_df)))

        # --- Daily Model Breakdown table ---
        # Read per-model daily data (exported by export_dashboard_data.R from cmonitor-rs model_stats)
        model_daily_path <- here::here("inst", "extdata", "model_daily.json")
        model_df <- NULL
        if (file.exists(model_daily_path)) {
          model_df <- tryCatch(
            fromJSON(model_daily_path) |> as_tibble() |>
              mutate(date = as.Date(date),
                     cost_per_mtok = if_else(tokens > 0, cost / (tokens / 1e6), 0)) |>
              filter(date %in% recent_days),
            error = function(e) NULL)
        }
        if (!is.null(model_df) && nrow(model_df) > 0) {
          model_df$cost_per_mtok[is.nan(model_df$cost_per_mtok)] <- 0

          daily_model_html <- sprintf('\n<h3 style="color: %s; margin-top: 20px;">Daily Cost by Model (Last 5 Days)</h3>
<table style="border-collapse: collapse; width: 100%%;">
  <tr style="background-color: %s;">
    <th style="padding: 6px; border: 1px solid %s; font-size: 11px; color: white;">Day</th>
    <th style="padding: 6px; border: 1px solid %s; font-size: 11px; color: white;">Model</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Cost</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Tokens</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">$/MTok</th>
  </tr>', accent_purple, accent_purple,
            dark_border, dark_border, dark_border, dark_border, dark_border)

          row_idx <- 0
          model_sorted_dates <- sort(unique(model_df$date), decreasing = TRUE)
          for (di2 in seq_along(model_sorted_dates)) {
            d <- model_sorted_dates[di2]
            d_rows <- model_df[model_df$date == d, ] |> arrange(desc(cost))
            d_total_cost <- as.numeric(sum(d_rows$cost))
            d_total_tokens <- as.numeric(sum(d_rows$tokens))
            day_label <- format(d, "%a %Y-%m-%d")
            # Day total row
            daily_model_html <- paste0(daily_model_html, sprintf('\n  <tr style="background-color: #1a3a5c; font-weight: bold;">
    <td style="padding: 8px; border: 1px solid %s; font-size: 12px; color: %s;">%s</td>
    <td style="padding: 8px; border: 1px solid %s; font-size: 12px; color: %s;">ALL</td>
    <td style="padding: 8px; border: 1px solid %s; text-align: right; font-size: 12px; color: %s;">%s</td>
    <td style="padding: 8px; border: 1px solid %s; text-align: right; font-size: 12px; color: %s;">%s</td>
    <td style="padding: 8px; border: 1px solid %s;"></td>
  </tr>',
              dark_border, "#ffffff", day_label,
              dark_border, "#ffffff",
              dark_border, accent_green, dollar(d_total_cost),
              dark_border, accent_blue, comma(d_total_tokens),
              dark_border))
            # Per-model rows
            for (j in seq_len(nrow(d_rows))) {
              row_idx <- row_idx + 1
              bg <- if (row_idx %% 2 == 0) dark_row_alt else dark_card
              m_name <- gsub("claude-", "", as.character(d_rows$model[j]))
              daily_model_html <- paste0(daily_model_html, sprintf('\n  <tr style="background-color: %s;">
    <td style="padding: 4px 6px; border: 1px solid %s; font-size: 10px; color: %s;"></td>
    <td style="padding: 4px 6px; border: 1px solid %s; font-size: 10px; color: %s; padding-left: 20px;">%s</td>
    <td style="padding: 4px 6px; border: 1px solid %s; text-align: right; font-size: 10px; color: %s;">%s</td>
    <td style="padding: 4px 6px; border: 1px solid %s; text-align: right; font-size: 10px; color: %s;">%s</td>
    <td style="padding: 4px 6px; border: 1px solid %s; text-align: right; font-size: 10px; color: %s;">%s</td>
  </tr>',
                bg,
                dark_border, dark_muted,
                dark_border, dark_text, m_name,
                dark_border, accent_green, dollar(as.numeric(d_rows$cost[j])),
                dark_border, accent_blue, comma(as.numeric(d_rows$tokens[j])),
                dark_border, dark_text, dollar(as.numeric(d_rows$cost_per_mtok[j]))))
            }
          }
          daily_model_html <- paste0(daily_model_html, "</table>",
            sprintf("<!-- QA:model_breakdown_days=%d -->", length(model_sorted_dates)),
            sprintf("<!-- QA:models_found=%s -->",
              paste(gsub("claude-", "", unique(model_df$model)), collapse = ",")))
        } else {
          # No model data — emit empty QA markers so QA doesn't fail
          daily_model_html <- paste0(
            "<!-- QA:model_breakdown_days=0 -->",
            "<!-- QA:models_found=none -->")
        }
      }
    }
  }
  # If blocks section was never built, still emit model markers
  if (nchar(daily_model_html) == 0) {
    daily_model_html <- paste0(
      "<!-- QA:model_breakdown_days=0 -->",
      "<!-- QA:models_found=none -->")
  }

  # --- Build Top Projects by Cost ---
  projects_html <- ""
  if (!is.null(daily_data) && nrow(daily_data) > 0 && "project" %in% names(daily_data)) {
    proj_totals <- daily_data |>
      group_by(project) |>
      summarise(cost = sum(totalCost, na.rm = TRUE),
                tokens = sum(totalTokens, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(cost)) |>
      head(10)
    if (nrow(proj_totals) > 0) {
      projects_html <- sprintf('\n<h3 style="color: %s; margin-top: 20px;">Top Projects by Cost</h3>
<table style="border-collapse: collapse; width: 100%%;">
  <tr style="background-color: %s;">
    <th style="padding: 6px; border: 1px solid %s; font-size: 11px; color: white;">Project</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Total Cost</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Tokens</th>
  </tr>', accent_orange, accent_orange, dark_border, dark_border, dark_border)
      for (i in seq_len(nrow(proj_totals))) {
        bg <- if (i %% 2 == 0) dark_row_alt else dark_card
        projects_html <- paste0(projects_html, sprintf('\n  <tr style="background-color: %s;">
    <td style="padding: 6px; border: 1px solid %s; font-size: 11px; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: %s;">%s</td>
  </tr>', bg,
          dark_border, dark_text, as.character(proj_totals$project[i]),
          dark_border, accent_green, dollar(as.numeric(proj_totals$cost[i])),
          dark_border, accent_blue, comma(as.numeric(proj_totals$tokens[i]))))
      }
      projects_html <- paste0(projects_html, "</table>")
    }
  }

  # --- Build Top Claude Sessions by Cost ---
  sessions_html <- ""
  if (!is.null(session_data) && nrow(session_data) > 0) {
    top_sessions <- session_data |> arrange(desc(totalCost)) |> head(5)
    sessions_html <- sprintf('\n<h3 style="color: %s; margin-top: 20px;">Top Claude Sessions by Cost</h3>
<table style="border-collapse: collapse; width: 100%%;">
  <tr style="background-color: %s;">
    <th style="padding: 6px; border: 1px solid %s; font-size: 11px; color: white;">Session</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Cost</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Tokens</th>
    <th style="padding: 6px; border: 1px solid %s; font-size: 11px; color: white;">Last Active</th>
  </tr>', accent_purple, accent_purple, dark_border, dark_border, dark_border, dark_border)
    for (i in seq_len(nrow(top_sessions))) {
      bg <- if (i %% 2 == 0) dark_row_alt else dark_card
      session_name <- top_sessions$sessionId[i]
      session_parts <- strsplit(gsub("^-", "", session_name), "-")[[1]]
      if (length(session_parts) > 2) session_name <- paste(tail(session_parts, 2), collapse = "/")
      sessions_html <- paste0(sessions_html, sprintf('\n  <tr style="background-color: %s;">
    <td style="padding: 6px; border: 1px solid %s; font-size: 11px; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; font-size: 11px; color: %s;">%s</td>
  </tr>', bg,
        dark_border, dark_text, session_name,
        dark_border, accent_green, dollar(top_sessions$totalCost[i]),
        dark_border, accent_blue, millions(top_sessions$totalTokens[i]),
        dark_border, dark_muted, top_sessions$lastActivity[i]))
    }
    sessions_html <- paste0(sessions_html, "</table>")
  }

  email_header <- sprintf('\n<div style="background-color: %s; color: %s; padding: 20px; font-family: -apple-system, BlinkMacSystemFont, sans-serif;">
<h2 style="color: %s; margin-bottom: 5px;">LLM Usage Report - %s</h2>
<p style="color: %s; font-size: 12px; margin-top: 0;">Data cached: %s</p>

<!-- Embedded Dashboard Link -->
<div style="margin-bottom: 20px;">
  <a href="https://johngavin.github.io/llmtelemetry/" style="display: inline-block; padding: 10px 20px; background-color: %s; color: #1a1a2e; text-decoration: none; border-radius: 5px; font-weight: bold;">
    View Full Telemetry Dashboard
  </a>
</div>

%s
%s
%s
%s

<h3 style="color: %s;">Summary</h3>
<table style="border-collapse: collapse; width: 100%%; font-size: 11px;">
  <tr style="background-color: %s; color: white;">
    <th style="padding: 6px; border: 1px solid %s;">Source</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right;">Cost<sup>1</sup></th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right;">$/Day</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right;">Tokens</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right;">Tok/Day</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right;">Days<sup>2</sup></th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right;">Sessions<sup>3</sup></th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right;">Entries<sup>4</sup></th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right;">Start Date</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right;">End Date</th>
  </tr>',
  dark_bg, dark_text, accent_orange, today, dark_muted, cache_time, accent_blue,
  blocks_html, daily_model_html, projects_html, sessions_html,
  accent_green,
  dark_row_alt, dark_border, dark_border, dark_border, dark_border, dark_border, dark_border, dark_border, dark_border, dark_border, dark_border)

  row_ccusage <- sprintf('
  <!-- ccusage Row -->
  <tr style="background-color: %s;">
    <td style="padding: 6px; border: 1px solid %s; color: %s;"><strong>ccusage</strong></td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
  </tr>',
  dark_card, dark_border, dark_text,
  dark_border, accent_green, dollar(total_cost),
  dark_border, dark_text, dollar(cc_cost_day),
  dark_border, accent_blue, millions(total_tokens),
  dark_border, dark_text, millions(cc_tok_day),
  dark_border, dark_text, cc_days,
  dark_border, dark_text, n_sessions,
  dark_border, dark_text, ifelse(is.na(cc_entries), "-", comma(cc_entries)),
  dark_border, dark_muted, as.character(cc_start),
  dark_border, dark_muted, as.character(cc_end))

  row_gemini <- sprintf('
  <!-- Gemini Row -->
  <tr style="background-color: %s;">
    <td style="padding: 6px; border: 1px solid %s; color: %s;"><strong>Gemini</strong></td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
  </tr>',
  dark_row_alt, dark_border, dark_text,
  dark_border, accent_green, dollar(gm_cost),
  dark_border, dark_text, dollar(gm_cost_day),
  dark_border, accent_blue, millions(gm_tokens),
  dark_border, dark_text, millions(gm_tok_day),
  dark_border, dark_text, gm_days,
  dark_border, dark_text, ifelse(gm_sessions_count == 0, "-", gm_sessions_count),
  dark_border, dark_text, comma(gm_entries),
  dark_border, dark_muted, as.character(gm_start),
  dark_border, dark_muted, as.character(gm_end))

  row_cmonitor <- sprintf('
  <!-- cmonitor Row -->
  <tr style="background-color: %s;">
    <td style="padding: 6px; border: 1px solid %s; color: %s;"><strong>cmonitor</strong></td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">-</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
  </tr>',
  dark_card, dark_border, dark_text,
  dark_border, accent_green, dollar(cm_cost),
  dark_border, dark_text, dollar(cm_cost_day),
  dark_border, accent_blue, millions(cm_tokens),
  dark_border, dark_text, millions(cm_tok_day),
  dark_border, dark_text, cm_days,
  dark_border, dark_text, # Sessions (2 placeholders for style, value is hardcoded '-')
  dark_border, dark_text, comma(cm_entries),
  dark_border, dark_muted, as.character(cm_start),
  dark_border, dark_muted, as.character(cm_end))

  # --- Codex row for summary table ---
  row_codex <- ""
  if (has_codex) {
    cx_total_cost    <- sum(codex_d$est_cost_usd, na.rm = TRUE)
    cx_total_tokens  <- sum(codex_d$total_tokens, na.rm = TRUE)
    cx_sessions      <- if (!is.null(codex_s) && nrow(codex_s) > 0 &&
                            "thread_id" %in% names(codex_s)) {
      length(unique(codex_s$thread_id))
    } else nrow(codex_d)
    cx_start <- if ("date" %in% names(codex_d)) min(safe_date(codex_d$date), na.rm = TRUE) else NA
    cx_end   <- if ("date" %in% names(codex_d)) max(safe_date(codex_d$date), na.rm = TRUE) else NA
    cx_days  <- if (!is.na(cx_start) && !is.na(cx_end)) as.numeric(cx_end - cx_start) + 1 else 0
    cx_cost_day <- if (cx_days > 0) cx_total_cost / cx_days else NA
    cx_tok_day  <- if (cx_days > 0) cx_total_tokens / cx_days else NA

    row_codex <- sprintf('
  <!-- Codex Row -->
  <tr style="background-color: %s;">
    <td style="padding: 6px; border: 1px solid %s; color: %s;"><strong>Codex</strong></td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s <em style="font-size:9px; color:%s;">(est)</em></td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">-</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
  </tr>',
      dark_row_alt, dark_border, dark_text,
      dark_border, accent_green, dollar(cx_total_cost), dark_muted,
      dark_border, dark_text, dollar(cx_cost_day),
      dark_border, accent_blue, millions(cx_total_tokens),
      dark_border, dark_text, millions(cx_tok_day),
      dark_border, dark_text, cx_days,
      dark_border, dark_text, cx_sessions,
      dark_border, dark_text,
      dark_border, dark_muted, as.character(cx_start),
      dark_border, dark_muted, as.character(cx_end))
  }

  # --- Codex by automation type section ---
  codex_type_html <- ""
  if (has_codex && "source" %in% names(codex_d)) {
    cx_by_type <- codex_d |>
      group_by(source) |>
      summarise(
        sessions  = n(),
        tokens    = sum(total_tokens, na.rm = TRUE),
        est_cost  = sum(est_cost_usd, na.rm = TRUE),
        .groups   = "drop"
      ) |>
      arrange(desc(est_cost))

    codex_type_html <- sprintf(
      '\n<h3 style="color: %s; margin-top: 20px;">Codex by Automation Type</h3>
<table style="border-collapse: collapse; width: 100%%;">
  <tr style="background-color: %s;">
    <th style="padding: 6px; border: 1px solid %s; font-size: 11px; color: white;">Type</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Sessions</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Total Tokens</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Est. Cost</th>
  </tr>',
      accent_orange, accent_orange,
      dark_border, dark_border, dark_border, dark_border)

    for (i in seq_len(nrow(cx_by_type))) {
      bg <- if (i %% 2 == 0) dark_row_alt else dark_card
      type_label <- switch(as.character(cx_by_type$source[i]),
        "roborev"     = "Roborev jobs",
        "interactive" = "Interactive",
        as.character(cx_by_type$source[i]))
      codex_type_html <- paste0(codex_type_html, sprintf(
        '\n  <tr style="background-color: %s;">
    <td style="padding: 6px; border: 1px solid %s; font-size: 11px; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: %s;">%s <em style="font-size:9px;">(est)</em></td>
  </tr>',
        bg,
        dark_border, dark_text, type_label,
        dark_border, dark_text, comma(cx_by_type$sessions[i]),
        dark_border, accent_blue, comma(cx_by_type$tokens[i]),
        dark_border, accent_green, dollar(cx_by_type$est_cost[i])))
    }
    codex_type_html <- paste0(codex_type_html, "\n</table>",
      "<!-- QA:codex_type_rows=", nrow(cx_by_type), " -->")

    # Stale-pricing warning
    pricing_path <- here::here("inst/extdata/codex_pricing.json")
    if (file.exists(pricing_path)) {
      pricing_meta <- tryCatch(
        jsonlite::fromJSON(pricing_path),
        error = function(e) NULL
      )
      if (!is.null(pricing_meta) && !is.null(pricing_meta$updated_at)) {
        pricing_date <- tryCatch(as.Date(pricing_meta$updated_at), error = function(e) NA)
        if (!is.na(pricing_date) &&
            as.numeric(Sys.Date() - pricing_date) > 30) {
          codex_type_html <- paste0(codex_type_html, sprintf(
            '\n<p style="color: %s; font-size: 10px; font-style: italic; margin-top: 6px;">
  Codex cost estimates use pricing from %s; verify
  <a href="https://openai.com/api/pricing" style="color: %s;">openai.com/api/pricing</a>
  if stale.
</p>',
            dark_muted,
            format(pricing_date, "%Y-%m-%d"),
            accent_blue))
        }
      }
    }
  }

  email_body <- paste0(email_header, row_ccusage, row_gemini, row_cmonitor,
                       row_codex, "\n</table>", codex_type_html)

  # Weekly Cost
  email_body <- paste0(email_body, sprintf('\n<h3 style="color: %s;">Weekly Cost (Claude)</h3>
<table style="border-collapse: collapse; max-width: 400px;">
  <tr style="background-color: %s;">
    <th style="padding: 8px; border: 1px solid %s; color: white;">Period</th>
    <th style="padding: 8px; border: 1px solid %s; text-align: right; color: white;">Cost</th>
  </tr>
  <tr style="background-color: %s;">
    <td style="padding: 8px; border: 1px solid %s; color: %s;">Week 1 (current)</td>
    <td style="padding: 8px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
  </tr>
  <tr style="background-color: %s;">
    <td style="padding: 8px; border: 1px solid %s; color: %s;">Week 2</td>
    <td style="padding: 8px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
  </tr>
  <tr style="background-color: %s;">
    <td style="padding: 8px; border: 1px solid %s; color: %s;">Week 3</td>
    <td style="padding: 8px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
  </tr>
  <tr style="background-color: %s;">
    <td style="padding: 8px; border: 1px solid %s; color: %s;">Week 4</td>
    <td style="padding: 8px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
  </tr>
</table>
',
     accent_green, accent_green, dark_border, dark_border,
     dark_card, dark_border, dark_text, dark_border, accent_green, dollar(week1$cost),
     dark_row_alt, dark_border, dark_text, dark_border, dark_text, dollar(week2$cost),
     dark_card, dark_border, dark_text, dark_border, dark_text, dollar(week3$cost),
     dark_row_alt, dark_border, dark_text, dark_border, dark_text, dollar(week4$cost)))

  # Weekly Tokens
  email_body <- paste0(email_body, sprintf('\n<h3 style="color: %s;">Weekly Tokens (Claude)</h3>
<table style="border-collapse: collapse; max-width: 400px;">
  <tr style="background-color: %s;">
    <th style="padding: 8px; border: 1px solid %s; color: white;">Period</th>
    <th style="padding: 8px; border: 1px solid %s; text-align: right; color: white;">Tokens</th>
  </tr>
  <tr style="background-color: %s;">
    <td style="padding: 8px; border: 1px solid %s; color: %s;">Week 1 (current)</td>
    <td style="padding: 8px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
  </tr>
  <tr style="background-color: %s;">
    <td style="padding: 8px; border: 1px solid %s; color: %s;">Week 2</td>
    <td style="padding: 8px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
  </tr>
  <tr style="background-color: %s;">
    <td style="padding: 8px; border: 1px solid %s; color: %s;">Week 3</td>
    <td style="padding: 8px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
  </tr>
  <tr style="background-color: %s;">
    <td style="padding: 8px; border: 1px solid %s; color: %s;">Week 4</td>
    <td style="padding: 8px; border: 1px solid %s; text-align: right; color: %s;">%s</td>
  </tr>
</table>
',
     accent_blue, accent_blue, dark_border, dark_border,
     dark_card, dark_border, dark_text, dark_border, accent_blue, millions(week1$tokens),
     dark_row_alt, dark_border, dark_text, dark_border, dark_text, millions(week2$tokens),
     dark_card, dark_border, dark_text, dark_border, dark_text, millions(week3$tokens),
     dark_row_alt, dark_border, dark_text, dark_border, dark_text, millions(week4$tokens)))

  # (Time Block Activity moved to top — see blocks_html above)

  # Gemini Recent Sessions
  if (exists("gm_sessions") && !is.null(gm_sessions) && nrow(gm_sessions) > 0) {
    email_body <- paste0(email_body, sprintf('\n<h3 style="color: %s; margin-top: 20px;">Gemini Recent Sessions</h3>
<table style="border-collapse: collapse; width: 100%%;">
  <tr style="background-color: %s;">
    <th style="padding: 6px; border: 1px solid %s; font-size: 11px; color: white;">Session ID</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Cost</th>
    <th style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: white;">Tokens</th>
    <th style="padding: 6px; border: 1px solid %s; font-size: 11px; color: white;">Started</th>
  </tr>', accent_blue, accent_blue, dark_border, dark_border, dark_border, dark_border))

    for (i in seq_len(nrow(gm_sessions))) {
      bg <- if (i %% 2 == 0) dark_row_alt else dark_card
      email_body <- paste0(email_body, sprintf('\n  <tr style="background-color: %s;">
    <td style="padding: 6px; border: 1px solid %s; font-size: 11px; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; text-align: right; font-size: 11px; color: %s;">%s</td>
    <td style="padding: 6px; border: 1px solid %s; font-size: 11px; color: %s;">%s</td>
  </tr>', 
        bg,
        dark_border, dark_text, gm_sessions$sessionId[i],
        dark_border, accent_green, dollar(gm_sessions$total_cost[i]),
        dark_border, accent_blue, millions(gm_sessions$total_tokens[i]),
        dark_border, dark_muted, format(gm_sessions$start_time[i], "%Y-%m-%d %H:%M")
      ))
    }
    email_body <- paste0(email_body, "</table>")
  }

  # (Top Claude Sessions moved to top — see sessions_html/projects_html below)

  email_body <- paste0(email_body, sprintf('\n<hr style="margin-top: 20px; border-color: %s;">
<p style="color: %s; font-size: 12px;">
  <a href="https://github.com/JohnGavin/llmtelemetry" style="color: %s;">llmtelemetry project</a> |
  <a href="https://johngavin.github.io/llmtelemetry/" style="color: %s;">Dashboard</a> |
  Refresh: <code style="background-color: %s; padding: 2px 6px; border-radius: 3px; color: %s;">bash exec/refresh_and_preserve.sh</code>
</p>

<!-- Footnotes -->
<div style="margin-top: 30px; border-top: 1px solid %s; padding-top: 10px; color: %s; font-size: 10px;">
  <strong>Definitions:</strong><br>
  <sup>1</sup> <strong>Cost:</strong> Total cost in USD. Codex figures are estimates from <code>codex_pricing.json</code> and marked <em>(est)</em>.<br>
  <sup>2</sup> <strong>Days:</strong> Number of days in the reporting period.<br>
  <sup>3</sup> <strong>Sessions:</strong> Number of distinct interactive sessions recorded.<br>
  <sup>4</sup> <strong>Entries:</strong> Total number of logged interactions/blocks.<br>
  <strong>Source:</strong> <em>ccusage/Gemini</em> (this R package) vs <em>cmonitor</em> (Rust-based system monitor) vs <em>Codex</em> (OpenAI Codex OTEL logs).
</div>
</div>
', dark_border, dark_muted, accent_blue, accent_blue, dark_card, dark_text, dark_border, dark_muted))
}

# --- QA: Validate email before sending ---
# Save HTML for CI QA step (always, even if checks pass)
writeLines(email_body, "/tmp/email_qa.html")
message("Email HTML saved to /tmp/email_qa.html for QA")

qa_errors <- character(0)

# Negative assertions: error patterns that should NOT appear
neg_patterns <- c("Error in", "Error:", "invalid 'trim'", "prettyNum")
for (pat in neg_patterns) {
  if (grepl(pat, email_body, ignore.case = TRUE)) {
    qa_errors <- c(qa_errors, sprintf("Error pattern found: '%s'", pat))
  }
}
# NaN/NULL in visible content (not inside HTML comments)
# Note: use fixed=TRUE or perl word-boundary to avoid false positives from
# words like "finance" matching case-insensitive "NaN".
visible <- gsub("<!--.*?-->", "", email_body)
for (pat in c("NaN", ">NULL<", "NA_real_")) {
  # Case-sensitive exact match only (NaN is a specific R token)
  if (grepl(pat, visible, fixed = TRUE)) {
    qa_errors <- c(qa_errors, sprintf("Bad value in visible content: '%s'", pat))
  }
}

# Positive assertions: structural features that MUST appear
pos_checks <- list(
  "Time Block Activity heading" = grepl("Time Block Activity", email_body),
  "Daily Cost by Model heading" = grepl("Daily Cost by Model", email_body),
  "Summary heading" = grepl("Summary", email_body),
  "Day group headers (bold rows)" = grepl("font-weight: bold", email_body),
  "$/MTok column" = grepl("MTok", email_body),
  "Dashboard link" = grepl("johngavin.github.io/llmtelemetry", email_body),
  "Cost values present" = grepl("\\$[0-9]+\\.[0-9]{2}", email_body)
)
for (nm in names(pos_checks)) {
  if (!isTRUE(pos_checks[[nm]])) {
    qa_errors <- c(qa_errors, sprintf("Missing feature: %s", nm))
  }
}

# Ordering: blocks must appear before summary
blocks_pos <- regexpr("Time Block Activity", email_body)
summary_pos <- regexpr(">Summary<", email_body)
if (blocks_pos > 0 && summary_pos > 0 && blocks_pos > summary_pos) {
  qa_errors <- c(qa_errors, "Time Block Activity appears AFTER Summary")
}

# QA markers must exist
for (marker in c("QA:blocks_grouped_by_day=", "QA:model_breakdown_days=", "QA:models_found=")) {
  if (!grepl(marker, email_body, fixed = TRUE)) {
    qa_errors <- c(qa_errors, sprintf("Missing QA marker: %s", marker))
  }
}

# Codex QA: if codex data was loaded, the type-breakdown marker and row must appear
if (has_codex) {
  if (!grepl("QA:codex_type_rows=", email_body, fixed = TRUE)) {
    qa_errors <- c(qa_errors, "Missing QA marker: QA:codex_type_rows= (codex data present but marker missing)")
  }
  if (!grepl("Codex by Automation Type", email_body)) {
    qa_errors <- c(qa_errors, "Missing section: 'Codex by Automation Type'")
  }
  if (!grepl("(est)", email_body, fixed = TRUE)) {
    qa_errors <- c(qa_errors, "Missing '(est)' suffix on Codex cost figures")
  }
}

if (length(qa_errors) > 0) {
  cli::cli_abort(c(
    "!" = "Email QA failed with {length(qa_errors)} issue(s):",
    set_names(qa_errors, rep("x", length(qa_errors))),
    "i" = "Debug HTML saved to /tmp/email_qa.html"
  ))
}
message("Email QA passed: all checks OK")

# Create and send email
london_time <- format(Sys.time(), tz = "Europe/London", "%Y-%m-%d %H:%M")

gmail_user <- Sys.getenv("GMAIL_USERNAME")
gmail_pass <- Sys.getenv("GMAIL_APP_PASSWORD")

if (gmail_user == "" || gmail_pass == "") {
  message("WARNING: GMAIL_USERNAME or GMAIL_APP_PASSWORD not set. Skipping email.")
  # Print the email body to stdout for debugging/logging
  cat("\n--- Generated Email Body ---\n")
  cat(email_body)
  cat("\n----------------------------\n")
  quit(status = 0)
}

email <- compose_email(
  body = md(email_body),
  footer = md(sprintf("<span style='color: %s;'>Report generated: %s (London)</span>", dark_muted, london_time))
)

smtp_creds <- creds_envvar(
  user = gmail_user,
  pass_envvar = "GMAIL_APP_PASSWORD",
  host = "smtp.gmail.com",
  port = 465,
  use_ssl = TRUE
)

tryCatch({
  smtp_send(
    email = email,
    to = gmail_user,
    from = gmail_user,
    subject = sprintf("LLM Telemetry Report - %s", today),
    credentials = smtp_creds
  )
  message("Email sent successfully!")
}, error = function(e) {
  message("ERROR: Failed to send email via SMTP.")
  message("Details: ", e$message)
  message("HINT: Check GMAIL_APP_PASSWORD in GitHub Secrets. It may be expired or invalid.")
  
  # Print the email body to stdout so the report is still accessible in CI logs
  cat("\n--- Generated Email Body (Fallback) ---\n")
  cat(email_body)
  cat("\n---------------------------------------\n")
  
  # Do not fail the CI job, just warn
  quit(status = 0)
})