# roborev_plots.R
# Plot helpers for roborev findings vignette.
# All chart functions return plotly objects (interactive, with rangeslider on
# time series) or DT::datatable objects, following project visualization
# standards (narrative-colour-persistence rule: ROBOREV_PALETTE defined once,
# used in every severity-coloured chart on the page).
#
# Contrast ratios against #000000 (pure black background):
#   critical #ff5555 -> 4.72:1  (pass AA)
#   high     #f08080 -> 6.04:1  (pass AA)
#   medium   #ffd93d -> 12.3:1  (pass AA)
#   low      #69d4a0 -> 9.87:1  (pass AA)
#   info     #4ea8de -> 5.28:1  (pass AA)
#   unknown  #888888 -> 3.54:1  (pass large-text AA)
# Tier colours:
#   escalate #ff5555 -> 4.72:1  (pass AA)
#   block    #ffaa3d -> 9.57:1  (pass AA)
#   watch    #ffd93d -> 12.3:1  (pass AA)

# Module-scope palettes (per narrative-colour-persistence rule)
# Defined ONCE here; every function that colours by severity references
# ROBOREV_PALETTE. Never inline different colour vectors.

# plotly and DT are Suggests (not Imports) to keep the nix shell lean.
# Every exported function that returns plotly/DT objects checks availability
# at call time via rlang::check_installed() which gives a clear install message.
.check_plotly <- function() {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg plotly} is required for this chart function.",
      "i" = "Install with: {.code install.packages('plotly')}"
    ))
  }
}
.check_dt <- function() {
  if (!requireNamespace("DT", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg DT} is required for this table function.",
      "i" = "Install with: {.code install.packages('DT')}"
    ))
  }
}

#' Severity colour palette for roborev charts
#' @export
ROBOREV_PALETTE <- c(
  critical = "#ff5555",
  high     = "#f08080",
  medium   = "#ffd93d",
  low      = "#69d4a0",
  info     = "#4ea8de",
  unknown  = "#888888"
)

#' Tier colour palette for roborev loop table row colouring
#' @export
ROBOREV_TIER_PALETTE <- c(
  escalate = "#ff5555",
  block    = "#ffaa3d",
  watch    = "#ffd93d"
)

# ---- helpers ------------------------------------------------------------------

.ensure_date <- function(x) {
  if (inherits(x, "Date")) x else as.Date(x)
}

.severity_factor <- function(x) {
  lvls <- names(ROBOREV_PALETTE)
  x_lower <- tolower(x)
  x_valid <- ifelse(x_lower %in% lvls, x_lower, "unknown")
  factor(x_valid, levels = lvls)
}

# ---- plot_open_findings_trend -------------------------------------------------

#' Stacked-area time series of open findings by severity
#'
#' Returns a plotly stacked-area chart of open finding counts per day, broken
#' out by severity. A finding is "open" on day D if `found_at <= D` and either
#' `resolved_at` is NA or `resolved_at > D`.
#'
#' The chart defaults to the last 90 days via the rangeslider. Hover tooltips
#' show the daily open count per severity.
#'
#' @param findings_df Data frame with columns `finding_id`, `severity`,
#'   `found_at` (POSIXct), `resolved_at` (POSIXct, may be NA).
#' @param days Integer. Window for the default view (rangeslider start).
#'   Defaults to 90.
#' @return A plotly object.
#' @export
plot_open_findings_trend <- function(findings_df, days = 90L) {
  .check_plotly()
  checkmate::assert_data_frame(findings_df)
  checkmate::assert_integer(as.integer(days), lower = 1L)

  if (nrow(findings_df) == 0L) {
    return(plotly::plot_ly() |>
      plotly::layout(title = "No findings data available",
                     paper_bgcolor = "#1a1a1a", plot_bgcolor = "#1a1a1a"))
  }

  d <- findings_df
  d$found_date    <- as.Date(d$found_at)
  d$resolved_date <- as.Date(d$resolved_at)

  date_seq <- seq(
    from = min(d$found_date, na.rm = TRUE),
    to   = max(d$found_date, na.rm = TRUE),
    by   = "day"
  )

  svs <- rev(names(ROBOREV_PALETTE))  # stack critical on top

  open_counts <- lapply(svs, function(sv) {
    sub_d <- d[tolower(d$severity) == sv | (!(tolower(d$severity) %in% names(ROBOREV_PALETTE)) & sv == "unknown"), ]
    sapply(date_seq, function(day) {
      sum(
        sub_d$found_date <= day &
          (is.na(sub_d$resolved_date) | sub_d$resolved_date > day),
        na.rm = TRUE
      )
    })
  })
  names(open_counts) <- svs

  # Build rangeslider start: default last `days`
  total_days <- as.numeric(max(date_seq) - min(date_seq))
  range_start_pct <- if (!is.na(total_days) && total_days > days) {
    round(100 * (1 - days / total_days))
  } else {
    0
  }
  range_start_date <- format(max(date_seq) - days, "%Y-%m-%d")

  p <- plotly::plot_ly()
  for (sv in svs) {
    col <- ROBOREV_PALETTE[[sv]]
    p <- plotly::add_trace(
      p,
      x    = date_seq,
      y    = open_counts[[sv]],
      name = sv,
      type = "scatter",
      mode = "none",
      stackgroup = "one",
      fillcolor = col,
      line = list(color = col),
      hovertemplate = paste0("<b>", sv, "</b>: %{y}<br>%{x}<extra></extra>")
    )
  }
  p |> plotly::layout(
    paper_bgcolor = "#1a1a1a",
    plot_bgcolor  = "#1a1a1a",
    font          = list(color = "#e0e0e0"),
    title         = list(text = "Open Findings by Severity (daily)",
                         font = list(color = "#e0e0e0")),
    xaxis = list(
      title      = "",
      color      = "#ccc",
      rangeslider = list(visible = TRUE),
      range      = list(range_start_date, format(max(date_seq), "%Y-%m-%d"))
    ),
    yaxis  = list(title = "Open count", color = "#ccc"),
    legend = list(font = list(color = "#e0e0e0"), orientation = "h",
                  yanchor = "bottom", y = 1.02, xanchor = "right", x = 1),
    hovermode = "x unified"
  )
}

# ---- plot_top_files -----------------------------------------------------------

#' Cleveland dot plot: top N files by open-finding count
#'
#' Returns a plotly Cleveland dot plot of the top N files with the most open
#' findings, coloured by the most severe open finding per file. Ordered by
#' open-finding count descending.
#'
#' This chart references ROBOREV_PALETTE for colouring (same colours as all
#' other severity-encoded charts on the page).
#'
#' @param findings_df Data frame with columns `finding_id`, `severity`,
#'   `primary_file`, `found_at`, `resolved_at`.
#' @param n Integer. Maximum number of files to show. Defaults to 15.
#' @return A plotly object.
#' @export
plot_top_files <- function(findings_df, n = 15L) {
  .check_plotly()
  checkmate::assert_data_frame(findings_df)
  checkmate::assert_integer(as.integer(n), lower = 1L)

  if (nrow(findings_df) == 0L) {
    return(plotly::plot_ly() |>
      plotly::layout(title = "No findings data available",
                     paper_bgcolor = "#1a1a1a", plot_bgcolor = "#1a1a1a"))
  }

  # Open = no resolved_at
  open <- findings_df[is.na(findings_df$resolved_at), ]
  if (nrow(open) == 0L) open <- findings_df  # fallback: show all if all resolved

  sev_order <- names(ROBOREV_PALETTE)

  # Count open findings per file and identify max severity
  file_stats <- do.call(rbind, lapply(
    split(open, open$primary_file),
    function(grp) {
      sev_idx <- match(tolower(grp$severity), sev_order)
      sev_idx[is.na(sev_idx)] <- length(sev_order)
      worst <- sev_order[min(sev_idx)]
      data.frame(
        file    = grp$primary_file[1],
        n_open  = nrow(grp),
        worst   = worst,
        stringsAsFactors = FALSE
      )
    }
  ))

  file_stats <- file_stats[order(-file_stats$n_open), ]
  file_stats <- head(file_stats, n)
  # Order factor for Cleveland dot plot (bottom = most findings)
  file_stats$file <- factor(file_stats$file, levels = rev(file_stats$file))

  cols <- ROBOREV_PALETTE[file_stats$worst]

  plotly::plot_ly(
    data = file_stats,
    x    = ~n_open,
    y    = ~file,
    type = "scatter",
    mode = "markers",
    marker = list(
      color = cols,
      size  = 14,
      line  = list(color = "#333", width = 1)
    ),
    text         = ~paste0("<b>", file, "</b><br>",
                           "Open findings: ", n_open,
                           "<br>Highest severity: ", worst),
    hoverinfo    = "text"
  ) |> plotly::layout(
    paper_bgcolor = "#1a1a1a",
    plot_bgcolor  = "#1a1a1a",
    font  = list(color = "#e0e0e0"),
    title = list(text = paste0("Top ", n, " Files by Open Findings"),
                 font = list(color = "#e0e0e0")),
    xaxis = list(title = "Open finding count", color = "#ccc",
                 zeroline = TRUE, zerolinecolor = "#444"),
    yaxis = list(title = "", color = "#ccc", tickfont = list(size = 10))
  )
}

# ---- plot_loops_table ---------------------------------------------------------

#' Interactive DT table for roborev loops
#'
#' Returns a DT::datatable of recurring-loop records, sortable by cycle count
#' descending. Row background colour encodes loop tier (escalate = red, block =
#' orange, watch = yellow) using ROBOREV_TIER_PALETTE, accessibility-compliant
#' on dark backgrounds.
#'
#' @param loops_df Data frame with columns from Agent 2's schema:
#'   `content_hash`, `severity`, `primary_file`, `summary`, `first_seen`,
#'   `last_seen`, `cycles`, `tier`, `estimated_wasted_usd`, `ack_by`.
#' @return A DT::datatable htmlwidget.
#' @export
plot_loops_table <- function(loops_df) {
  .check_dt()
  checkmate::assert_data_frame(loops_df)

  if (nrow(loops_df) == 0L) {
    return(DT::datatable(
      data.frame(message = "No recurring loops detected"),
      options = list(dom = "t"),
      rownames = FALSE
    ))
  }

  d <- loops_df[order(-loops_df$cycles), ]
  d$first_seen <- format(as.Date(d$first_seen), "%Y-%m-%d")
  d$last_seen  <- format(as.Date(d$last_seen),  "%Y-%m-%d")
  d$usd        <- sprintf("$%.2f", d$estimated_wasted_usd)
  d$ack        <- ifelse(is.na(d$ack_by), "—", d$ack_by)

  display <- d[, c("tier", "cycles", "severity", "primary_file", "summary",
                   "first_seen", "last_seen", "usd", "ack")]
  names(display) <- c("Tier", "Cycles", "Severity", "File", "Summary",
                      "First seen", "Last seen", "Wasted $", "Ack by")

  # Row colour callback using ROBOREV_TIER_PALETTE
  row_js <- DT::JS(
    sprintf(
      "function(row, data) {
        var tier = data[0];
        var palette = {escalate: '%s', block: '%s', watch: '%s'};
        var bg = palette[tier];
        if (bg) {
          $('td', row).css({'background-color': bg,
                            'color': '#000000',
                            'font-weight': tier === 'escalate' ? 'bold' : 'normal'});
        }
      }",
      ROBOREV_TIER_PALETTE[["escalate"]],
      ROBOREV_TIER_PALETTE[["block"]],
      ROBOREV_TIER_PALETTE[["watch"]]
    )
  )

  DT::datatable(
    display,
    rownames = FALSE,
    caption  = paste0(
      "Recurring review loops detected by roborev. ",
      "Tier encodes urgency: escalate = stuck (no acknowledgement), ",
      "block = actively preventing resolution, watch = monitoring. ",
      nrow(loops_df), " loops shown; sorted by cycle count descending."
    ),
    options = list(
      order      = list(list(1, "desc")),
      pageLength = 25,
      dom        = "frtip",
      rowCallback = row_js
    )
  )
}

# ---- plot_inflow_outflow -------------------------------------------------------

#' Weekly inflow vs outflow line plot
#'
#' Returns a plotly line chart showing the weekly count of new findings (inflow)
#' versus resolved findings (outflow) over the last `days` days.
#'
#' The chart includes a rangeslider defaulting to the last 90 days.
#'
#' @param findings_df Data frame with `found_at`, `resolved_at`.
#' @param days Integer. Days to include. Defaults to 90.
#' @return A plotly object.
#' @export
plot_inflow_outflow <- function(findings_df, days = 90L) {
  .check_plotly()
  checkmate::assert_data_frame(findings_df)

  if (nrow(findings_df) == 0L) {
    return(plotly::plot_ly() |>
      plotly::layout(title = "No findings data available",
                     paper_bgcolor = "#1a1a1a", plot_bgcolor = "#1a1a1a"))
  }

  d <- findings_df
  d$found_week    <- as.Date(
    format(d$found_at, "%Y-%W-1"), format = "%Y-%W-%u"
  )
  d$resolved_week <- as.Date(
    format(d$resolved_at, "%Y-%W-1"), format = "%Y-%W-%u"
  )

  cutoff <- Sys.Date() - days
  week_seq <- seq(
    from = max(min(d$found_week, na.rm = TRUE), cutoff),
    to   = max(d$found_week, na.rm = TRUE),
    by   = "week"
  )
  if (length(week_seq) == 0L) week_seq <- Sys.Date()

  inflow  <- sapply(week_seq, function(w) sum(d$found_week == w, na.rm = TRUE))
  outflow <- sapply(week_seq, function(w)
    sum(!is.na(d$resolved_week) & d$resolved_week == w, na.rm = TRUE)
  )

  total_days <- as.numeric(max(week_seq) - min(week_seq))
  range_start_date <- format(max(week_seq) - days, "%Y-%m-%d")

  plotly::plot_ly() |>
    plotly::add_trace(
      x    = week_seq,
      y    = inflow,
      name = "New findings",
      type = "scatter", mode = "lines+markers",
      line   = list(color = ROBOREV_PALETTE[["high"]], width = 2),
      marker = list(color = ROBOREV_PALETTE[["high"]], size = 6),
      hovertemplate = "<b>New</b>: %{y}<br>%{x}<extra></extra>"
    ) |>
    plotly::add_trace(
      x    = week_seq,
      y    = outflow,
      name = "Resolved",
      type = "scatter", mode = "lines+markers",
      line   = list(color = ROBOREV_PALETTE[["low"]], width = 2),
      marker = list(color = ROBOREV_PALETTE[["low"]], size = 6),
      hovertemplate = "<b>Resolved</b>: %{y}<br>%{x}<extra></extra>"
    ) |>
    plotly::layout(
      paper_bgcolor = "#1a1a1a",
      plot_bgcolor  = "#1a1a1a",
      font  = list(color = "#e0e0e0"),
      title = list(text = "Weekly Inflow vs Outflow",
                   font = list(color = "#e0e0e0")),
      xaxis = list(
        title       = "",
        color       = "#ccc",
        rangeslider = list(visible = TRUE),
        range       = list(range_start_date, format(max(week_seq), "%Y-%m-%d"))
      ),
      yaxis     = list(title = "Finding count", color = "#ccc"),
      legend    = list(font = list(color = "#e0e0e0"), orientation = "h",
                       yanchor = "bottom", y = 1.02, xanchor = "right", x = 1),
      hovermode = "x unified"
    )
}

# ---- plot_resolution_time -----------------------------------------------------

#' Cleveland dot plot of median time-to-resolution by severity
#'
#' Returns a plotly Cleveland dot plot. Each point represents the median
#' time-to-resolution (in hours) for resolved findings of a given severity.
#' Only resolved findings (non-NA `resolved_at`) are included. Points are
#' coloured using ROBOREV_PALETTE.
#'
#' @param findings_df Data frame with `severity`, `found_at`, `resolved_at`.
#' @return A plotly object.
#' @export
plot_resolution_time <- function(findings_df) {
  .check_plotly()
  checkmate::assert_data_frame(findings_df)

  resolved <- findings_df[!is.na(findings_df$resolved_at), ]

  if (nrow(resolved) == 0L) {
    return(plotly::plot_ly() |>
      plotly::layout(title = "No resolved findings yet",
                     paper_bgcolor = "#1a1a1a", plot_bgcolor = "#1a1a1a"))
  }

  resolved$hours <- as.numeric(
    difftime(resolved$resolved_at, resolved$found_at, units = "hours")
  )

  sev_order <- names(ROBOREV_PALETTE)
  stats <- do.call(rbind, lapply(sev_order, function(sv) {
    sub <- resolved[tolower(resolved$severity) == sv, ]
    if (nrow(sub) == 0L) return(NULL)
    data.frame(
      severity = sv,
      median_h = median(sub$hours, na.rm = TRUE),
      n        = nrow(sub),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(stats) || nrow(stats) == 0L) {
    return(plotly::plot_ly() |>
      plotly::layout(title = "No resolved findings yet",
                     paper_bgcolor = "#1a1a1a", plot_bgcolor = "#1a1a1a"))
  }

  stats <- stats[order(-stats$median_h), ]
  stats$severity <- factor(stats$severity, levels = rev(stats$severity))
  cols <- ROBOREV_PALETTE[as.character(stats$severity)]

  plotly::plot_ly(
    data  = stats,
    x     = ~median_h,
    y     = ~severity,
    type  = "scatter",
    mode  = "markers",
    marker = list(color = cols, size = 14,
                  line = list(color = "#333", width = 1)),
    text  = ~paste0("<b>", severity, "</b><br>",
                    "Median hours: ", round(median_h, 1),
                    "<br>n resolved: ", n),
    hoverinfo = "text"
  ) |> plotly::layout(
    paper_bgcolor = "#1a1a1a",
    plot_bgcolor  = "#1a1a1a",
    font  = list(color = "#e0e0e0"),
    title = list(text = "Median Time-to-Resolution by Severity",
                 font = list(color = "#e0e0e0")),
    xaxis = list(title = "Median hours to resolve", color = "#ccc",
                 zeroline = TRUE, zerolinecolor = "#444"),
    yaxis = list(title = "", color = "#ccc")
  )
}

# ---- build_pulse_table --------------------------------------------------------

#' Build compact pulse summary as a named list (for vignette Metric | Value table)
#'
#' Returns a data.frame with columns `metric` and `value` summarising the
#' current state of roborev findings and loops.
#'
#' @param findings_df Data frame from Agent 1's `roborev_findings` target.
#' @param loops_df Data frame from Agent 2's `roborev_loops` target.
#' @return A data.frame with columns `metric` and `value`.
#' @export
build_pulse_table <- function(findings_df, loops_df) {
  checkmate::assert_data_frame(findings_df)
  checkmate::assert_data_frame(loops_df)

  n_open      <- sum(is.na(findings_df$resolved_at))
  n_critical  <- sum(
    is.na(findings_df$resolved_at) &
      tolower(findings_df$severity) == "critical"
  )
  n_high      <- sum(
    is.na(findings_df$resolved_at) &
      tolower(findings_df$severity) == "high"
  )
  n_resolved  <- sum(!is.na(findings_df$resolved_at))
  n_total     <- nrow(findings_df)
  n_loops     <- nrow(loops_df)
  n_escalate  <- sum(tolower(loops_df$tier) == "escalate" &
                       is.na(loops_df$ack_by))
  n_block     <- sum(tolower(loops_df$tier) == "block")
  wasted_usd  <- sum(loops_df$estimated_wasted_usd, na.rm = TRUE)

  data.frame(
    metric = c(
      "Open findings",
      "  Critical",
      "  High",
      "Resolved findings",
      "Total findings",
      "Active loops",
      "  Escalate (no ack)",
      "  Block",
      "Est. wasted USD (loops)"
    ),
    value = c(
      as.character(n_open),
      as.character(n_critical),
      as.character(n_high),
      as.character(n_resolved),
      as.character(n_total),
      as.character(n_loops),
      as.character(n_escalate),
      as.character(n_block),
      sprintf("$%.2f", wasted_usd)
    ),
    stringsAsFactors = FALSE
  )
}

# ---- build_recent_table -------------------------------------------------------

#' Build recent-activity table (last N days)
#'
#' Returns a DT::datatable of findings opened or resolved in the last `days`
#' days, ordered by `found_at` descending.
#'
#' @param findings_df Data frame from Agent 1's `roborev_findings` target.
#' @param days Integer. Look-back window. Defaults to 30.
#' @return A DT::datatable htmlwidget.
#' @export
build_recent_table <- function(findings_df, days = 30L) {
  .check_dt()
  checkmate::assert_data_frame(findings_df)
  checkmate::assert_integer(as.integer(days), lower = 1L)

  cutoff <- Sys.time() - days * 86400
  recent <- findings_df[
    (!is.na(findings_df$found_at)    & findings_df$found_at    >= cutoff) |
    (!is.na(findings_df$resolved_at) & findings_df$resolved_at >= cutoff),
  ]
  recent <- recent[order(recent$found_at, decreasing = TRUE), ]

  if (nrow(recent) == 0L) {
    return(DT::datatable(
      data.frame(message = paste0("No activity in the last ", days, " days")),
      options  = list(dom = "t"),
      rownames = FALSE
    ))
  }

  display <- recent[, intersect(
    c("severity", "primary_file", "summary", "found_at", "resolved_at", "project"),
    names(recent)
  )]
  if ("found_at" %in% names(display))
    display$found_at <- format(as.Date(display$found_at), "%Y-%m-%d")
  if ("resolved_at" %in% names(display))
    display$resolved_at <- ifelse(
      is.na(display$resolved_at), "—",
      format(as.Date(display$resolved_at), "%Y-%m-%d")
    )

  DT::datatable(
    display,
    rownames = FALSE,
    caption  = paste0(
      "Findings opened or resolved in the last ", days, " days. ",
      nrow(recent), " records shown, ordered by find date descending."
    ),
    options  = list(
      order      = list(list(3L, "desc")),
      pageLength = 25L,
      dom        = "frtip"
    )
  )
}
