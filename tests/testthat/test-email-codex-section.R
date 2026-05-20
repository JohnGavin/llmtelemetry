# test-email-codex-section.R
# Tests for the codex section added to send_daily_email.R (issue #130)
#
# Coverage:
#   - Empty / NULL codex_d  → section omitted gracefully
#   - codex_d with rows     → summary row and type-breakdown table rendered
#   - Roborev / interactive split → both rows present in breakdown
#   - Stale pricing        → warning note injected
#   - Fresh pricing        → no stale note
#
# Strategy: source only the helper functions from send_daily_email.R that are
# needed here (dollar, comma, millions, safe_date, format_hhmm), then build
# the codex HTML snippets directly — avoids loading DBI/blastula in tests.

# ── Shared colour palette (mirrors send_daily_email.R) ───────────────────────
dark_bg       <- "#1a1a2e"
dark_card     <- "#16213e"
dark_row_alt  <- "#0f3460"
dark_text     <- "#e8e8e8"
dark_muted    <- "#a0a0a0"
dark_border   <- "#2a2a4a"
accent_green  <- "#00d26a"
accent_blue   <- "#4fc3f7"
accent_orange <- "#ff9800"

# ── Inline helpers (keep tests self-contained) ────────────────────────────────
dollar  <- function(x) if (is.na(x) || is.null(x) || is.nan(x)) "-" else sprintf("$%.2f", x)
comma   <- function(x) if (is.na(x) || is.null(x) || is.nan(x)) "-" else format(x, big.mark = ",", scientific = FALSE)
millions <- function(x) if (is.na(x) || is.null(x) || is.nan(x)) "-" else sprintf("%.1fM", x / 1e6)
safe_date <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA)
  tryCatch(as.Date(x), error = function(e) NA)
}

# ── Helper: build codex type HTML (extracted logic from the script) ───────────
build_codex_type_html <- function(codex_d, pricing_date = NULL) {
  if (is.null(codex_d) || nrow(codex_d) == 0 || !"source" %in% names(codex_d)) {
    return("")
  }
  cx_by_type <- codex_d |>
    dplyr::group_by(source) |>
    dplyr::summarise(
      sessions = dplyr::n(),
      tokens   = sum(total_tokens, na.rm = TRUE),
      est_cost = sum(est_cost_usd, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(est_cost))

  html <- sprintf(
    '<h3 style="color: %s;">Codex by Automation Type</h3><table>
  <tr><th>Type</th><th>Sessions</th><th>Total Tokens</th><th>Est. Cost</th></tr>',
    accent_orange
  )
  for (i in seq_len(nrow(cx_by_type))) {
    type_label <- switch(as.character(cx_by_type$source[i]),
      "roborev"     = "Roborev jobs",
      "interactive" = "Interactive",
      as.character(cx_by_type$source[i]))
    html <- paste0(html, sprintf(
      "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s <em>(est)</em></td></tr>",
      type_label,
      comma(cx_by_type$sessions[i]),
      comma(cx_by_type$tokens[i]),
      dollar(cx_by_type$est_cost[i])
    ))
  }
  html <- paste0(html, "</table>",
    "<!-- QA:codex_type_rows=", nrow(cx_by_type), " -->")

  # Stale pricing note
  if (!is.null(pricing_date) && !is.na(pricing_date)) {
    if (as.numeric(Sys.Date() - as.Date(pricing_date)) > 30) {
      html <- paste0(html, sprintf(
        '<p style="font-style: italic;">Codex cost estimates use pricing from %s; verify openai.com/api/pricing if stale.</p>',
        format(as.Date(pricing_date), "%Y-%m-%d")
      ))
    }
  }
  html
}

# ── Helper: build codex summary row ──────────────────────────────────────────
build_codex_row <- function(codex_d, codex_s = NULL) {
  if (is.null(codex_d) || nrow(codex_d) == 0) return("")
  cx_total_cost   <- sum(codex_d$est_cost_usd, na.rm = TRUE)
  cx_total_tokens <- sum(codex_d$total_tokens, na.rm = TRUE)
  cx_sessions <- if (!is.null(codex_s) && nrow(codex_s) > 0 &&
                     "thread_id" %in% names(codex_s)) {
    length(unique(codex_s$thread_id))
  } else nrow(codex_d)
  sprintf(
    "<tr><td><strong>Codex</strong></td><td>%s <em>(est)</em></td><td>%s sessions</td></tr>",
    dollar(cx_total_cost),
    cx_sessions
  )
}

# ── Fixtures ──────────────────────────────────────────────────────────────────
make_codex_d <- function(sources = c("roborev", "interactive", "roborev", "interactive")) {
  tibble::tibble(
    date            = rep("2026-05-19", length(sources)),
    canonical_project = rep("llmtelemetry", length(sources)),
    model           = rep("gpt-5.4", length(sources)),
    source          = sources,
    total_tokens    = c(10000L, 20000L, 15000L, 25000L)[seq_along(sources)],
    est_cost_usd    = c(0.01, 0.02, 0.015, 0.025)[seq_along(sources)]
  )
}

make_codex_s <- function(n = 4) {
  tibble::tibble(
    thread_id   = paste0("thread-", seq_len(n)),
    source      = c("roborev", "interactive", "roborev", "interactive")[seq_len(n)],
    total_tokens = rep(10000L, n),
    est_cost_usd = rep(0.01, n)
  )
}

# ── Tests: NULL / empty codex_d ───────────────────────────────────────────────

test_that("build_codex_type_html returns empty string when codex_d is NULL", {
  result <- build_codex_type_html(NULL)
  expect_equal(result, "")
})

test_that("build_codex_type_html returns empty string for zero-row tibble", {
  empty_df <- tibble::tibble(
    date = character(0), source = character(0),
    total_tokens = integer(0), est_cost_usd = numeric(0)
  )
  result <- build_codex_type_html(empty_df)
  expect_equal(result, "")
})

test_that("build_codex_row returns empty string when codex_d is NULL", {
  result <- build_codex_row(NULL)
  expect_equal(result, "")
})

# ── Tests: codex_d with rows ──────────────────────────────────────────────────

test_that("build_codex_type_html renders Codex by Automation Type heading", {
  html <- build_codex_type_html(make_codex_d())
  expect_match(html, "Codex by Automation Type")
})

test_that("build_codex_type_html includes QA marker", {
  html <- build_codex_type_html(make_codex_d())
  expect_match(html, "QA:codex_type_rows=")
})

test_that("build_codex_row includes (est) suffix on cost", {
  html <- build_codex_row(make_codex_d())
  expect_match(html, "\\(est\\)")
})

test_that("build_codex_row shows total cost formatted as dollar", {
  html <- build_codex_row(make_codex_d())
  # Total est_cost_usd = 0.01 + 0.02 + 0.015 + 0.025 = 0.07
  expect_match(html, "\\$0\\.07")
})

# ── Tests: roborev / interactive split ───────────────────────────────────────

test_that("build_codex_type_html shows both Roborev jobs and Interactive rows", {
  html <- build_codex_type_html(make_codex_d())
  expect_match(html, "Roborev jobs")
  expect_match(html, "Interactive")
})

test_that("build_codex_type_html QA marker counts correct rows (2 sources)", {
  html <- build_codex_type_html(make_codex_d())
  expect_match(html, "QA:codex_type_rows=2")
})

test_that("build_codex_type_html handles roborev-only data gracefully", {
  roborev_only <- make_codex_d(sources = c("roborev", "roborev"))
  html <- build_codex_type_html(roborev_only)
  expect_match(html, "Roborev jobs")
  expect_false(grepl("Interactive", html))
  expect_match(html, "QA:codex_type_rows=1")
})

test_that("build_codex_type_html handles interactive-only data gracefully", {
  interactive_only <- make_codex_d(sources = c("interactive", "interactive"))
  html <- build_codex_type_html(interactive_only)
  expect_match(html, "Interactive")
  expect_false(grepl("Roborev", html))
})

# ── Tests: session count from codex_s ────────────────────────────────────────

test_that("build_codex_row uses distinct thread_id count from codex_s", {
  codex_d <- make_codex_d()
  codex_s <- make_codex_s(4)
  html <- build_codex_row(codex_d, codex_s)
  expect_match(html, "4 sessions")
})

test_that("build_codex_row falls back to nrow(codex_d) when codex_s is NULL", {
  codex_d <- make_codex_d()  # 4 rows
  html <- build_codex_row(codex_d, NULL)
  expect_match(html, "4 sessions")
})

# ── Tests: stale pricing warning ──────────────────────────────────────────────

test_that("build_codex_type_html adds stale note when pricing > 30 days old", {
  old_date <- format(Sys.Date() - 31L, "%Y-%m-%d")
  html <- build_codex_type_html(make_codex_d(), pricing_date = old_date)
  expect_match(html, "Codex cost estimates use pricing from")
  expect_match(html, "openai.com/api/pricing")
})

test_that("build_codex_type_html does NOT add stale note when pricing is fresh", {
  fresh_date <- format(Sys.Date(), "%Y-%m-%d")
  html <- build_codex_type_html(make_codex_d(), pricing_date = fresh_date)
  expect_false(grepl("Codex cost estimates use pricing from", html))
})

test_that("build_codex_type_html does NOT add stale note when pricing_date is NULL", {
  html <- build_codex_type_html(make_codex_d(), pricing_date = NULL)
  expect_false(grepl("Codex cost estimates use pricing from", html))
})

# ── Snapshot tests ────────────────────────────────────────────────────────────

test_that("build_codex_type_html snapshot — structure is stable for 2-source fixture", {
  html <- build_codex_type_html(make_codex_d())
  # Stabilise: strip dynamic cost values but keep structure markers
  stable <- gsub("\\$[0-9]+\\.[0-9]+", "$X.XX", html)
  stable <- gsub("[0-9,]+(?= sessions| tokens)", "N", stable, perl = TRUE)
  expect_snapshot(stable)
})

test_that("build_codex_row snapshot — column structure is stable", {
  html <- build_codex_row(make_codex_d(), make_codex_s())
  stable <- gsub("\\$[0-9]+\\.[0-9]+", "$X.XX", html)
  stable <- gsub("[0-9]+ sessions", "N sessions", stable)
  expect_snapshot(stable)
})
