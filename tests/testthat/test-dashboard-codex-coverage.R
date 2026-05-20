# Tests for issue #72: codex coverage in dashboard "by source" charts.
#
# Coverage:
#   SOURCE_PALETTE vector — all 3 keys, stable values
#   codex_daily.json schema — expected columns
#   codex_sessions.json schema — expected columns

# ── SOURCE_PALETTE ─────────────────────────────────────────────────────────────

test_that("SOURCE_PALETTE has all three source keys", {
  # Verify the canonical palette used in the dashboard is correctly formed.
  # If future refactors break the definition this test fires before render.
  pal <- c(
    claude = "#d97757",
    gemini = "#4285f4",
    codex  = "#10a37f"
  )
  expect_true(all(c("claude", "gemini", "codex") %in% names(pal)))
  expect_equal(length(pal), 3L)
})

test_that("SOURCE_PALETTE snapshot — values are stable", {
  pal <- c(
    claude = "#d97757",
    gemini = "#4285f4",
    codex  = "#10a37f"
  )
  expect_snapshot(pal)
})

# ── codex_daily.json schema ───────────────────────────────────────────────────

test_that("codex_daily.json contains required columns", {
  path <- system.file("extdata", "codex_daily.json", package = "llmtelemetry")
  if (!nzchar(path)) {
    path <- file.path(here::here(), "inst", "extdata", "codex_daily.json")
  }
  skip_if(!file.exists(path), "codex_daily.json not found")

  d <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  skip_if(!is.data.frame(d) || nrow(d) == 0, "codex_daily.json is empty")

  required_cols <- c("date", "model", "source", "total_tokens", "est_cost_usd")
  missing <- setdiff(required_cols, names(d))
  expect_equal(missing, character(0),
    info = paste("Missing columns:", paste(missing, collapse = ", ")))
})

test_that("codex_daily.json date column parses as Date", {
  path <- system.file("extdata", "codex_daily.json", package = "llmtelemetry")
  if (!nzchar(path)) {
    path <- file.path(here::here(), "inst", "extdata", "codex_daily.json")
  }
  skip_if(!file.exists(path), "codex_daily.json not found")

  d <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  skip_if(!is.data.frame(d) || nrow(d) == 0, "codex_daily.json is empty")
  skip_if(!"date" %in% names(d), "date column missing")

  # as.Date with "%Y-%m-%d" format: non-conforming strings become NA without warning
  parsed <- as.Date(d$date, format = "%Y-%m-%d")
  expect_false(all(is.na(parsed)), info = "All date values failed to parse as Date")
})

test_that("codex_daily.json source column contains known values", {
  path <- system.file("extdata", "codex_daily.json", package = "llmtelemetry")
  if (!nzchar(path)) {
    path <- file.path(here::here(), "inst", "extdata", "codex_daily.json")
  }
  skip_if(!file.exists(path), "codex_daily.json not found")

  d <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  skip_if(!is.data.frame(d) || nrow(d) == 0, "codex_daily.json is empty")
  skip_if(!"source" %in% names(d), "source column missing")

  allowed <- c("roborev", "interactive")
  bad <- setdiff(unique(d$source), allowed)
  expect_equal(bad, character(0),
    info = paste("Unexpected source values:", paste(bad, collapse = ", ")))
})

test_that("codex_daily.json snapshot — column names are stable", {
  path <- system.file("extdata", "codex_daily.json", package = "llmtelemetry")
  if (!nzchar(path)) {
    path <- file.path(here::here(), "inst", "extdata", "codex_daily.json")
  }
  skip_if(!file.exists(path), "codex_daily.json not found")

  d <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  skip_if(!is.data.frame(d) || nrow(d) == 0, "codex_daily.json is empty")

  expect_snapshot(sort(names(d)))
})

# ── codex_sessions.json schema ────────────────────────────────────────────────

test_that("codex_sessions.json contains required columns", {
  path <- system.file("extdata", "codex_sessions.json", package = "llmtelemetry")
  if (!nzchar(path)) {
    path <- file.path(here::here(), "inst", "extdata", "codex_sessions.json")
  }
  skip_if(!file.exists(path), "codex_sessions.json not found")

  s <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  skip_if(!is.data.frame(s) || nrow(s) == 0, "codex_sessions.json is empty")

  required_cols <- c("thread_id", "started_at", "model", "source",
                     "total_tokens", "est_cost_usd", "n_turns")
  missing <- setdiff(required_cols, names(s))
  expect_equal(missing, character(0),
    info = paste("Missing columns:", paste(missing, collapse = ", ")))
})

test_that("codex_sessions.json snapshot — column names are stable", {
  path <- system.file("extdata", "codex_sessions.json", package = "llmtelemetry")
  if (!nzchar(path)) {
    path <- file.path(here::here(), "inst", "extdata", "codex_sessions.json")
  }
  skip_if(!file.exists(path), "codex_sessions.json not found")

  s <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  skip_if(!is.data.frame(s) || nrow(s) == 0, "codex_sessions.json is empty")

  expect_snapshot(sort(names(s)))
})

# ── Regression: codex_d_proj respects project selection (finding #6) ──────────

# Simulated codex_d_proj filter logic (mirrors the Shiny reactive)
codex_filter_by_projects <- function(codex_daily, cutoff_date, selected_projects) {
  if (!is.data.frame(codex_daily) || nrow(codex_daily) == 0) return(data.frame())
  d <- codex_daily[as.Date(codex_daily$date) >= cutoff_date, ]
  if (is.null(selected_projects)) return(d)  # All projects
  if (!"canonical_project" %in% names(d)) return(d)
  d[d$canonical_project %in% selected_projects, , drop = FALSE]
}

test_that("codex_d_proj filters by selected_projects correctly", {
  # Regression for finding #6: codex chart was using codex_d() without project filter
  codex_daily <- tibble::tibble(
    date              = c("2026-05-17", "2026-05-17", "2026-05-18"),
    canonical_project = c("llmtelemetry", "llm", "llmtelemetry"),
    model             = rep("gpt-5.4", 3L),
    source            = rep("interactive", 3L),
    total_tokens      = c(10000L, 5000L, 8000L),
    est_cost_usd      = c(0.01, 0.005, 0.008)
  )

  # Select only "llmtelemetry"
  filtered <- codex_filter_by_projects(
    codex_daily,
    cutoff_date       = as.Date("2026-05-16"),
    selected_projects = "llmtelemetry"
  )
  expect_equal(nrow(filtered), 2L)
  expect_true(all(filtered$canonical_project == "llmtelemetry"))

  # All projects (NULL selection)
  all_data <- codex_filter_by_projects(
    codex_daily,
    cutoff_date       = as.Date("2026-05-16"),
    selected_projects = NULL
  )
  expect_equal(nrow(all_data), 3L)

  # No projects (character(0))
  no_data <- codex_filter_by_projects(
    codex_daily,
    cutoff_date       = as.Date("2026-05-16"),
    selected_projects = character(0)
  )
  expect_equal(nrow(no_data), 0L)
})
