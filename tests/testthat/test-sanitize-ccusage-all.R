# Tests for sanitize_ccusage_all.R helper functions.
# These unit tests construct fixture data with known path leaks and verify that
# sanitize_session_all() and sanitize_daily_all() produce clean output.

# Source the script in test mode so only functions are loaded, not the main block.
options(sanitize_ccusage_all_sourced_for_test = TRUE)
source(system.file("scripts", "sanitize_ccusage_all.R", package = "llmtelemetry"))

# ---------------------------------------------------------------------------
# Fixture helpers
# ---------------------------------------------------------------------------

make_session_fixture <- function() {
  list(
    sessions = list(
      list(
        sessionId        = "-Users-johngavin-docs-gh-llm",
        inputTokens      = 100L,
        outputTokens     = 200L,
        totalCost        = 0.5,
        lastActivity     = "2026-05-01",
        modelsUsed       = list("claude-opus-4-6"),
        modelBreakdowns  = list(),
        projectPath      = "Unknown Project"
      ),
      list(
        sessionId        = "subagents",
        inputTokens      = 50L,
        outputTokens     = 80L,
        totalCost        = 0.1,
        lastActivity     = "2026-05-02",
        modelsUsed       = list("claude-sonnet-4-6"),
        modelBreakdowns  = list(),
        projectPath      = "-Users-johngavin-docs-gh-llmtelemetry/abc12345-dead-beef-0000-111111111111"
      ),
      list(
        sessionId        = "-Users-johngavin-docs--pers-NHS-health-data-antigravity-mycare",
        inputTokens      = 10L,
        outputTokens     = 30L,
        totalCost        = 0.05,
        lastActivity     = "2026-05-03",
        modelsUsed       = list("claude-haiku-4-6"),
        modelBreakdowns  = list(),
        projectPath      = "Unknown Project"
      )
    )
  )
}

make_daily_fixture <- function() {
  list(
    projects = list(
      "-Users-johngavin-docs-gh-llm" = list(
        list(date = "2026-04-01", inputTokens = 10L, outputTokens = 5L,
             cacheCreationTokens = 0L, cacheReadTokens = 0L,
             totalTokens = 15L, totalCost = 0.05,
             modelsUsed = list("claude-opus-4-6"), modelBreakdowns = list())
      ),
      "subagents" = list(
        list(date = "2026-04-01", inputTokens = 1L, outputTokens = 2L,
             cacheCreationTokens = 0L, cacheReadTokens = 0L,
             totalTokens = 3L, totalCost = 0.01,
             modelsUsed = list("claude-sonnet-4-6"), modelBreakdowns = list())
      ),
      "-Users-johngavin-docs-gh-llmtelemetry" = list(
        list(date = "2026-04-01", inputTokens = 20L, outputTokens = 10L,
             cacheCreationTokens = 0L, cacheReadTokens = 0L,
             totalTokens = 30L, totalCost = 0.1,
             modelsUsed = list("claude-opus-4-6"), modelBreakdowns = list())
      )
    )
  )
}

FORBIDDEN_PATTERNS <- c(
  "-Users-johngavin",
  "sessionId.*Users",
  "projectPath.*Users",
  "antigravity",
  "pers-NHS"
)

contains_leak <- function(text) {
  any(vapply(FORBIDDEN_PATTERNS, function(p) grepl(p, text, perl = TRUE), logical(1)))
}

# ---------------------------------------------------------------------------
# sanitize_session_all() tests
# ---------------------------------------------------------------------------

test_that("sanitize_session_all removes path-style sessionId values", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)

  session_ids <- vapply(result$sanitized_data$sessions, `[[`, character(1), "sessionId")
  # No sessionId should start with -Users-
  expect_false(any(grepl("^-Users-", session_ids)))
})

test_that("sanitize_session_all removes path-style projectPath values", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)

  project_paths <- vapply(result$sanitized_data$sessions, `[[`, character(1), "projectPath")
  expect_false(any(grepl("^-Users-", project_paths)))
})

test_that("sanitize_session_all reports correct count of sanitized fields", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)
  # Session 1: sessionId is a path (1 field sanitized); projectPath is "Unknown Project" (0)
  # Session 2: sessionId is "subagents" (0); projectPath is a path (1)
  # Session 3: sessionId is a path (1); projectPath is "Unknown Project" (0)
  # Total = 3
  expect_equal(result$n_sanitized, 3L)
})

test_that("sanitize_session_all preserves non-path sessionId values unchanged", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)
  # "subagents" is not a path — must survive unchanged
  session_ids <- vapply(result$sanitized_data$sessions, `[[`, character(1), "sessionId")
  expect_true("subagents" %in% session_ids)
})

test_that("sanitize_session_all output contains no forbidden leak patterns", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)
  json_text <- jsonlite::toJSON(result$sanitized_data, auto_unbox = TRUE)
  expect_false(contains_leak(json_text))
})

test_that("sanitize_session_all replaces NHS path patterns", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)
  json_text <- jsonlite::toJSON(result$sanitized_data, auto_unbox = TRUE)
  expect_false(grepl("antigravity", json_text))
  expect_false(grepl("pers-NHS", json_text))
})

test_that("sanitize_session_all preserves token and cost data", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)
  # First session should still have its cost
  expect_equal(result$sanitized_data$sessions[[1]]$totalCost, 0.5)
  expect_equal(result$sanitized_data$sessions[[1]]$inputTokens, 100L)
})

test_that("sanitize_session_all stable hash: same input always same output", {
  fixture <- make_session_fixture()
  r1 <- sanitize_session_all(fixture)
  r2 <- sanitize_session_all(fixture)
  ids1 <- vapply(r1$sanitized_data$sessions, `[[`, character(1), "sessionId")
  ids2 <- vapply(r2$sanitized_data$sessions, `[[`, character(1), "sessionId")
  expect_identical(ids1, ids2)
})

# ---------------------------------------------------------------------------
# sanitize_daily_all() tests
# ---------------------------------------------------------------------------

test_that("sanitize_daily_all removes path-style project keys", {
  fixture <- make_daily_fixture()
  result <- sanitize_daily_all(fixture)
  project_names <- names(result$sanitized_data$projects)
  expect_false(any(grepl("^-Users-", project_names)))
})

test_that("sanitize_daily_all reports correct count of sanitized keys", {
  fixture <- make_daily_fixture()
  result <- sanitize_daily_all(fixture)
  # Two path-style keys: -Users-johngavin-docs-gh-llm, -Users-johngavin-docs-gh-llmtelemetry
  expect_equal(result$n_sanitized, 2L)
})

test_that("sanitize_daily_all preserves non-path keys unchanged", {
  fixture <- make_daily_fixture()
  result <- sanitize_daily_all(fixture)
  expect_true("subagents" %in% names(result$sanitized_data$projects))
})

test_that("sanitize_daily_all output contains no forbidden leak patterns", {
  fixture <- make_daily_fixture()
  result <- sanitize_daily_all(fixture)
  json_text <- jsonlite::toJSON(result$sanitized_data, auto_unbox = TRUE)
  expect_false(contains_leak(json_text))
})

test_that("sanitize_daily_all preserves entry data after key rename", {
  fixture <- make_daily_fixture()
  result <- sanitize_daily_all(fixture)
  # All canonical project entries must still have their date arrays
  for (proj_entries in result$sanitized_data$projects) {
    expect_true(is.list(proj_entries))
    expect_true(length(proj_entries) >= 1L)
  }
})

# ---------------------------------------------------------------------------
# Round-trip: write to tempfile, re-read, verify
# ---------------------------------------------------------------------------

test_that("session sanitization round-trips through JSON without leaks", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(result$sanitized_data, tmp, auto_unbox = TRUE, pretty = TRUE)
  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_false(contains_leak(txt))
})

test_that("daily sanitization round-trips through JSON without leaks", {
  fixture <- make_daily_fixture()
  result <- sanitize_daily_all(fixture)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(result$sanitized_data, tmp, auto_unbox = TRUE, pretty = TRUE)
  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_false(contains_leak(txt))
})
