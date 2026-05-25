# test-codexbar.R — Unit tests + PII regression gate for CodexBar data layer.
#
# Tests cover:
#   1. parse_codexbar_usage()  — shape + fixture row values
#   2. parse_codexbar_cost()   — shape + fixture row values
#   3. sanitize_codexbar.R helpers — PII stripping logic (strip_pii_from_usage_provider,
#      sanitize_usage, verify_no_pii_leak)
#   4. PII-leak regression gate — no accountEmail/@/accountOrganization/loginMethod
#      in sanitised fixture output

# ---------------------------------------------------------------------------
# Source the sanitizer helpers once at file level (same pattern as
# test-sanitize-ccusage-all.R).  Setting the option first prevents the main()
# block from running.
# ---------------------------------------------------------------------------
options(sanitize_codexbar_sourced_for_test = TRUE)
source(system.file("scripts", "sanitize_codexbar.R", package = "llmtelemetry"))

fixture_path <- function(fname) {
  testthat::test_path("fixtures", fname)
}

# ---------------------------------------------------------------------------
# 1. parse_codexbar_usage — shape
# ---------------------------------------------------------------------------

test_that("parse_codexbar_usage returns a tibble with correct columns from fixture", {
  result <- parse_codexbar_usage(fixture_path("codexbar_usage_fixture.json"))

  expect_s3_class(result, "tbl_df")
  expected_cols <- c(
    "provider", "limit_window", "used_pct", "window_minutes",
    "resets_at", "credits_remaining", "source", "updated_at"
  )
  expect_named(result, expected_cols, ignore.order = TRUE)
  expect_gt(nrow(result), 0L)
})

test_that("parse_codexbar_usage returns empty tibble for non-existent file", {
  result <- parse_codexbar_usage("/nonexistent/path/codexbar_usage.json")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_true("provider" %in% names(result))
})

test_that("parse_codexbar_usage extracts known fixture row correctly", {
  result <- parse_codexbar_usage(fixture_path("codexbar_usage_fixture.json"))

  claude_primary <- result[result$provider == "claude" &
                             result$limit_window == "primary", ]
  expect_equal(nrow(claude_primary), 1L)
  expect_equal(claude_primary$used_pct,          42.5, tolerance = 1e-6)
  expect_equal(claude_primary$window_minutes,      60L)
  expect_equal(claude_primary$credits_remaining,   85.5, tolerance = 1e-6)
  expect_equal(claude_primary$source,             "api")
})

test_that("parse_codexbar_usage handles multiple providers and windows", {
  result <- parse_codexbar_usage(fixture_path("codexbar_usage_fixture.json"))

  # Fixture: claude has 2 windows, openai has 1
  expect_equal(nrow(result), 3L)
  expect_setequal(unique(result$provider), c("claude", "openai"))
  expect_setequal(
    result$limit_window[result$provider == "claude"],
    c("primary", "secondary")
  )
})

# ---------------------------------------------------------------------------
# 2. parse_codexbar_cost — shape
# ---------------------------------------------------------------------------

test_that("parse_codexbar_cost returns a tibble with correct columns from fixture", {
  result <- parse_codexbar_cost(fixture_path("codexbar_cost_fixture.json"))

  expect_s3_class(result, "tbl_df")
  expected_cols <- c(
    "provider", "date", "model", "cost", "total_tokens",
    "input_tokens", "output_tokens",
    "cache_read_tokens", "cache_creation_tokens"
  )
  expect_named(result, expected_cols, ignore.order = TRUE)
  expect_gt(nrow(result), 0L)
})

test_that("parse_codexbar_cost returns empty tibble for non-existent file", {
  result <- parse_codexbar_cost("/nonexistent/path/codexbar_cost_daily.json")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_true("provider" %in% names(result))
})

test_that("parse_codexbar_cost extracts known fixture row correctly", {
  result <- parse_codexbar_cost(fixture_path("codexbar_cost_fixture.json"))

  row <- result[
    result$provider == "claude" &
      result$date == "2026-05-24" &
      result$model == "claude-sonnet-4-6",
  ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$cost,                  1.23,  tolerance = 1e-6)
  expect_equal(row$total_tokens,          45000, tolerance = 1e-6)
  expect_equal(row$input_tokens,          30000, tolerance = 1e-6)
  expect_equal(row$output_tokens,         10000, tolerance = 1e-6)
  expect_equal(row$cache_read_tokens,     4000,  tolerance = 1e-6)
  expect_equal(row$cache_creation_tokens, 1000,  tolerance = 1e-6)
})

test_that("parse_codexbar_cost expands model breakdowns (one row per provider x date x model)", {
  result <- parse_codexbar_cost(fixture_path("codexbar_cost_fixture.json"))

  # Fixture: claude has 2 dates (1 model each) + openai has 1 date (1 model) = 3 rows
  expect_equal(nrow(result), 3L)
  expect_setequal(unique(result$provider), c("claude", "openai"))
  expect_true("gpt-4o-mini" %in% result$model)
})

test_that("parse_codexbar_cost numeric columns are non-negative", {
  result <- parse_codexbar_cost(fixture_path("codexbar_cost_fixture.json"))

  numeric_cols <- c(
    "cost", "total_tokens", "input_tokens", "output_tokens",
    "cache_read_tokens", "cache_creation_tokens"
  )
  for (col in numeric_cols) {
    expect_true(all(result[[col]] >= 0, na.rm = TRUE),
                info = sprintf("Column '%s' must be non-negative", col))
  }
})

# ---------------------------------------------------------------------------
# 3. sanitize_codexbar.R helpers — PII stripping
# (functions sourced at file level above)
# ---------------------------------------------------------------------------

test_that("strip_pii_from_usage_provider removes all four PII fields from usage sub-object", {
  raw <- list(
    provider = "claude",
    usage = list(
      primary        = list(usedPercent = 50, windowMinutes = 60, resetsAt = "2026-01-01T00:00:00Z"),
      updatedAt      = "2026-01-01T00:00:00Z",
      accountEmail   = "secret@example.com",
      accountOrganization = "SecretOrg",
      identity       = "uid_secret",
      loginMethod    = "sso"
    ),
    credits = list(remaining = 100)
  )

  cleaned <- strip_pii_from_usage_provider(raw)

  expect_null(cleaned[["usage"]][["accountEmail"]])
  expect_null(cleaned[["usage"]][["accountOrganization"]])
  expect_null(cleaned[["usage"]][["identity"]])
  expect_null(cleaned[["usage"]][["loginMethod"]])

  # Non-PII fields must survive
  expect_equal(cleaned[["provider"]], "claude")
  expect_equal(cleaned[["usage"]][["primary"]][["usedPercent"]], 50)
  expect_equal(cleaned[["credits"]][["remaining"]], 100)
})

test_that("sanitize_usage removes PII from all providers and counts stripped fields", {
  raw <- list(
    list(
      provider = "claude",
      usage = list(accountEmail = "a@b.com", accountOrganization = "Org",
                   identity = "id1", loginMethod = "oauth",
                   updatedAt = "2026-01-01T00:00:00Z",
                   primary = list(usedPercent = 10, windowMinutes = 60,
                                  resetsAt = "2026-01-01T01:00:00Z"))
    ),
    list(
      provider = "openai",
      usage = list(accountEmail = "x@y.com", accountOrganization = "OrgX",
                   identity = "id2", loginMethod = "key",
                   updatedAt = "2026-01-01T00:00:00Z")
    )
  )

  result <- sanitize_usage(raw)

  expect_equal(result$n_stripped, 8L)  # 4 PII fields * 2 providers
  expect_null(result$sanitized[[1L]][["usage"]][["accountEmail"]])
  expect_null(result$sanitized[[2L]][["usage"]][["accountOrganization"]])
  expect_equal(result$sanitized[[1L]][["provider"]], "claude")
})

# ---------------------------------------------------------------------------
# 4. PII-leak regression gate — sanitised fixture output is clean
# ---------------------------------------------------------------------------

test_that("sanitised codexbar_usage_fixture contains no PII fields or @ signs", {
  raw_list <- jsonlite::read_json(
    fixture_path("codexbar_usage_raw_fixture.json"),
    simplifyVector = FALSE
  )

  result <- sanitize_usage(raw_list)
  clean_json <- jsonlite::toJSON(result$sanitized, auto_unbox = TRUE, pretty = TRUE)

  # No PII field names
  expect_false(grepl('"accountEmail"',        clean_json, fixed = TRUE),
               label = "sanitised usage must not contain accountEmail field")
  expect_false(grepl('"accountOrganization"', clean_json, fixed = TRUE),
               label = "sanitised usage must not contain accountOrganization field")
  expect_false(grepl('"identity"',            clean_json, fixed = TRUE),
               label = "sanitised usage must not contain identity field")
  expect_false(grepl('"loginMethod"',         clean_json, fixed = TRUE),
               label = "sanitised usage must not contain loginMethod field")

  # No @ signs (email addresses; URL schemas like @ are absent in this fixture JSON)
  expect_false(grepl("@",                     clean_json, fixed = TRUE),
               label = "sanitised usage must not contain @ (email address)")
})

test_that("clean codexbar_usage_fixture passes verify_no_pii_leak without error", {
  clean_text <- paste(
    readLines(fixture_path("codexbar_usage_fixture.json"), warn = FALSE),
    collapse = "\n"
  )

  expect_no_error(
    verify_no_pii_leak(clean_text, "codexbar_usage_fixture.json"),
    message = "Clean fixture must pass verify_no_pii_leak without error"
  )
})

test_that("cost fixture passes verify_no_pii_leak (cost has no PII)", {
  cost_text <- paste(
    readLines(fixture_path("codexbar_cost_fixture.json"), warn = FALSE),
    collapse = "\n"
  )

  expect_no_error(
    verify_no_pii_leak(cost_text, "codexbar_cost_fixture.json"),
    message = "Cost fixture must pass verify_no_pii_leak without error"
  )
})
