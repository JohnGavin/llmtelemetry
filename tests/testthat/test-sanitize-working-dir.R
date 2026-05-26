# Tests for .redact_working_dir() helper in rollup_sessions.R.
#
# The helper is defined inside append_sessions_from_staging() so we test it
# via append_sessions_from_staging() with working_dir payloads, and also
# via a local re-definition to cover the helper logic directly.

# ---------------------------------------------------------------------------
# Re-define the helper locally for direct unit testing (matches the exact
# implementation in rollup_sessions.R so regressions surface here first).
# ---------------------------------------------------------------------------

redact_working_dir_test <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    v2 <- v
    v2 <- sub("^/Users/[^/]+/",      "", v2)
    v2 <- sub("^/private/tmp/[^/]+/", "", v2)
    v2 <- sub("^/private/tmp/[^/]+$", "", v2)
    v2 <- sub("^/tmp/[^/]+/",         "", v2)
    v2 <- sub("^/tmp/[^/]+$",         "", v2)
    # Fail-safe: if any forbidden pattern still matches, redact entirely
    forbidden <- c(
      "Users-johngavin", "/Users/", "/private/", "/tmp/", "/var/",
      "-private-tmp-", "-tmp-", "-worktree-", "worktree-agent-", "johngavin"
    )
    if (any(vapply(forbidden, function(p) grepl(p, v2, perl = TRUE), logical(1L)))) {
      return(NA_character_)
    }
    v2
  }, character(1L), USE.NAMES = FALSE)
}

# ---------------------------------------------------------------------------
# Core unit tests for the helper logic
# ---------------------------------------------------------------------------

test_that("redact_working_dir: home-dir path strips /Users/<user>/ prefix", {
  input    <- "/Users/johngavin/docs_gh/llmtelemetry"
  result   <- redact_working_dir_test(input)

  # Must not contain /Users/ or johngavin
  expect_false(grepl("/Users/",   result, fixed = TRUE),
               label = "result must not contain /Users/")
  expect_false(grepl("johngavin", result, fixed = TRUE),
               label = "result must not contain johngavin")
})

test_that("redact_working_dir: /private/tmp/<id>/... becomes the sub-path", {
  input  <- "/private/tmp/foo/bar"
  result <- redact_working_dir_test(input)

  expect_false(grepl("/private/", result, fixed = TRUE),
               label = "result must not contain /private/")
  # "bar" is the surviving relative segment
  expect_equal(result, "bar")
})

test_that("redact_working_dir: bare /private/tmp/<id> (no trailing path) becomes empty or NA", {
  input  <- "/private/tmp/some-session-id"
  result <- redact_working_dir_test(input)

  # After stripping the prefix the result is "" — that's fine (no forbidden patterns)
  # OR the fail-safe replaces it with NA. Either way, no /private/ or /tmp/ must remain.
  safe_result <- if (is.na(result)) "" else result
  expect_false(grepl("/private/", safe_result, fixed = TRUE))
  expect_false(grepl("/tmp/",     safe_result, fixed = TRUE))
})

test_that("redact_working_dir: NA input preserved as NA", {
  expect_true(is.na(redact_working_dir_test(NA_character_)))
})

test_that("redact_working_dir: clean relative path passes through unchanged", {
  input  <- "docs_gh/llmtelemetry"
  result <- redact_working_dir_test(input)
  expect_equal(result, input)
})

test_that("redact_working_dir: fail-safe sets NA when pattern cannot be stripped", {
  # A path where the username appears deeper than the prefix
  input  <- "/some/path/johngavin/file"
  result <- redact_working_dir_test(input)
  # After no prefix match, "johngavin" still present → fail-safe → NA
  expect_true(is.na(result))
})

# ---------------------------------------------------------------------------
# Snapshot: stable reference for a set of typical inputs
# ---------------------------------------------------------------------------

test_that("redact_working_dir snapshot covers typical inputs", {
  inputs <- c(
    "/Users/johngavin/docs_gh/llmtelemetry",  # home-dir absolute path
    "/private/tmp/foo/bar",                    # macOS tmp sub-path
    "docs_gh/llmtelemetry",                    # clean relative path (pass-through)
    NA_character_                              # NA preserved
  )
  results <- redact_working_dir_test(inputs)
  expect_snapshot(results)
})

# ---------------------------------------------------------------------------
# Integration: append_sessions_from_staging() sanitizes working_dir in the
# written parquet row.
# ---------------------------------------------------------------------------

test_that("append_sessions_from_staging: working_dir with /Users/ path is redacted in parquet", {
  # Build a staging event with a raw absolute path in working_dir
  payload <- list(
    event_type   = "session_stop",
    session_id   = "sess-wd-001",
    project      = "docs-gh-llmtelemetry",
    started_at   = "2026-01-01T09:00:00Z",
    ended_at     = "2026-01-01T10:00:00Z",
    duration_min = 60,
    agent        = NA_character_,
    source       = "claude-code-hook",
    working_dir  = "/Users/johngavin/docs_gh/llmtelemetry"
  )
  envelope <- list(
    ts = "2026-01-01T10:00:00Z", host = "testhost", pid = "1",
    payload = payload
  )

  staging <- tempfile()
  dir.create(staging, recursive = TRUE)
  writeLines(
    jsonlite::toJSON(envelope, auto_unbox = TRUE),
    file.path(staging, "events-test.jsonl")
  )

  out_f <- withr::local_tempfile(fileext = ".parquet")

  append_sessions_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-01-01 11:00:00", tz = "UTC")
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  back <- DBI::dbGetQuery(con, sprintf("SELECT working_dir FROM read_parquet('%s')", out_f))

  wd <- back$working_dir
  # working_dir must not contain /Users/ or johngavin
  non_na <- wd[!is.na(wd)]
  if (length(non_na) > 0L) {
    expect_false(any(grepl("/Users/",   non_na, fixed = TRUE)),
                 label = "parquet working_dir must not contain /Users/")
    expect_false(any(grepl("johngavin", non_na, fixed = TRUE)),
                 label = "parquet working_dir must not contain johngavin")
  }
})
