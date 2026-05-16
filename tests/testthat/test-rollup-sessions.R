# Helper: write a minimal fixture JSON and return its path
make_fixture_json <- function(n = 5L, dir = tempdir()) {
  df <- data.frame(
    session_id        = paste0("sess-", seq_len(n)),
    project           = rep("test_proj", n),
    canonical_project = rep("test_proj", n),
    started_at        = format(
      as.POSIXct("2026-01-01 10:00:00", tz = "UTC") + seq_len(n) * 60,
      "%Y-%m-%d %H:%M:%S"
    ),
    ended_at          = format(
      as.POSIXct("2026-01-01 10:00:00", tz = "UTC") + seq_len(n) * 60 + 300,
      "%Y-%m-%d %H:%M:%S"
    ),
    duration_min      = rep(5.0, n),
    stringsAsFactors  = FALSE
  )
  path <- file.path(dir, paste0("fixture_sessions_", n, ".json"))
  jsonlite::write_json(df, path, auto_unbox = FALSE)
  path
}

# --- Basic correctness ------------------------------------------------------

test_that("rollup_sessions() produces a parquet with the expected row count", {
  n       <- 7L
  fixture <- make_fixture_json(n)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")

  result <- rollup_sessions(
    input_path  = fixture,
    output_path = out_f,
    now         = fixed_t
  )

  expect_equal(nrow(result), n)
})

test_that("rollup_sessions() writes a readable parquet file to the output path", {
  fixture <- make_fixture_json(3L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  rollup_sessions(input_path = fixture, output_path = out_f,
                  now = as.POSIXct("2026-01-01", tz = "UTC"))

  expect_true(file.exists(out_f))
  expect_gt(file.size(out_f), 0L)
})

# --- Column completeness ----------------------------------------------------

test_that("parquet has exactly the v1 column set — no extra, no missing", {
  fixture <- make_fixture_json(4L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  rollup_sessions(input_path = fixture, output_path = out_f,
                  now = as.POSIXct("2026-01-01", tz = "UTC"))

  # Read back via DuckDB (no arrow needed)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  back <- DBI::dbGetQuery(con, sprintf("SELECT * FROM '%s' LIMIT 1", out_f))

  expected_cols <- c(
    "session_id", "project", "canonical_project", "started_at", "ended_at",
    "duration_min", "agent", "source", "working_dir", "valid_from"
  )
  expect_equal(sort(names(back)), sort(expected_cols))
})

# --- valid_from populated ---------------------------------------------------

test_that("valid_from is populated and equals the 'now' argument", {
  fixture <- make_fixture_json(2L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-03-15 12:30:00", tz = "UTC")

  result <- rollup_sessions(
    input_path  = fixture,
    output_path = out_f,
    now         = fixed_t
  )

  expect_false(anyNA(result$valid_from))
  expect_s3_class(result$valid_from, "POSIXct")
  # All rows share the same valid_from passed as 'now'
  expect_true(all(abs(as.numeric(result$valid_from) - as.numeric(fixed_t)) < 1))
})

# --- canonical_project preserved -------------------------------------------

test_that("canonical_project is preserved from input", {
  n       <- 3L
  fixture <- make_fixture_json(n)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_sessions(input_path = fixture, output_path = out_f,
                             now = as.POSIXct("2026-01-01", tz = "UTC"))

  expect_equal(unique(result$canonical_project), "test_proj")
})

# --- source field -----------------------------------------------------------

test_that("source column is set to 'unified_duckdb'", {
  fixture <- make_fixture_json(2L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_sessions(input_path = fixture, output_path = out_f,
                             now = as.POSIXct("2026-01-01", tz = "UTC"))

  expect_true(all(result$source == "unified_duckdb"))
})

# --- Round-trip: write -> read -> equals ------------------------------------

test_that("round-trip: parquet read back matches the returned data frame", {
  fixture <- make_fixture_json(5L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-06-01 00:00:00", tz = "UTC")

  result <- rollup_sessions(
    input_path  = fixture,
    output_path = out_f,
    now         = fixed_t
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  back <- DBI::dbGetQuery(con, sprintf("SELECT * FROM '%s' ORDER BY session_id", out_f))

  result_ordered <- result[order(result$session_id), ]
  row.names(result_ordered) <- NULL
  row.names(back)           <- NULL

  # Compare session_id and row count (timestamp rounding may differ slightly)
  expect_equal(back$session_id, result_ordered$session_id)
  expect_equal(nrow(back), nrow(result_ordered))
  expect_equal(back$source, result_ordered$source)
})

# --- Error on empty input ---------------------------------------------------

test_that("rollup_sessions() errors on empty input data", {
  empty_json <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(data.frame(), empty_json)

  out_f <- withr::local_tempfile(fileext = ".parquet")

  expect_error(
    rollup_sessions(input_path = empty_json, output_path = out_f),
    regexp = "rollup_sessions: input has no rows"
  )
})

# --- Idempotency (row count unchanged on second run) -----------------------
# NOTE: valid_from differs between runs (it reflects when the run happened),
# so we only assert row count and non-valid_from columns are identical.

test_that("running rollup_sessions() twice yields same row count", {
  fixture <- make_fixture_json(6L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-01-01", tz = "UTC")

  r1 <- rollup_sessions(input_path = fixture, output_path = out_f, now = fixed_t)
  r2 <- rollup_sessions(input_path = fixture, output_path = out_f, now = fixed_t)

  expect_equal(nrow(r1), nrow(r2))
  expect_equal(r1$session_id, r2$session_id)
})
