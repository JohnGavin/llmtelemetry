# Helper: write a minimal fixture JSON and return its path
make_costs_fixture_json <- function(n = 5L, dir = tempdir()) {
  df <- data.frame(
    date              = format(as.Date("2026-01-01") + seq_len(n) - 1L),
    project           = paste0("proj_", seq_len(n)),
    est_cost          = seq_len(n) * 10.5,
    duration_min      = seq_len(n) * 60.0,
    share             = rep(1.0, n),
    canonical_project = paste0("proj_", seq_len(n)),
    stringsAsFactors  = FALSE
  )
  path <- file.path(dir, paste0("fixture_costs_", n, ".json"))
  jsonlite::write_json(df, path, auto_unbox = FALSE)
  path
}

# --- Basic correctness --------------------------------------------------------

test_that("rollup_costs() produces a data frame with the expected row count", {
  n       <- 6L
  fixture <- make_costs_fixture_json(n)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")

  result <- rollup_costs(
    input_path  = fixture,
    output_path = out_f,
    now         = fixed_t
  )

  expect_equal(nrow(result), n)
})

test_that("rollup_costs() writes a readable parquet file to the output path", {
  fixture <- make_costs_fixture_json(3L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  rollup_costs(input_path = fixture, output_path = out_f,
               now = as.POSIXct("2026-01-01", tz = "UTC"))

  expect_true(file.exists(out_f))
  expect_gt(file.size(out_f), 0L)
})

# --- Column completeness ------------------------------------------------------

test_that("parquet has exactly the v1 column set — no extra, no missing", {
  fixture <- make_costs_fixture_json(4L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  rollup_costs(input_path = fixture, output_path = out_f,
               now = as.POSIXct("2026-01-01", tz = "UTC"))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  back <- DBI::dbGetQuery(con, sprintf("SELECT * FROM '%s' LIMIT 1", out_f))

  expected_cols <- c(
    "cost_id", "project", "canonical_project", "date",
    "source", "daily_cost_usd", "n_sessions", "duration_min", "valid_from"
  )
  expect_equal(sort(names(back)), sort(expected_cols))
})

# --- valid_from populated -----------------------------------------------------

test_that("valid_from is populated and equals the 'now' argument", {
  fixture <- make_costs_fixture_json(2L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-03-15 12:30:00", tz = "UTC")

  result <- rollup_costs(
    input_path  = fixture,
    output_path = out_f,
    now         = fixed_t
  )

  expect_false(anyNA(result$valid_from))
  expect_s3_class(result$valid_from, "POSIXct")
  expect_true(all(abs(as.numeric(result$valid_from) - as.numeric(fixed_t)) < 1))
})

# --- canonical_project preserved ---------------------------------------------

test_that("canonical_project is preserved from input", {
  n       <- 3L
  fixture <- make_costs_fixture_json(n)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_costs(input_path = fixture, output_path = out_f,
                         now = as.POSIXct("2026-01-01", tz = "UTC"))

  expect_equal(sort(unique(result$canonical_project)),
               sort(paste0("proj_", seq_len(n))))
})

# --- source field -------------------------------------------------------------

test_that("source column is set to 'estimated'", {
  fixture <- make_costs_fixture_json(2L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_costs(input_path = fixture, output_path = out_f,
                         now = as.POSIXct("2026-01-01", tz = "UTC"))

  expect_true(all(result$source == "estimated"))
})

# --- cost_id uniqueness -------------------------------------------------------

test_that("cost_id is unique across all rows", {
  fixture <- make_costs_fixture_json(8L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_costs(input_path = fixture, output_path = out_f,
                         now = as.POSIXct("2026-01-01", tz = "UTC"))

  expect_equal(length(unique(result$cost_id)), nrow(result))
})

# --- Round-trip: write -> read -> equals -------------------------------------

test_that("round-trip: parquet read back matches the returned data frame", {
  fixture <- make_costs_fixture_json(5L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-06-01 00:00:00", tz = "UTC")

  result <- rollup_costs(
    input_path  = fixture,
    output_path = out_f,
    now         = fixed_t
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  back <- DBI::dbGetQuery(con, sprintf("SELECT * FROM '%s' ORDER BY cost_id", out_f))

  result_ordered <- result[order(result$cost_id), ]
  row.names(result_ordered) <- NULL
  row.names(back)           <- NULL

  expect_equal(back$cost_id, result_ordered$cost_id)
  expect_equal(nrow(back), nrow(result_ordered))
  expect_equal(back$source, result_ordered$source)
})

# --- Error on empty input -----------------------------------------------------

test_that("rollup_costs() errors on empty input data", {
  empty_json <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(data.frame(), empty_json)

  out_f <- withr::local_tempfile(fileext = ".parquet")

  expect_error(
    rollup_costs(input_path = empty_json, output_path = out_f),
    regexp = "rollup_costs: input has no rows"
  )
})

# --- Idempotency --------------------------------------------------------------

test_that("running rollup_costs() twice yields same row count", {
  fixture <- make_costs_fixture_json(6L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-01-01", tz = "UTC")

  r1 <- rollup_costs(input_path = fixture, output_path = out_f, now = fixed_t)
  r2 <- rollup_costs(input_path = fixture, output_path = out_f, now = fixed_t)

  expect_equal(nrow(r1), nrow(r2))
  expect_equal(r1$cost_id, r2$cost_id)
})
