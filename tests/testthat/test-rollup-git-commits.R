# Helper: write a minimal fixture JSON and return its path
make_git_commits_fixture_json <- function(n = 5L, dir = tempdir()) {
  df <- data.frame(
    project           = rep("test_proj", n),
    hash              = paste0("abc", sprintf("%04d", seq_len(n))),
    date              = format(as.Date("2026-01-01") + seq_len(n) - 1L),
    message           = paste0("commit message ", seq_len(n)),
    lines_added       = seq_len(n) * 10L,
    lines_deleted     = seq_len(n) * 2L,
    files_changed     = seq_len(n),
    lines_changed     = seq_len(n) * 12L,
    canonical_project = rep("test_proj", n),
    stringsAsFactors  = FALSE
  )
  path <- file.path(dir, paste0("fixture_git_commits_", n, ".json"))
  jsonlite::write_json(df, path, auto_unbox = FALSE)
  path
}

# --- Basic correctness --------------------------------------------------------

test_that("rollup_git_commits() produces a data frame with the expected row count", {
  n       <- 7L
  fixture <- make_git_commits_fixture_json(n)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")

  result <- rollup_git_commits(
    input_path  = fixture,
    output_path = out_f,
    now         = fixed_t
  )

  expect_equal(nrow(result), n)
})

test_that("rollup_git_commits() writes a readable parquet file to the output path", {
  fixture <- make_git_commits_fixture_json(3L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  rollup_git_commits(input_path = fixture, output_path = out_f,
                     now = as.POSIXct("2026-01-01", tz = "UTC"))

  expect_true(file.exists(out_f))
  expect_gt(file.size(out_f), 0L)
})

# --- Column completeness ------------------------------------------------------

test_that("parquet has exactly the v1 column set — no extra, no missing", {
  fixture <- make_git_commits_fixture_json(4L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  rollup_git_commits(input_path = fixture, output_path = out_f,
                     now = as.POSIXct("2026-01-01", tz = "UTC"))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  back <- DBI::dbGetQuery(con, sprintf("SELECT * FROM '%s' LIMIT 1", out_f))

  expected_cols <- c(
    "commit_pk", "project", "canonical_project", "hash", "date",
    "message", "lines_added", "lines_deleted", "files_changed",
    "lines_changed", "valid_from"
  )
  expect_equal(sort(names(back)), sort(expected_cols))
})

# --- valid_from populated -----------------------------------------------------

test_that("valid_from is populated and equals the 'now' argument", {
  fixture <- make_git_commits_fixture_json(2L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-03-15 12:30:00", tz = "UTC")

  result <- rollup_git_commits(
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
  fixture <- make_git_commits_fixture_json(n)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_git_commits(input_path = fixture, output_path = out_f,
                               now = as.POSIXct("2026-01-01", tz = "UTC"))

  expect_equal(unique(result$canonical_project), "test_proj")
})

# --- commit_pk uniqueness -----------------------------------------------------

test_that("commit_pk is unique across all rows", {
  fixture <- make_git_commits_fixture_json(8L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_git_commits(input_path = fixture, output_path = out_f,
                               now = as.POSIXct("2026-01-01", tz = "UTC"))

  expect_equal(length(unique(result$commit_pk)), nrow(result))
})

# --- commit_pk composite key format ------------------------------------------

test_that("commit_pk follows canonical_project|hash format", {
  fixture <- make_git_commits_fixture_json(2L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_git_commits(input_path = fixture, output_path = out_f,
                               now = as.POSIXct("2026-01-01", tz = "UTC"))

  # Each commit_pk should be "canonical_project|hash"
  expected_pks <- paste(result$canonical_project, result$hash, sep = "|")
  expect_equal(result$commit_pk, expected_pks)
})

# --- Round-trip: write -> read -> equals -------------------------------------

test_that("round-trip: parquet read back matches the returned data frame", {
  fixture <- make_git_commits_fixture_json(5L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-06-01 00:00:00", tz = "UTC")

  result <- rollup_git_commits(
    input_path  = fixture,
    output_path = out_f,
    now         = fixed_t
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  back <- DBI::dbGetQuery(con, sprintf("SELECT * FROM '%s' ORDER BY commit_pk", out_f))

  result_ordered <- result[order(result$commit_pk), ]
  row.names(result_ordered) <- NULL
  row.names(back)           <- NULL

  expect_equal(back$commit_pk, result_ordered$commit_pk)
  expect_equal(nrow(back), nrow(result_ordered))
  expect_equal(back$hash, result_ordered$hash)
})

# --- Error on empty input -----------------------------------------------------

test_that("rollup_git_commits() errors on empty input data", {
  empty_json <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(data.frame(), empty_json)

  out_f <- withr::local_tempfile(fileext = ".parquet")

  expect_error(
    rollup_git_commits(input_path = empty_json, output_path = out_f),
    regexp = "rollup_git_commits: input has no rows"
  )
})

# --- Idempotency --------------------------------------------------------------

test_that("running rollup_git_commits() twice yields same row count", {
  fixture <- make_git_commits_fixture_json(6L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-01-01", tz = "UTC")

  r1 <- rollup_git_commits(input_path = fixture, output_path = out_f, now = fixed_t)
  r2 <- rollup_git_commits(input_path = fixture, output_path = out_f, now = fixed_t)

  expect_equal(nrow(r1), nrow(r2))
  expect_equal(r1$commit_pk, r2$commit_pk)
})
