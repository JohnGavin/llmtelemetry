# Helper: write fixture JSONL files into a caller-supplied temp dir
# dir must be created by the caller (e.g. withr::local_tempdir()) inside a test
write_staging_fixture <- function(lines, dir) {
  path <- file.path(dir, "events-2026-01-01.jsonl")
  writeLines(lines, path)
  dir
}

# --- Empty staging directory -------------------------------------------------

test_that("read_staging() returns empty tibble when directory doesn't exist", {
  result <- read_staging(staging_dir = "/tmp/does_not_exist_llmtelemetry_test")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_equal(names(result), c("ts", "host", "pid", "payload"))
})

test_that("read_staging() returns empty tibble for empty directory", {
  staging_dir <- withr::local_tempdir()
  result <- read_staging(staging_dir = staging_dir)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_equal(names(result), c("ts", "host", "pid", "payload"))
})

test_that("read_staging() returns empty tibble for directory with no .jsonl files", {
  staging_dir <- withr::local_tempdir()
  writeLines("not json", file.path(staging_dir, "other.txt"))
  result <- read_staging(staging_dir = staging_dir)

  expect_equal(nrow(result), 0L)
})

# --- Single file, single event -----------------------------------------------

test_that("read_staging() reads one envelope from a single JSONL file", {
  line <- '{"ts":"2026-01-01T10:00:00Z","host":"myhost","pid":"12345","payload":{"event_type":"session_stop"}}'
  staging_dir <- write_staging_fixture(line, withr::local_tempdir())

  result <- read_staging(staging_dir = staging_dir)

  expect_equal(nrow(result), 1L)
  expect_s3_class(result$ts, "POSIXct")
  expect_equal(result$host, "myhost")
  expect_equal(result$pid, "12345")
  expect_equal(result$payload[[1]]$event_type, "session_stop")
})

test_that("read_staging() ts column is POSIXct UTC", {
  line <- '{"ts":"2026-03-15T08:30:00Z","host":"h","pid":"1","payload":{}}'
  staging_dir <- write_staging_fixture(line, withr::local_tempdir())

  result <- read_staging(staging_dir = staging_dir)

  expect_s3_class(result$ts, "POSIXct")
  expect_equal(attr(result$ts, "tzone"), "UTC")
})

# --- Multiple files: rows are combined ----------------------------------------

test_that("read_staging() combines rows from multiple JSONL files", {
  staging_dir <- withr::local_tempdir()

  writeLines(
    '{"ts":"2026-01-01T10:00:00Z","host":"h1","pid":"1","payload":{}}',
    file.path(staging_dir, "events-2026-01-01.jsonl")
  )
  writeLines(
    c(
      '{"ts":"2026-01-02T11:00:00Z","host":"h2","pid":"2","payload":{}}',
      '{"ts":"2026-01-02T12:00:00Z","host":"h2","pid":"3","payload":{}}'
    ),
    file.path(staging_dir, "events-2026-01-02.jsonl")
  )

  result <- read_staging(staging_dir = staging_dir)

  expect_equal(nrow(result), 3L)
})

# --- Invalid JSON lines are skipped ------------------------------------------

test_that("read_staging() skips invalid JSON lines without error", {
  lines <- c(
    '{"ts":"2026-01-01T10:00:00Z","host":"h","pid":"1","payload":{}}',
    "this is not json at all",
    '{"ts":"2026-01-01T11:00:00Z","host":"h","pid":"2","payload":{}}'
  )
  staging_dir <- write_staging_fixture(lines, withr::local_tempdir())

  result <- read_staging(staging_dir = staging_dir)

  # Only 2 valid lines survive
  expect_equal(nrow(result), 2L)
})

# --- Empty lines in JSONL are handled -----------------------------------------

test_that("read_staging() handles empty lines in JSONL gracefully", {
  lines <- c(
    '{"ts":"2026-01-01T10:00:00Z","host":"h","pid":"1","payload":{}}',
    "",
    '{"ts":"2026-01-01T11:00:00Z","host":"h","pid":"2","payload":{}}'
  )
  staging_dir <- write_staging_fixture(lines, withr::local_tempdir())

  result <- read_staging(staging_dir = staging_dir)

  expect_equal(nrow(result), 2L)
})

# --- Column types and structure -----------------------------------------------

test_that("read_staging() returns tibble with correct column types", {
  line <- '{"ts":"2026-01-01T10:00:00Z","host":"testhost","pid":"99","payload":{"k":"v"}}'
  staging_dir <- write_staging_fixture(line, withr::local_tempdir())

  result <- read_staging(staging_dir = staging_dir)

  expect_s3_class(result, "tbl_df")
  expect_type(result$host, "character")
  expect_type(result$pid, "character")
  expect_type(result$payload, "list")
})
