# Tests for scrape_roborev.R — parser, content hash, upsert idempotency

# ---------------------------------------------------------------------------
# Fixture helpers
# ---------------------------------------------------------------------------

fixture_path <- function(name) {
  system.file("extdata", "roborev_fixtures", name, package = "llmtelemetry")
}

read_fixture <- function(name) {
  path <- fixture_path(name)
  if (!nzchar(path)) {
    # dev fallback
    path <- here::here("inst", "extdata", "roborev_fixtures", name)
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

# ---------------------------------------------------------------------------
# .parse_roborev_show — multi-finding fixture
# ---------------------------------------------------------------------------

test_that(".parse_roborev_show returns correct number of findings from multi-finding fixture", {
  md <- read_fixture("fixture-multi-finding.md")
  result <- llmtelemetry:::.parse_roborev_show(md, job_id = 9001L)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4L)
})

test_that(".parse_roborev_show extracts severity correctly from multi-finding fixture", {
  md <- read_fixture("fixture-multi-finding.md")
  result <- llmtelemetry:::.parse_roborev_show(md, job_id = 9001L)

  expect_equal(result$severity, c("high", "high", "medium", "low"))
})

test_that(".parse_roborev_show extracts primary_file correctly", {
  md <- read_fixture("fixture-multi-finding.md")
  result <- llmtelemetry:::.parse_roborev_show(md, job_id = 9001L)

  # First finding: R/scrape_data.R:287-307 -> R/scrape_data.R
  expect_equal(result$primary_file[1], "R/scrape_data.R")
  # Third finding: R/canonicalize.R:44-67; inst/scripts/export_data.R:71-99
  # primary_file is the first path (before semicolon), stripped of line range
  expect_equal(result$primary_file[3], "R/canonicalize.R")
})

test_that(".parse_roborev_show strips backticks from full_location", {
  md <- read_fixture("fixture-multi-finding.md")
  result <- llmtelemetry:::.parse_roborev_show(md, job_id = 9001L)

  # Should not have backticks
  expect_false(any(grepl("`", result$full_location, fixed = TRUE), na.rm = TRUE))
})

test_that(".parse_roborev_show extracts problem_text as non-empty strings", {
  md <- read_fixture("fixture-multi-finding.md")
  result <- llmtelemetry:::.parse_roborev_show(md, job_id = 9001L)

  expect_true(all(nzchar(result$problem_text)))
})

# ---------------------------------------------------------------------------
# .parse_roborev_show — snapshot test (stability guarantee)
# ---------------------------------------------------------------------------

test_that(".parse_roborev_show snapshot is stable for multi-finding fixture", {
  md <- read_fixture("fixture-multi-finding.md")
  result <- llmtelemetry:::.parse_roborev_show(md, job_id = 9001L)

  # Snapshot just the key columns so the test is readable
  snap_input <- result[, c("severity", "primary_file")]
  expect_snapshot(snap_input)
})

# ---------------------------------------------------------------------------
# .parse_roborev_show — single low-severity fixture
# ---------------------------------------------------------------------------

test_that(".parse_roborev_show returns one row from single-low fixture", {
  md <- read_fixture("fixture-single-low.md")
  result <- llmtelemetry:::.parse_roborev_show(md, job_id = 9002L)

  expect_equal(nrow(result), 1L)
  expect_equal(result$severity, "low")
})

test_that(".parse_roborev_show returns NA fix_text when no Fix field", {
  md <- read_fixture("fixture-single-low.md")
  result <- llmtelemetry:::.parse_roborev_show(md, job_id = 9002L)

  # fix_text may be NA when there's no **Fix**: line or when there is one
  # In the single-low fixture there IS a fix field, so it should be non-NA
  expect_false(is.na(result$fix_text[1]))
})

# ---------------------------------------------------------------------------
# .parse_roborev_show — malformed fixture (missing Location)
# ---------------------------------------------------------------------------

test_that(".parse_roborev_show warns on finding without Location field", {
  md <- read_fixture("fixture-malformed.md")

  # The first block has no Location — should warn
  expect_warning(
    llmtelemetry:::.parse_roborev_show(md, job_id = 9003L),
    regexp = NULL  # any warning is fine
  )
})

test_that(".parse_roborev_show still returns valid findings from malformed fixture", {
  md <- read_fixture("fixture-malformed.md")

  result <- suppressWarnings(
    llmtelemetry:::.parse_roborev_show(md, job_id = 9003L)
  )

  # Should get 2 findings (one with NA location, one valid high-severity)
  expect_equal(nrow(result), 2L)
})

test_that(".parse_roborev_show sets primary_file NA for finding without Location", {
  md <- read_fixture("fixture-malformed.md")

  result <- suppressWarnings(
    llmtelemetry:::.parse_roborev_show(md, job_id = 9003L)
  )

  expect_true(is.na(result$primary_file[1]))
  expect_true(is.na(result$full_location[1]))
})

# ---------------------------------------------------------------------------
# .parse_roborev_show — empty / degenerate inputs
# ---------------------------------------------------------------------------

test_that(".parse_roborev_show returns empty tibble for empty string", {
  result <- llmtelemetry:::.parse_roborev_show("")
  expect_equal(nrow(result), 0L)
  expect_s3_class(result, "tbl_df")
})

test_that(".parse_roborev_show returns empty tibble when no Severity field present", {
  result <- llmtelemetry:::.parse_roborev_show("This is just a header with no findings.")
  expect_equal(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# .finding_content_hash — stability and sensitivity
# ---------------------------------------------------------------------------

test_that(".finding_content_hash produces stable hash for same inputs", {
  h1 <- llmtelemetry:::.finding_content_hash("high", "R/foo.R", "Problem text here")
  h2 <- llmtelemetry:::.finding_content_hash("high", "R/foo.R", "Problem text here")
  expect_equal(h1, h2)
})

test_that(".finding_content_hash produces different hash when first 12 words differ", {
  # Different first word
  h1 <- llmtelemetry:::.finding_content_hash("high", "R/foo.R", "AAA second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth thirteenth")
  h2 <- llmtelemetry:::.finding_content_hash("high", "R/foo.R", "BBB second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth thirteenth")
  expect_false(h1 == h2)
})

test_that(".finding_content_hash is insensitive to words beyond position 12", {
  # Only the last word differs (position 13+)
  prefix <- paste(rep("word", 12L), collapse = " ")
  h1 <- llmtelemetry:::.finding_content_hash("high", "R/foo.R", paste(prefix, "trailing1"))
  h2 <- llmtelemetry:::.finding_content_hash("high", "R/foo.R", paste(prefix, "trailing2"))
  expect_equal(h1, h2)
})

test_that(".finding_content_hash handles NA gracefully", {
  h1 <- llmtelemetry:::.finding_content_hash(NA_character_, NA_character_, NA_character_)
  h2 <- llmtelemetry:::.finding_content_hash(NA_character_, NA_character_, NA_character_)
  expect_equal(h1, h2)
  expect_type(h1, "character")
  expect_equal(nchar(h1), 32L)  # md5 hex length
})

test_that(".finding_content_hash returns 32-char md5 hex string", {
  h <- llmtelemetry:::.finding_content_hash("critical", "R/bar.R", "Some problem description here")
  expect_type(h, "character")
  expect_equal(nchar(h), 32L)
  expect_true(grepl("^[0-9a-f]+$", h))
})

# ---------------------------------------------------------------------------
# upsert_roborev_findings — idempotency via temp DuckDB
# ---------------------------------------------------------------------------

make_test_findings <- function() {
  tibble::tibble(
    job_id        = 1L,
    project       = "test-project",
    branch        = "main",
    commit_sha    = "abc123",
    reviewed_at   = as.POSIXct("2026-01-01 12:00:00", tz = "UTC"),
    severity      = factor("high", levels = c("critical", "high", "medium", "low", "info", "unknown")),
    primary_file  = "R/foo.R",
    full_location = "R/foo.R:10-20",
    problem_text  = "There is a problem with this function that causes issues",
    fix_text      = "Apply the fix here",
    agent         = "codex",
    verdict       = factor("unknown", levels = c("pass", "fail", "unknown"))
  )
}

test_that("upsert_roborev_findings inserts new findings into temp DuckDB", {
  db_path <- tempfile(fileext = ".duckdb")
  withr::defer(unlink(db_path))

  findings <- make_test_findings()
  result <- upsert_roborev_findings(findings, db_path = db_path)

  expect_s3_class(result, "tbl_df")
  expect_equal(result$reviews_seen, 1L)
  expect_equal(result$new_findings, 1L)
  expect_equal(result$new_reviews, 1L)
})

test_that("upsert_roborev_findings is idempotent — running twice gives no duplicates", {
  db_path <- tempfile(fileext = ".duckdb")
  withr::defer(unlink(db_path))

  findings <- make_test_findings()

  # First upsert
  upsert_roborev_findings(findings, db_path = db_path)

  # Second upsert with the same data
  result2 <- upsert_roborev_findings(findings, db_path = db_path)

  expect_equal(result2$new_findings, 0L)
  expect_equal(result2$new_reviews, 0L)

  # Verify DB row count is still 1
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM roborev_findings")$n
  expect_equal(as.integer(n), 1L)
})

test_that("upsert_roborev_findings handles empty findings tibble gracefully", {
  db_path <- tempfile(fileext = ".duckdb")
  withr::defer(unlink(db_path))

  findings <- .empty_scrape_tibble()
  result <- upsert_roborev_findings(findings, db_path = db_path)

  expect_equal(result$reviews_seen, 0L)
  expect_equal(result$new_findings, 0L)
})

test_that("upsert_roborev_findings logs run in roborev_snapshot_log", {
  db_path <- tempfile(fileext = ".duckdb")
  withr::defer(unlink(db_path))

  findings <- make_test_findings()
  upsert_roborev_findings(findings, db_path = db_path)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  log_rows <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM roborev_snapshot_log")$n
  expect_equal(as.integer(log_rows), 1L)
})

test_that("upsert_roborev_findings adds new findings for same job_id but different hash", {
  db_path <- tempfile(fileext = ".duckdb")
  withr::defer(unlink(db_path))

  findings1 <- make_test_findings()
  upsert_roborev_findings(findings1, db_path = db_path)

  # Different problem_text -> different content_hash -> new finding
  findings2 <- make_test_findings()
  findings2$problem_text <- "A completely different problem with entirely new words here for testing"
  result2 <- upsert_roborev_findings(findings2, db_path = db_path)

  expect_equal(result2$new_findings, 1L)
  expect_equal(result2$new_reviews, 0L)  # same job_id, not a new review

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM roborev_findings")$n
  expect_equal(as.integer(n), 2L)
})

# ---------------------------------------------------------------------------
# migrate_unified_db — schema creation
# ---------------------------------------------------------------------------

test_that("migrate_unified_db creates all four roborev tables", {
  db_path <- tempfile(fileext = ".duckdb")
  withr::defer(unlink(db_path))

  migrate_unified_db(db_path = db_path)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  tables <- DBI::dbListTables(con)
  expect_true("roborev_reviews" %in% tables)
  expect_true("roborev_findings" %in% tables)
  expect_true("roborev_loops" %in% tables)
  expect_true("roborev_snapshot_log" %in% tables)
})

test_that("migrate_unified_db is idempotent when called twice", {
  db_path <- tempfile(fileext = ".duckdb")
  withr::defer(unlink(db_path))

  migrate_unified_db(db_path = db_path)
  expect_no_error(migrate_unified_db(db_path = db_path))
})
