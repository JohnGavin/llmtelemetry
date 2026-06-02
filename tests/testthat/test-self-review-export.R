# Tests for self-review findings export (#207)
# Validates the committed JSON snapshot at inst/extdata/self_review_findings.json
# and the sanitisation logic in export_dashboard_data.R section 8z.
#
# Design: validate the committed artefact (not the export script directly) so
# CI can run these tests without a live ~/.claude/logs/unified.duckdb.

extdata_path <- function(filename) {
  system.file("extdata", filename, package = "llmtelemetry")
}

# ── 1. File presence and parse ─────────────────────────────────────────────────

test_that("self_review_findings.json exists in inst/extdata", {
  path <- extdata_path("self_review_findings.json")
  expect_true(nzchar(path), info = "self_review_findings.json not found via system.file()")
})

test_that("self_review_findings.json parses to a data.frame", {
  path <- extdata_path("self_review_findings.json")
  skip_if(!nzchar(path), "self_review_findings.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("self_review_findings.json has required columns", {
  path <- extdata_path("self_review_findings.json")
  skip_if(!nzchar(path), "self_review_findings.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  required_cols <- c("finding_id", "finding_type", "severity", "date",
                     "evidence_summary")
  expect_true(
    all(required_cols %in% names(result)),
    info = paste("Missing columns:", paste(setdiff(required_cols, names(result)), collapse = ", "))
  )
})

test_that("self_review_findings.json has no extraneous columns", {
  path <- extdata_path("self_review_findings.json")
  skip_if(!nzchar(path), "self_review_findings.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expected_cols <- c("finding_id", "finding_type", "severity", "date",
                     "evidence_summary")
  extra <- setdiff(names(result), expected_cols)
  expect_equal(
    length(extra), 0L,
    info = paste("Unexpected columns:", paste(extra, collapse = ", "))
  )
})

# ── 2. Schema validation ───────────────────────────────────────────────────────

test_that("self_review_findings.json date column matches YYYY-MM-DD pattern", {
  path <- extdata_path("self_review_findings.json")
  skip_if(!nzchar(path), "self_review_findings.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  skip_if(nrow(result) == 0L, "no rows in snapshot")
  expect_true(
    all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", result$date)),
    info = "date values should match YYYY-MM-DD format"
  )
})

test_that("self_review_findings.json finding_type values are non-empty strings", {
  path <- extdata_path("self_review_findings.json")
  skip_if(!nzchar(path), "self_review_findings.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  skip_if(nrow(result) == 0L, "no rows in snapshot")
  expect_true(all(nzchar(result$finding_type)))
})

test_that("self_review_findings.json severity values are non-empty strings", {
  path <- extdata_path("self_review_findings.json")
  skip_if(!nzchar(path), "self_review_findings.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  skip_if(nrow(result) == 0L, "no rows in snapshot")
  expect_true(all(nzchar(result$severity)))
})

test_that("self_review_findings.json finding_id values are unique", {
  path <- extdata_path("self_review_findings.json")
  skip_if(!nzchar(path), "self_review_findings.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  skip_if(nrow(result) == 0L, "no rows in snapshot")
  expect_equal(length(unique(result$finding_id)), nrow(result),
               info = "finding_id must be unique per row")
})

test_that("self_review_findings.json evidence_summary is valid JSON per row", {
  path <- extdata_path("self_review_findings.json")
  skip_if(!nzchar(path), "self_review_findings.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  skip_if(nrow(result) == 0L, "no rows in snapshot")
  invalid_rows <- vapply(result$evidence_summary, function(ev) {
    if (is.na(ev) || !nzchar(ev)) return(FALSE)  # empty is acceptable
    tryCatch({
      jsonlite::fromJSON(ev)
      FALSE
    }, error = function(e) TRUE)
  }, logical(1L))
  expect_false(any(invalid_rows),
               info = paste("Invalid JSON in evidence_summary at rows:",
                            paste(which(invalid_rows), collapse = ", ")))
})

# ── 3. Privacy / sanitisation ─────────────────────────────────────────────────

test_that("self_review_findings.json contains no confidential project names", {
  path <- extdata_path("self_review_findings.json")
  skip_if(!nzchar(path), "self_review_findings.json not installed")
  raw_text <- readLines(path, warn = FALSE)
  combined <- paste(raw_text, collapse = " ")

  confidential_patterns <- c(
    "mycare", "crypto", "crypto_solwatch", "crypto_swarms",
    "solwatch", "swarms", "my_t_project", "hello_t", "t_demos"
  )
  for (pat in confidential_patterns) {
    expect_false(
      grepl(pat, combined, ignore.case = TRUE),
      info = paste("Confidential project name found in JSON:", pat)
    )
  }
})

test_that("self_review_findings.json contains no raw session_id column", {
  path <- extdata_path("self_review_findings.json")
  skip_if(!nzchar(path), "self_review_findings.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_false("session_id" %in% names(result),
               info = "session_id should not be exposed in public JSON")
})

test_that("self_review_findings.json contains no filesystem paths", {
  path <- extdata_path("self_review_findings.json")
  skip_if(!nzchar(path), "self_review_findings.json not installed")
  raw_text <- readLines(path, warn = FALSE)
  combined <- paste(raw_text, collapse = " ")
  # Filesystem paths start with / or ~ or contain Users/johngavin
  expect_false(
    grepl("Users/johngavin|/home/|~/.claude", combined),
    info = "Filesystem paths must not appear in the public JSON export"
  )
})

# ── 4. Content sanity ─────────────────────────────────────────────────────────

test_that("self_review_findings.json has at least one row", {
  path <- extdata_path("self_review_findings.json")
  skip_if(!nzchar(path), "self_review_findings.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_gt(nrow(result), 0L,
            label = "self_review_findings.json should have at least one finding")
})

test_that("self_review_findings.json known finding types appear in snapshot", {
  path <- extdata_path("self_review_findings.json")
  skip_if(!nzchar(path), "self_review_findings.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  skip_if(nrow(result) == 0L, "no rows in snapshot")
  # Both finding_type values documented in llm#235 must appear
  expect_true("stuck_loop" %in% result$finding_type,
              info = "stuck_loop finding_type expected in snapshot")
})
