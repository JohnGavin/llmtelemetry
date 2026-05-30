# Tests for parse_review_locations() in R/parse_roborev_output.R
# Snapshot tests disabled (osv — use structural assertions throughout).
# Test cases: (a) 0 Location lines, (b) single Location, (c) multiple, (d) no line number.

# ---------------------------------------------------------------------------
# (a) Review with 0 Location lines → empty tibble
# ---------------------------------------------------------------------------

test_that("parse_review_locations returns empty tibble when no Location lines", {
  txt <- paste0(
    "- **Severity**: High\n",
    "- **Problem**: Something is wrong\n",
    "- **Fix**: Do something\n"
  )
  result <- parse_review_locations(txt, review_id = 1L, severity = "High", repo = "llm")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("review_id", "file_path", "line", "severity", "repo"))
})

test_that("parse_review_locations returns empty tibble for NA input", {
  result <- parse_review_locations(NA_character_, review_id = 1L, severity = "High", repo = "llm")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("parse_review_locations returns empty tibble for empty string input", {
  result <- parse_review_locations("", review_id = 1L, severity = "High", repo = "llm")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# (b) Review with single Location line → one-row tibble
# ---------------------------------------------------------------------------

test_that("parse_review_locations returns one row for single Location line", {
  txt <- paste0(
    "- **Severity**: High\n",
    "- **Location**: R/foo.R:42\n",
    "- **Problem**: Something is wrong\n"
  )
  result <- parse_review_locations(txt, review_id = 10L, severity = "High", repo = "llmtelemetry")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_equal(result$review_id, 10L)
  expect_equal(result$file_path, "R/foo.R")
  expect_equal(result$line, 42L)
  expect_equal(result$severity, "High")
  expect_equal(result$repo, "llmtelemetry")
})

test_that("parse_review_locations handles backtick-wrapped path in single Location", {
  txt <- "- **Location**: `inst/scripts/export.R:120`\n"
  result <- parse_review_locations(txt, review_id = 2L, severity = "Medium", repo = "llm")

  expect_equal(nrow(result), 1L)
  expect_equal(result$file_path, "inst/scripts/export.R")
  expect_equal(result$line, 120L)
})

# ---------------------------------------------------------------------------
# (c) Review with multiple Location lines → multi-row tibble
# ---------------------------------------------------------------------------

test_that("parse_review_locations returns multiple rows for multiple Location lines", {
  txt <- paste0(
    "- **Severity**: Medium\n",
    "- **Location**: R/canonicalize.R:44-67\n",
    "- **Location**: inst/scripts/export_data.R:71-99\n",
    "- **Problem**: Some issue\n"
  )
  result <- parse_review_locations(txt, review_id = 5L, severity = "Medium", repo = "llm")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2L)
  expect_equal(result$file_path, c("R/canonicalize.R", "inst/scripts/export_data.R"))
  # Line range "44-67" — line is NA because it's a range, not a single int
  # (the regex captures single digits only; ranges are stripped as part of path cleanup)
  expect_true(all(is.na(result$line)))
})

test_that("parse_review_locations extracts correct review_id and severity for all rows", {
  txt <- paste0(
    "- **Location**: R/foo.R:10\n",
    "- **Location**: R/bar.R:20\n"
  )
  result <- parse_review_locations(txt, review_id = 99L, severity = "Low", repo = "irishbuoys")

  expect_equal(nrow(result), 2L)
  expect_equal(result$review_id, c(99L, 99L))
  expect_equal(result$severity,  c("Low", "Low"))
  expect_equal(result$repo,      c("irishbuoys", "irishbuoys"))
})

# ---------------------------------------------------------------------------
# (d) Location without line number → row with line = NA
# ---------------------------------------------------------------------------

test_that("parse_review_locations sets line = NA_integer_ when no line number", {
  txt <- "- **Location**: R/helpers.R\n"
  result <- parse_review_locations(txt, review_id = 7L, severity = "Low", repo = "footbet")

  expect_equal(nrow(result), 1L)
  expect_equal(result$file_path, "R/helpers.R")
  expect_true(is.na(result$line))
  expect_type(result$line, "integer")
})

test_that("parse_review_locations handles path with directory but no line", {
  txt <- "- **Location**: tests/testthat/test-export.R\n"
  result <- parse_review_locations(txt, review_id = 3L, severity = "High", repo = "llm")

  expect_equal(nrow(result), 1L)
  expect_equal(result$file_path, "tests/testthat/test-export.R")
  expect_true(is.na(result$line))
})

# ---------------------------------------------------------------------------
# (e) Real-world fixture — multi-finding from existing fixture file
# ---------------------------------------------------------------------------

test_that("parse_review_locations extracts 4 locations from multi-finding fixture", {
  fixture_path <- system.file("extdata", "roborev_fixtures", "fixture-multi-finding.md",
                               package = "llmtelemetry")
  if (!nzchar(fixture_path)) {
    fixture_path <- here::here("inst", "extdata", "roborev_fixtures", "fixture-multi-finding.md")
  }
  skip_if_not(file.exists(fixture_path), "fixture-multi-finding.md not found")

  txt <- paste(readLines(fixture_path, warn = FALSE), collapse = "\n")
  # The fixture has 4 findings but the medium finding has 2 locations on ONE
  # Location: line (semicolon-separated). We take only the first segment per
  # Location: line, so we get 4 rows (one per Location: line in the raw output).
  #
  # Fixture Location: lines:
  #  1. R/scrape_data.R:287-307 (High)
  #  2. R/process_data.R:100-120 (High)
  #  3. `R/canonicalize.R:44-67`; `inst/scripts/export_data.R:71-99` (Medium — one line, first segment)
  #  4. tests/testthat/test-export.R:383-405 (Low)
  # → 4 Location: lines → 4 rows (primary file_path per line)
  result <- parse_review_locations(txt, review_id = 9001L, severity = "mixed", repo = "llmtelemetry")

  expect_s3_class(result, "tbl_df")
  # 4 Location: lines in the fixture
  expect_equal(nrow(result), 4L)
  expect_named(result, c("review_id", "file_path", "line", "severity", "repo"))
  # All review_ids match
  expect_true(all(result$review_id == 9001L))
  # All file_paths are non-empty strings
  expect_true(all(nzchar(result$file_path)))
})
