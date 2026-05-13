# Tests for Phase 2 dashboard exports
# These tests validate the committed JSON files in inst/extdata/.
# The export script is run manually (or in CI via the refresh workflow) and the
# resulting files are committed as a CI fallback — the same pattern used for
# git_commits_by_project.json.
#
# Do NOT run the full export script inside these tests (too slow; requires live
# ~/.claude/logs/ and git repos). Instead validate the committed snapshots.

extdata_path <- function(filename) {
  system.file("extdata", filename, package = "llmtelemetry")
}

# ── 1. weekly_commits_by_project.json ─────────────────────────────────────────

test_that("weekly_commits_by_project.json exists in inst/extdata", {
  path <- extdata_path("weekly_commits_by_project.json")
  expect_true(nzchar(path), info = "weekly_commits_by_project.json not found via system.file()")
})

test_that("weekly_commits_by_project.json parses to a data.frame", {
  path <- extdata_path("weekly_commits_by_project.json")
  skip_if(!nzchar(path), "weekly_commits_by_project.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("weekly_commits_by_project.json has required columns", {
  path <- extdata_path("weekly_commits_by_project.json")
  skip_if(!nzchar(path), "weekly_commits_by_project.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  required_cols <- c("project", "iso_week", "week_start_date", "n_commits",
                     "total_lines_changed")
  expect_true(
    all(required_cols %in% names(result)),
    info = paste("Missing columns:", paste(setdiff(required_cols, names(result)), collapse = ", "))
  )
})

test_that("weekly_commits_by_project.json has at least one row", {
  path <- extdata_path("weekly_commits_by_project.json")
  skip_if(!nzchar(path), "weekly_commits_by_project.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_gt(nrow(result), 0L)
})

test_that("weekly_commits_by_project.json iso_week matches YYYY-WW pattern", {
  path <- extdata_path("weekly_commits_by_project.json")
  skip_if(!nzchar(path), "weekly_commits_by_project.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_true(
    all(grepl("^[0-9]{4}-[0-9]{2}$", result$iso_week)),
    info = "iso_week values should match YYYY-WW format"
  )
})

test_that("weekly_commits_by_project.json n_commits are positive integers", {
  path <- extdata_path("weekly_commits_by_project.json")
  skip_if(!nzchar(path), "weekly_commits_by_project.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_true(all(result$n_commits > 0L))
  expect_true(all(result$n_commits == as.integer(result$n_commits)))
})

# ── 2. cost_per_commit.json ───────────────────────────────────────────────────

test_that("cost_per_commit.json exists in inst/extdata", {
  path <- extdata_path("cost_per_commit.json")
  expect_true(nzchar(path), info = "cost_per_commit.json not found via system.file()")
})

test_that("cost_per_commit.json parses to a data.frame", {
  path <- extdata_path("cost_per_commit.json")
  skip_if(!nzchar(path), "cost_per_commit.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("cost_per_commit.json has required columns", {
  path <- extdata_path("cost_per_commit.json")
  skip_if(!nzchar(path), "cost_per_commit.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  required_cols <- c("project", "date", "daily_cost_usd", "n_commits",
                     "cost_per_commit_usd")
  expect_true(
    all(required_cols %in% names(result)),
    info = paste("Missing columns:", paste(setdiff(required_cols, names(result)), collapse = ", "))
  )
})

test_that("cost_per_commit.json has at least one row", {
  path <- extdata_path("cost_per_commit.json")
  skip_if(!nzchar(path), "cost_per_commit.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_gt(nrow(result), 0L)
})

test_that("cost_per_commit.json has no zero n_commits (would be divide-by-zero)", {
  path <- extdata_path("cost_per_commit.json")
  skip_if(!nzchar(path), "cost_per_commit.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_true(all(result$n_commits > 0L),
              info = "All rows should have n_commits > 0 (zero-commit days are omitted)")
})

test_that("cost_per_commit.json cost_per_commit_usd is consistent with ratio", {
  path <- extdata_path("cost_per_commit.json")
  skip_if(!nzchar(path), "cost_per_commit.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expected <- round(result$daily_cost_usd / result$n_commits, 4)
  expect_equal(result$cost_per_commit_usd, expected, tolerance = 0.001)
})

# ── 3. file_churn.json ────────────────────────────────────────────────────────

test_that("file_churn.json exists in inst/extdata", {
  path <- extdata_path("file_churn.json")
  expect_true(nzchar(path), info = "file_churn.json not found via system.file()")
})

test_that("file_churn.json parses to a data.frame", {
  path <- extdata_path("file_churn.json")
  skip_if(!nzchar(path), "file_churn.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("file_churn.json has required columns", {
  path <- extdata_path("file_churn.json")
  skip_if(!nzchar(path), "file_churn.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  required_cols <- c("project", "file", "n_commits", "total_lines_added",
                     "total_lines_deleted", "total_lines_changed", "last_changed_date")
  expect_true(
    all(required_cols %in% names(result)),
    info = paste("Missing columns:", paste(setdiff(required_cols, names(result)), collapse = ", "))
  )
})

test_that("file_churn.json has at least one row", {
  path <- extdata_path("file_churn.json")
  skip_if(!nzchar(path), "file_churn.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_gt(nrow(result), 0L)
})

test_that("file_churn.json total_lines_changed equals additions + deletions", {
  path <- extdata_path("file_churn.json")
  skip_if(!nzchar(path), "file_churn.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expected <- result$total_lines_added + result$total_lines_deleted
  expect_equal(result$total_lines_changed, expected)
})

test_that("file_churn.json has at most 50 rows per project", {
  path <- extdata_path("file_churn.json")
  skip_if(!nzchar(path), "file_churn.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  counts_by_project <- tapply(result$file, result$project, length)
  expect_true(
    all(counts_by_project <= 50L),
    info = paste("Projects exceeding 50 files:", paste(names(counts_by_project[counts_by_project > 50]), collapse = ", "))
  )
})

# ── 4. change_coupling.json ───────────────────────────────────────────────────

test_that("change_coupling.json exists in inst/extdata", {
  path <- extdata_path("change_coupling.json")
  expect_true(nzchar(path), info = "change_coupling.json not found via system.file()")
})

test_that("change_coupling.json parses to a data.frame", {
  path <- extdata_path("change_coupling.json")
  skip_if(!nzchar(path), "change_coupling.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("change_coupling.json has required columns", {
  path <- extdata_path("change_coupling.json")
  skip_if(!nzchar(path), "change_coupling.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  required_cols <- c("project", "file_a", "file_b", "n_cochanges",
                     "weight_normalised")
  expect_true(
    all(required_cols %in% names(result)),
    info = paste("Missing columns:", paste(setdiff(required_cols, names(result)), collapse = ", "))
  )
})

test_that("change_coupling.json has at least one row", {
  path <- extdata_path("change_coupling.json")
  skip_if(!nzchar(path), "change_coupling.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_gt(nrow(result), 0L)
})

test_that("change_coupling.json all rows have n_cochanges >= 3", {
  path <- extdata_path("change_coupling.json")
  skip_if(!nzchar(path), "change_coupling.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_true(
    all(result$n_cochanges >= 3L),
    info = "All pairs should have n_cochanges >= 3 (noise threshold)"
  )
})

test_that("change_coupling.json file_a < file_b (no duplicate reversed pairs)", {
  path <- extdata_path("change_coupling.json")
  skip_if(!nzchar(path), "change_coupling.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  # R's < is locale-aware; both creation and check use the same comparison
  expect_true(
    all(result$file_a < result$file_b),
    info = "file_a should always be lexicographically less than file_b within R's locale"
  )
})

test_that("change_coupling.json no duplicate (file_a, file_b, project) pairs", {
  path <- extdata_path("change_coupling.json")
  skip_if(!nzchar(path), "change_coupling.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  pair_keys <- paste(result$project, result$file_a, result$file_b, sep = "|||")
  expect_equal(length(unique(pair_keys)), length(pair_keys),
               info = "Each (project, file_a, file_b) triplet should appear at most once")
})

test_that("change_coupling.json weight_normalised is in [0, 1]", {
  path <- extdata_path("change_coupling.json")
  skip_if(!nzchar(path), "change_coupling.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_true(all(result$weight_normalised >= 0 & result$weight_normalised <= 1))
})

test_that("change_coupling.json has at most 100 rows per project", {
  path <- extdata_path("change_coupling.json")
  skip_if(!nzchar(path), "change_coupling.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  counts_by_project <- tapply(result$file_a, result$project, length)
  expect_true(
    all(counts_by_project <= 100L),
    info = paste("Projects exceeding 100 pairs:", paste(names(counts_by_project[counts_by_project > 100]), collapse = ", "))
  )
})
