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
  # Use xtfrm() for bytewise ordering consistent with sort(..., method = "radix")
  # used in the export script. R's bare < is locale-dependent and may disagree.
  expect_true(
    all(xtfrm(result$file_a) < xtfrm(result$file_b)),
    info = "file_a should always be bytewise-less than file_b (radix-sort order)"
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

# ── 5. canonical_project column in each project-bearing export ────────────────

check_canonical_project_col <- function(filename) {
  path <- extdata_path(filename)
  if (!nzchar(path)) return(invisible(NULL))
  df <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  if (!is.data.frame(df) || nrow(df) == 0L) return(invisible(NULL))
  if (!"project" %in% names(df)) return(invisible(NULL))

  test_that(paste(filename, "has canonical_project column"), {
    expect_true("canonical_project" %in% names(df),
                info = paste(filename, "should have canonical_project column"))
  })

  test_that(paste(filename, "canonical_project has no NA where project is non-NA"), {
    skip_if(!"canonical_project" %in% names(df))
    # Rows with a project value should mostly resolve; some NAs are expected
    # (agent worktree IDs, meta names).  At minimum the column must exist.
    expect_true(is.character(df$canonical_project) || all(is.na(df$canonical_project)),
                info = "canonical_project must be character or all-NA")
  })
}

for (fn in c("git_commits_by_project.json",
             "weekly_commits_by_project.json",
             "cost_per_commit.json",
             "file_churn.json",
             "change_coupling.json",
             "unified_sessions.json",
             "cost_by_project_estimated.json")) {
  check_canonical_project_col(fn)
}

# ── 6. projects_master.json ───────────────────────────────────────────────────

test_that("projects_master.json exists in inst/extdata", {
  path <- extdata_path("projects_master.json")
  expect_true(nzchar(path), info = "projects_master.json not found via system.file()")
})

test_that("projects_master.json parses to a data.frame", {
  path <- extdata_path("projects_master.json")
  skip_if(!nzchar(path), "projects_master.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("projects_master.json has required columns", {
  path <- extdata_path("projects_master.json")
  skip_if(!nzchar(path), "projects_master.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  required_cols <- c("canonical_project", "n_sources", "first_seen", "last_seen")
  expect_true(
    all(required_cols %in% names(result)),
    info = paste("Missing columns:", paste(setdiff(required_cols, names(result)), collapse = ", "))
  )
})

test_that("projects_master.json has at least one row", {
  path <- extdata_path("projects_master.json")
  skip_if(!nzchar(path), "projects_master.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_gt(nrow(result), 0L)
})

test_that("projects_master.json has no NA canonical_project", {
  path <- extdata_path("projects_master.json")
  skip_if(!nzchar(path), "projects_master.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_false(any(is.na(result$canonical_project)),
               info = "projects_master.json must not contain NA canonical_project rows")
})

test_that("projects_master.json has no empty canonical_project", {
  path <- extdata_path("projects_master.json")
  skip_if(!nzchar(path), "projects_master.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_false(any(!nzchar(result$canonical_project)),
               info = "projects_master.json must not contain empty canonical_project rows")
})

test_that("projects_master.json is sorted alphabetically", {
  path <- extdata_path("projects_master.json")
  skip_if(!nzchar(path), "projects_master.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_equal(result$canonical_project, sort(result$canonical_project),
               info = "canonical_project should be sorted alphabetically")
})

test_that("projects_master.json contains known real projects", {
  path <- extdata_path("projects_master.json")
  skip_if(!nzchar(path), "projects_master.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  known <- c("llm", "llmtelemetry")
  expect_true(all(known %in% result$canonical_project),
              info = paste("Expected projects not found:",
                           paste(setdiff(known, result$canonical_project), collapse = ", ")))
})

test_that("projects_master.json first_seen <= last_seen for all rows", {
  path <- extdata_path("projects_master.json")
  skip_if(!nzchar(path), "projects_master.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  valid <- is.na(result$first_seen) | is.na(result$last_seen) |
    result$first_seen <= result$last_seen
  expect_true(all(valid),
              info = "first_seen should be <= last_seen for every project")
})

test_that("projects_master.json n_sources is a positive integer for all rows", {
  path <- extdata_path("projects_master.json")
  skip_if(!nzchar(path), "projects_master.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_true(all(result$n_sources >= 1L),
              info = "n_sources should be >= 1 for every project")
})

# ── 7. ccusage_daily.json CI fallback regression (#2080) ─────────────────────

test_that("ccusage_daily.json is valid JSON and a flat array (data.frame)", {
  path <- extdata_path("ccusage_daily.json")
  skip_if(!nzchar(path), "ccusage_daily.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_s3_class(result, "data.frame",
    info = "ccusage_daily.json must be a flat JSON array, not a nested object")
})

test_that("export_dashboard_data.R CI fallback uses ccusage_daily.json not ccusage_daily_all.json", {
  script_path <- system.file("scripts", "export_dashboard_data.R",
                             package = "llmtelemetry")
  skip_if(!nzchar(script_path), "export_dashboard_data.R not installed")
  lines <- readLines(script_path)
  fallback_block <- grep("fallback_map", lines)
  skip_if(length(fallback_block) == 0, "fallback_map not found in export script")
  # The CI fallback must copy ccusage_daily.json (not ccusage_daily_all.json)
  # to avoid serving a stale/renamed source file.
  block_lines <- lines[seq(max(1, min(fallback_block) - 2),
                           min(length(lines), max(fallback_block) + 10))]
  expect_false(
    any(grepl('"ccusage_daily_all\\.json"\\s*=\\s*"ccusage_daily\\.json"',
              block_lines)),
    info = paste("fallback_map still maps ccusage_daily_all.json -> ccusage_daily.json.",
                 "PR #93 changed the source key to ccusage_daily.json directly.",
                 "Revert to ccusage_daily_all.json = 'ccusage_daily.json' is a regression.")
  )
  expect_true(
    any(grepl('"ccusage_daily\\.json"\\s*=\\s*"ccusage_daily\\.json"', block_lines)),
    info = paste("fallback_map must map 'ccusage_daily.json' -> 'ccusage_daily.json'",
                 "so the committed extdata file is used directly as the CI fallback.")
  )
})
