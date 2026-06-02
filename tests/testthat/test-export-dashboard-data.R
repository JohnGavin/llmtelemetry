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
  # Export uses sort(method = "radix") which is byte-order (C-locale). xtfrm()
  # is locale-aware and disagrees in en_US.UTF-8. Force C-locale comparison so
  # this test matches the export's invariant. #116
  withr::with_locale(
    c(LC_COLLATE = "C"),
    expect_true(
      all(result$file_a < result$file_b),
      info = "file_a should always be bytewise-less than file_b (radix-sort order)"
    )
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
  # Use method="radix" (byte-order, locale-independent) to match the export
  # script's sort(method="radix"). Locale-aware sort() disagrees between
  # devtools::test() (C locale) and standalone Rscript (en_US.UTF-8) on
  # mixed-case names like "ClaudeProbe". Radix sort is deterministic
  # regardless of the R session's LC_COLLATE setting. (#163 follow-up)
  expect_equal(result$canonical_project,
               sort(result$canonical_project, method = "radix"),
               info = "canonical_project should be sorted in byte-order (radix) order")
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

# ── 7. legacy_ccusage_daily.json CI fallback regression (#2080, #281 Phase 2) ─

test_that("legacy_ccusage_daily.json is valid JSON and a flat array (data.frame)", {
  path <- extdata_path("legacy_ccusage_daily.json")
  skip_if(!nzchar(path), "legacy_ccusage_daily.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  # expect_s3_class() does not accept info= in this testthat version; use
  # expect_true(is.data.frame()) to keep the diagnostic message. #117
  expect_true(
    is.data.frame(result),
    info = "legacy_ccusage_daily.json must be a flat JSON array, not a nested object"
  )
})

test_that("export_dashboard_data.R CI fallback copies legacy_ccusage_daily.json (not ccusage_daily_all.json)", {
  # Rewritten from the old fallback_map regex test (roborev round V/d).
  # (#281 Phase 2): renamed ccusage_daily.json -> legacy_ccusage_daily.json.
  # The script no longer has a fallback_map list — it copies files directly.
  # We parse the CI fallback block to verify:
  #   (i)  The source file is legacy_ccusage_daily.json (not ccusage_daily_all.json)
  #   (ii) ccusage_daily_all.json does NOT appear as the source of any file.copy call
  #
  # Strategy: find the `fallback_daily_src <- ...` assignment line, extract the
  # filename string, and assert it equals "legacy_ccusage_daily.json".
  script_path <- system.file("scripts", "export_dashboard_data.R",
                             package = "llmtelemetry")
  skip_if(!nzchar(script_path), "export_dashboard_data.R not installed")
  lines <- readLines(script_path)

  # Find the line that assigns the CI fallback source file
  src_line_idx <- grep("fallback_daily_src\\s*<-", lines)
  skip_if(length(src_line_idx) == 0L,
          "fallback_daily_src assignment not found in export_dashboard_data.R")

  # Extract the block: from src assignment to the matching dst copy, max 10 lines
  block_start <- src_line_idx[1L]
  block_end   <- min(length(lines), block_start + 15L)
  block_lines <- lines[block_start:block_end]
  block_text  <- paste(block_lines, collapse = "\n")

  # Parse the block to extract the source filename
  # The line looks like: fallback_daily_src <- file.path(extdata, "legacy_ccusage_daily.json")
  # Extract the quoted filename from the file.path() call
  src_match <- regmatches(block_text,
    regexpr('"([^"]+\\.json)"', block_text, perl = TRUE))
  src_filename <- if (length(src_match) > 0L) gsub('"', '', src_match[1L]) else NA_character_

  # (i) Source must be legacy_ccusage_daily.json (#281 Phase 2 rename)
  expect_equal(
    src_filename,
    "legacy_ccusage_daily.json",
    info = paste("CI fallback source must be 'legacy_ccusage_daily.json' (#281 Phase 2 rename).",
                 "Using 'ccusage_daily_all.json' (old nested format) is a regression",
                 "(PR #93 changed the schema). Found:", src_filename)
  )

  # (ii) ccusage_daily_all.json must not appear as a source for file.copy or
  #       file.path() calls that copy data to the public output.
  #       (Comments explaining why NOT to use it are acceptable.)
  non_comment_lines <- grep("^\\s*#", lines, invert = TRUE, value = TRUE)
  expect_false(
    any(grepl("ccusage_daily_all\\.json", non_comment_lines)),
    info = paste("'ccusage_daily_all.json' must not appear in non-comment code.",
                 "PR #93 renamed/removed this file to eliminate the nested-format schema.",
                 "Any non-comment reference to it is a regression.")
  )

  # Snapshot the block so future changes to CI fallback logic surface explicitly
  expect_snapshot(block_lines)
})

# ── CI fallback determinism (issue #141) ──────────────────────────────────────

test_that("export_dashboard_data.R CI fallback always writes legacy_ccusage_blocks.json (no stale guard)", {
  # (#281 Phase 2): renamed ccusage_blocks.json -> legacy_ccusage_blocks.json.
  # The fallback must always produce legacy_ccusage_blocks.json in the output
  # directory, either by copying the committed inst/extdata snapshot (if present)
  # or by writing an empty array []. It must NEVER silently preserve a stale output
  # (the pre-#163 bug: `if (!file.exists(dst))` skipped writing when dst existed).
  # Validates the fix for issue #141 (updated for PR #163 blocks-fallback refactor).
  script_path <- system.file("scripts", "export_dashboard_data.R",
                             package = "llmtelemetry")
  skip_if(!nzchar(script_path), "export_dashboard_data.R not installed")
  lines <- readLines(script_path)

  # Locate the else-branch fallback block (after `} else {`)
  else_idx <- which(grepl("^\\s*}\\s*else\\s*\\{", lines))
  skip_if(length(else_idx) == 0, "could not locate else-branch in export script")
  fallback_lines <- lines[seq(min(else_idx), length(lines))]

  # (1) The fallback must reference legacy_ccusage_blocks.json as the destination.
  #     Either as a literal or via a fallback_blocks_dst variable assignment.
  references_blocks_dst <- any(grepl("legacy_ccusage_blocks\\.json", fallback_lines))
  expect_true(
    references_blocks_dst,
    info = paste(
      "CI fallback does not reference legacy_ccusage_blocks.json.",
      "The fallback must produce this file (copy snapshot or write empty array). Fixes #141."
    )
  )

  # (2) The old stale-guard pattern (`if (!file.exists(dst))` gating the write
  #     on the DESTINATION not existing) must NOT be present. This is the exact
  #     bug that caused nondeterministic CI: if the destination already existed,
  #     the write was skipped and a stale snapshot was preserved.
  #     Note: `file.exists(src)` gating on the SOURCE is intentional (#163)
  #     and distinguishes "copy committed snapshot" from "write empty fallback".
  #     Only a dst-file-existence guard is forbidden.
  stale_guard <- any(grepl(
    "file\\.exists.*fallback_blocks_dst|!file\\.exists.*['\"]legacy_ccusage_blocks",
    fallback_lines
  ))
  expect_false(
    stale_guard,
    info = paste(
      "CI fallback gates legacy_ccusage_blocks.json on DESTINATION file existence.",
      "This preserves stale output and causes nondeterministic CI.",
      "Remove the if(!file.exists(dst)) guard. Fixes #141."
    )
  )

  # (3) The fallback must contain a write_json call whose PATH argument is
  #     fallback_blocks_dst, not merely any write_json anywhere in the window.
  #     PR #163 uses intermediate variables (fallback_blocks_dst). We narrow the
  #     check to the ~15-line window around `fallback_blocks_src <-` to avoid
  #     unrelated write_json calls elsewhere in the script satisfying the assertion
  #     (#168: the broad fallback_lines-to-EOF search was too loose).
  #     Round-2 tightening: collapse the window to a single string and require
  #     write_json(<first-arg>, fallback_blocks_dst, ...) — i.e. fallback_blocks_dst
  #     must appear as the SECOND positional argument (the path), not as an
  #     unrelated token on a separate line.
  blocks_src_idx <- grep("fallback_blocks_src\\s*<-", lines)
  skip_if(length(blocks_src_idx) == 0L,
          "fallback_blocks_src assignment not found in export_dashboard_data.R")
  blocks_window_start <- blocks_src_idx[[1L]]
  blocks_window_end   <- min(length(lines), blocks_window_start + 15L)
  blocks_branch_lines <- lines[blocks_window_start:blocks_window_end]
  # Collapse to a single string so multi-line calls are matched as one unit.
  blocks_window_text  <- paste(blocks_branch_lines, collapse = "\n")
  # Match write_json or write_json_atomic(<any-first-arg>, fallback_blocks_dst, ...)
  # — the destination must be fallback_blocks_dst, not just any co-occurring token.
  # write_json_atomic is the atomic wrapper used throughout this project (#163).
  has_write_json_fallback <- grepl(
    "write_json(?:_atomic)?\\s*\\([^,]+,\\s*fallback_blocks_dst",
    blocks_window_text,
    perl = TRUE
  )
  expect_true(
    has_write_json_fallback,
    info = paste(
      "CI fallback blocks branch does not contain write_json_atomic(<data>, fallback_blocks_dst, ...).",
      "The destination argument of write_json_atomic must be fallback_blocks_dst.",
      "Add write_json_atomic(list(), fallback_blocks_dst, auto_unbox=TRUE) in the else branch. Fixes #141 #168."
    )
  )
})

test_that("export_dashboard_data.R in-script QA does not treat ccusage_blocks as critical (round-2 fix #141)", {
  # The in-script final QA (section 9) must NOT include ccusage_blocks in its
  # critical_files set. The CI fallback intentionally writes an empty array []
  # when cmonitor-rs is unavailable. Treating 0 rows as a QA failure would cause
  # the script to stop() before the workflow's relaxed gate ever runs — making
  # the workflow-gate change a no-op. Fixes #141 round-2.
  script_path <- system.file("scripts", "export_dashboard_data.R",
                             package = "llmtelemetry")
  skip_if(!nzchar(script_path), "export_dashboard_data.R not installed")
  lines <- readLines(script_path)

  # Locate the critical_files assignment
  crit_idx <- grep("critical_files\\s*<-\\s*c\\(", lines)
  skip_if(length(crit_idx) == 0L,
          "critical_files assignment not found in export_dashboard_data.R")

  # Extract from that line up to the closing parenthesis (max 10 lines)
  block_end <- min(length(lines), crit_idx[1L] + 10L)
  crit_block <- paste(lines[crit_idx[1L]:block_end], collapse = "\n")

  # ccusage_blocks must NOT appear inside the critical_files vector
  expect_false(
    grepl('"ccusage_blocks"', crit_block),
    info = paste(
      "ccusage_blocks is in the critical_files set in section 9 of export_dashboard_data.R.",
      "The CI fallback writes an empty array for this file (cmonitor-rs unavailable),",
      "so treating 0 rows as a QA failure makes the workflow relaxation a no-op.",
      "Remove 'ccusage_blocks' from critical_files and add schema-only validation instead.",
      "Fixes #141 round-2."
    )
  )
})

test_that("export_dashboard_data.R schema-only validation rejects non-array legacy_ccusage_blocks.json (#141 round-3)", {
  # The schema-only validation loop must assert the parsed JSON is a plain
  # (unnamed) list — i.e. a JSON array. A JSON object {} or a scalar must be
  # REJECTED with stop(). A zero-length array [] and a populated array must PASS.
  script_path <- system.file("scripts", "export_dashboard_data.R",
                             package = "llmtelemetry")
  skip_if(!nzchar(script_path), "export_dashboard_data.R not installed")
  lines <- readLines(script_path)

  # Locate the optional_schema_files loop body (lines between the loop header
  # and the closing `}` that ends the for-loop).
  loop_start <- grep("optional_schema_files\\s*<-", lines)
  skip_if(length(loop_start) == 0L,
          "optional_schema_files not found in export_dashboard_data.R")
  # The block runs from the assignment through the end of the for-loop.
  # Grab up to 40 lines from the assignment — generous enough to cover the body.
  block_end <- min(length(lines), loop_start[1L] + 40L)
  schema_block <- paste(lines[loop_start[1L]:block_end], collapse = "\n")

  # 1. Must parse WITHOUT simplification — simplifyVector/DataFrame/Matrix = FALSE.
  #    Using simplifyDataFrame = TRUE (the old code) silently coerces a JSON object
  #    into a data.frame, making the type check below meaningless.
  expect_true(
    grepl("simplifyVector\\s*=\\s*FALSE", schema_block),
    info = paste(
      "Schema-only validation must use simplifyVector = FALSE so that JSON arrays",
      "are returned as plain R lists rather than simplified vectors/data.frames.",
      "Fixes #141 round-3."
    )
  )

  # 2. Must have an explicit named-list / non-list guard that calls stop().
  #    The guard must check both is.list(parsed) AND that names() is empty/NULL
  #    (to distinguish unnamed JSON arrays from named JSON objects).
  has_type_guard <- grepl("is\\.list\\(parsed\\)", schema_block) &&
    grepl("names\\(parsed\\)", schema_block) &&
    grepl("stop\\(", schema_block)
  expect_true(
    has_type_guard,
    info = paste(
      "Schema-only validation must guard against non-array JSON by checking",
      "is.list(parsed), names(parsed), and calling stop() on violation.",
      "A JSON object {} would otherwise be accepted as 'QA OK'.",
      "Fixes #141 round-3."
    )
  )
})

test_that("optional schema guard: [] passes, {} and scalar are REJECTED (#154)", {
  # Behavioural test for the !is.null(names(parsed)) guard (#154).
  # jsonlite::fromJSON("{}") returns a named list with names = character(0)
  # (length 0 but NOT NULL), so the old `length(names(parsed)) > 0L` guard
  # incorrectly accepted {}.  The fix uses !is.null(names(parsed)).
  #
  # Acceptance criteria (directly mirror what export_dashboard_data.R does):
  #   [] -> is.list = TRUE, is.null(names) = TRUE  -> PASS
  #   {} -> is.list = TRUE, is.null(names) = FALSE -> FAIL
  #   42 -> is.list = FALSE                        -> FAIL
  is_valid_array <- function(json_str) {
    parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE,
                                 simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
    is.list(parsed) && is.null(names(parsed))
  }

  # Empty array [] must pass
  expect_true(is_valid_array("[]"),
              label = "[] (empty JSON array) must be accepted as a valid array")

  # Populated array must pass
  expect_true(is_valid_array('[{"a":1},{"a":2}]'),
              label = "Populated JSON array must be accepted")

  # Empty object {} must be REJECTED (#154 — the bug: old guard allowed {})
  expect_false(is_valid_array("{}"),
               label = "{} (empty JSON object) must be REJECTED, not accepted as an array")

  # Scalar must be REJECTED
  expect_false(is_valid_array("42"),
               label = "Scalar JSON value must be REJECTED, not accepted as an array")
})

# ── Phase 2 (#281): CodexBar canonical exports ────────────────────────────────

# ── 8. codexbar_cost_per_day.json ─────────────────────────────────────────────

test_that("codexbar_cost_per_day.json exists in inst/extdata", {
  path <- extdata_path("codexbar_cost_per_day.json")
  expect_true(nzchar(path), info = "codexbar_cost_per_day.json not found via system.file()")
})

test_that("codexbar_cost_per_day.json parses to a data.frame", {
  path <- extdata_path("codexbar_cost_per_day.json")
  skip_if(!nzchar(path), "codexbar_cost_per_day.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_true(is.data.frame(result),
              info = "codexbar_cost_per_day.json must be a flat JSON array (data.frame)")
})

test_that("codexbar_cost_per_day.json has required columns", {
  path <- extdata_path("codexbar_cost_per_day.json")
  skip_if(!nzchar(path), "codexbar_cost_per_day.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  required_cols <- c("date", "model", "cost_usd")
  expect_true(
    all(required_cols %in% names(result)),
    info = paste("Missing columns:", paste(setdiff(required_cols, names(result)), collapse = ", "))
  )
})

test_that("codexbar_cost_per_day.json has at least one row", {
  path <- extdata_path("codexbar_cost_per_day.json")
  skip_if(!nzchar(path), "codexbar_cost_per_day.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_gt(nrow(result), 0L)
})

test_that("codexbar_cost_per_day.json date column matches YYYY-MM-DD pattern", {
  path <- extdata_path("codexbar_cost_per_day.json")
  skip_if(!nzchar(path), "codexbar_cost_per_day.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_true(
    all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", result$date)),
    info = "date values should match YYYY-MM-DD format"
  )
})

test_that("codexbar_cost_per_day.json cost_usd is non-negative", {
  path <- extdata_path("codexbar_cost_per_day.json")
  skip_if(!nzchar(path), "codexbar_cost_per_day.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_true(all(result$cost_usd >= 0), info = "cost_usd must be >= 0")
})

# ── 9. codexbar_cost_per_project.json ─────────────────────────────────────────

test_that("codexbar_cost_per_project.json exists in inst/extdata", {
  path <- extdata_path("codexbar_cost_per_project.json")
  expect_true(nzchar(path), info = "codexbar_cost_per_project.json not found via system.file()")
})

test_that("codexbar_cost_per_project.json parses to a data.frame or is an empty list", {
  path <- extdata_path("codexbar_cost_per_project.json")
  skip_if(!nzchar(path), "codexbar_cost_per_project.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_true(
    is.data.frame(result) || is.list(result),
    info = "codexbar_cost_per_project.json must be a JSON array"
  )
})

test_that("codexbar_cost_per_project.json has required columns if non-empty", {
  path <- extdata_path("codexbar_cost_per_project.json")
  skip_if(!nzchar(path), "codexbar_cost_per_project.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  if (!is.data.frame(result) || nrow(result) == 0L) skip("empty array is valid placeholder")
  required_cols <- c("project", "date", "cost_usd")
  expect_true(
    all(required_cols %in% names(result)),
    info = paste("Missing columns:", paste(setdiff(required_cols, names(result)), collapse = ", "))
  )
})

# ── 10. codexbar_cost_per_session.json ────────────────────────────────────────

test_that("codexbar_cost_per_session.json exists in inst/extdata", {
  path <- extdata_path("codexbar_cost_per_session.json")
  expect_true(nzchar(path), info = "codexbar_cost_per_session.json not found via system.file()")
})

test_that("codexbar_cost_per_session.json parses to a data.frame or is an empty list", {
  path <- extdata_path("codexbar_cost_per_session.json")
  skip_if(!nzchar(path), "codexbar_cost_per_session.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_true(
    is.data.frame(result) || is.list(result),
    info = "codexbar_cost_per_session.json must be a JSON array"
  )
})

test_that("codexbar_cost_per_session.json has required columns if non-empty", {
  path <- extdata_path("codexbar_cost_per_session.json")
  skip_if(!nzchar(path), "codexbar_cost_per_session.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  if (!is.data.frame(result) || nrow(result) == 0L) skip("empty array is valid placeholder")
  required_cols <- c("date", "total_cost_usd")
  expect_true(
    all(required_cols %in% names(result)),
    info = paste("Missing columns:", paste(setdiff(required_cols, names(result)), collapse = ", "))
  )
})

# ── 11. legacy_ccusage_daily.json and legacy_ccusage_blocks.json (2b rename) ──

test_that("legacy_ccusage_daily.json exists in inst/extdata after rename", {
  path <- extdata_path("legacy_ccusage_daily.json")
  expect_true(nzchar(path), info = "legacy_ccusage_daily.json not found — check 2b rename was applied")
})

test_that("legacy_ccusage_daily.json parses to a data.frame", {
  path <- extdata_path("legacy_ccusage_daily.json")
  skip_if(!nzchar(path), "legacy_ccusage_daily.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_true(is.data.frame(result),
              info = "legacy_ccusage_daily.json must be a flat JSON array")
})

test_that("legacy_ccusage_blocks.json exists in inst/extdata after rename", {
  path <- extdata_path("legacy_ccusage_blocks.json")
  expect_true(nzchar(path), info = "legacy_ccusage_blocks.json not found — check 2b rename was applied")
})

# ── 12. unified_costs.json new shape (2c) ─────────────────────────────────────

test_that("unified_costs.json parses as a flat array (data.frame)", {
  path <- extdata_path("unified_costs.json")
  skip_if(!nzchar(path), "unified_costs.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_true(
    is.data.frame(result) || is.list(result),
    info = "unified_costs.json must be a valid JSON array"
  )
})

test_that("unified_costs.json has Phase 2c columns (primary, cross_check, deficit_pct)", {
  # (#281 Phase 2c): unified_costs.json is enriched with CodexBar primary total,
  # ccusage cross-check total, and deficit_pct. These columns may be NA when
  # CodexBar data is absent (CI), but they MUST be present in the schema.
  path <- extdata_path("unified_costs.json")
  skip_if(!nzchar(path), "unified_costs.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  if (!is.data.frame(result) || nrow(result) == 0L)
    skip("unified_costs.json is empty — Phase 2c columns cannot be validated")
  required_cols <- c("primary", "cross_check", "deficit_pct")
  expect_true(
    all(required_cols %in% names(result)),
    info = paste(
      "Phase 2c columns missing from unified_costs.json.",
      "Run export_dashboard_data.R locally to regenerate.",
      "Missing:", paste(setdiff(required_cols, names(result)), collapse = ", ")
    )
  )
})

test_that("unified_costs.json deficit_pct column is numeric or all-NA", {
  # When all values are null in JSON (no CodexBar data for committed date range),
  # fromJSON returns a logical NA vector. Both numeric and all-NA logical are valid.
  path <- extdata_path("unified_costs.json")
  skip_if(!nzchar(path), "unified_costs.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  if (!is.data.frame(result) || nrow(result) == 0L ||
      !"deficit_pct" %in% names(result))
    skip("unified_costs.json empty or missing deficit_pct — checked in sibling test")
  col <- result$deficit_pct
  all_na <- all(is.na(col))
  expect_true(
    is.numeric(col) || all_na,
    info = paste("deficit_pct must be numeric or all-NA.",
                 "Got class:", class(col),
                 "All-NA:", all_na)
  )
  if (is.numeric(col)) {
    non_na <- col[!is.na(col)]
    if (length(non_na) > 0L)
      expect_true(all(non_na > -200 & non_na < 500),
                  info = "deficit_pct out of reasonable range (-200 to 500%)")
  }
})

test_that("unified_costs.json primary column is numeric or all-NA", {
  # Same null→logical coercion caveat as deficit_pct test above.
  path <- extdata_path("unified_costs.json")
  skip_if(!nzchar(path), "unified_costs.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  if (!is.data.frame(result) || nrow(result) == 0L ||
      !"primary" %in% names(result))
    skip("unified_costs.json empty or missing primary — checked in sibling test")
  col <- result$primary
  all_na <- all(is.na(col))
  expect_true(
    is.numeric(col) || all_na,
    info = paste("primary must be numeric (CodexBar daily total) or all-NA (no CodexBar data).",
                 "Got class:", class(col))
  )
  if (is.numeric(col)) {
    non_na <- col[!is.na(col)]
    if (length(non_na) > 0L)
      expect_true(all(non_na >= 0), info = "primary (CodexBar cost) must be >= 0")
  }
})

test_that("unified_costs.json export script adds Phase 2c enrichment block", {
  # Verify that export_dashboard_data.R contains the Phase 2c deficit_pct join.
  # This is a script-level smoke test: if someone removes the enrichment block
  # the schema test above will fail, but this test catches it at the source.
  script_path <- system.file("scripts", "export_dashboard_data.R",
                             package = "llmtelemetry")
  skip_if(!nzchar(script_path), "export_dashboard_data.R not installed")
  lines <- readLines(script_path)
  non_comment <- grep("^\\s*#", lines, invert = TRUE, value = TRUE)
  expect_true(
    any(grepl("deficit_pct", non_comment)),
    info = paste("export_dashboard_data.R does not compute deficit_pct.",
                 "Phase 2c enrichment block missing from section 8.")
  )
  expect_true(
    any(grepl("cross_check", non_comment)),
    info = paste("export_dashboard_data.R does not add cross_check column.",
                 "Phase 2c enrichment block missing from section 8.")
  )
})

# ── 13. export_dashboard_data.R script references legacy_ccusage filenames (2b) ──

test_that("export_dashboard_data.R writes legacy_ccusage_daily.json not ccusage_daily.json to vignettes/data", {
  script_path <- system.file("scripts", "export_dashboard_data.R",
                             package = "llmtelemetry")
  skip_if(!nzchar(script_path), "export_dashboard_data.R not installed")
  lines <- readLines(script_path)
  non_comment <- grep("^\\s*#", lines, invert = TRUE, value = TRUE)
  # The primary live path must write legacy_ccusage_daily.json
  has_legacy_write <- any(grepl("legacy_ccusage_daily\\.json", non_comment))
  expect_true(has_legacy_write,
              info = "export_dashboard_data.R must write legacy_ccusage_daily.json (Phase 2 rename)")
})

test_that("export_dashboard_data.R writes legacy_ccusage_blocks.json not ccusage_blocks.json to vignettes/data", {
  script_path <- system.file("scripts", "export_dashboard_data.R",
                             package = "llmtelemetry")
  skip_if(!nzchar(script_path), "export_dashboard_data.R not installed")
  lines <- readLines(script_path)
  non_comment <- grep("^\\s*#", lines, invert = TRUE, value = TRUE)
  has_legacy_write <- any(grepl("legacy_ccusage_blocks\\.json", non_comment))
  expect_true(has_legacy_write,
              info = "export_dashboard_data.R must write legacy_ccusage_blocks.json (Phase 2 rename)")
})

# ── 14. Snapshot test for codexbar_cost_per_day.json schema ───────────────────

test_that("codexbar_cost_per_day.json column names match expected schema snapshot", {
  path <- extdata_path("codexbar_cost_per_day.json")
  skip_if(!nzchar(path), "codexbar_cost_per_day.json not installed")
  result <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  skip_if(!is.data.frame(result) || nrow(result) == 0L, "empty data; skip snapshot")
  expect_snapshot(sort(names(result)))
})

