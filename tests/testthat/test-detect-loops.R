# test-detect-loops.R — Tests for detect_loops() and upsert_loops()

# Load synthetic fixtures
source(test_path("fixtures", "loops_synth.R"))

# ---- helper to build a minimal findings tibble ------------------------------

make_findings <- function(
  n_reviews = 4L,
  severity  = "major",
  file      = "R/foo.R",
  text      = "The function does not validate NA input before division",
  base_time = as.POSIXct("2026-04-01 10:00:00", tz = "UTC"),
  gap_days  = 1L
) {
  tibble::tibble(
    job_id       = seq_len(n_reviews),
    project      = "testpkg",
    branch       = "main",
    commit_sha   = paste0("sha", seq_len(n_reviews)),
    reviewed_at  = base_time + (seq_len(n_reviews) - 1L) * gap_days * 86400L,
    severity     = factor(rep(severity, n_reviews)),
    primary_file = file,
    full_location = paste0(file, ":10"),
    problem_text = text,
    fix_text     = "Add is.na() check",
    agent        = "critic",
    verdict      = factor("fail")
  )
}

make_agent_runs <- function(between_findings, base_time = as.POSIXct("2026-04-01 10:00:00", tz = "UTC")) {
  # between_findings: integer vector of half-day offsets where fix runs occur
  tibble::tibble(
    agent_id       = paste0("r-", seq_along(between_findings)),
    agent_type     = "fixer",
    started_at     = base_time + between_findings * 43200L,
    ended_at       = base_time + between_findings * 43200L + 120L,
    commit_sha     = paste0("fix", seq_along(between_findings)),
    session_id     = paste0("s-", seq_along(between_findings)),
    total_cost_usd = rep(0.10, length(between_findings))
  )
}

# ===========================================================================
# 1. Escalate tier: 12-cycle chain
# ===========================================================================

test_that("detect_loops() classifies a 12-cycle chain as tier = 'escalate'", {
  # 13 findings → 12 consecutive pairs; fixer run between each pair
  n <- 13L
  base <- as.POSIXct("2026-03-01 10:00:00", tz = "UTC")
  findings <- make_findings(n_reviews = n, base_time = base)
  runs <- make_agent_runs(
    between_findings = seq(1L, 2L * (n - 1L), by = 2L),
    base_time        = base
  )

  result <- detect_loops(findings, runs, window_days = 60L)

  expect_equal(nrow(result), 1L)
  expect_equal(result$tier, "escalate")
  expect_equal(result$cycles, n - 1L)
})

# ===========================================================================
# 2. Block tier: 6-cycle chain
# ===========================================================================

test_that("detect_loops() classifies a 6-cycle chain as tier = 'block'", {
  n <- 7L
  base <- as.POSIXct("2026-04-01 10:00:00", tz = "UTC")
  findings <- make_findings(n_reviews = n, base_time = base)
  runs <- make_agent_runs(between_findings = seq(1L, 2L * (n - 1L), by = 2L), base_time = base)

  result <- detect_loops(findings, runs, window_days = 60L)

  expect_equal(result$tier, "block")
  expect_equal(result$cycles, n - 1L)
})

# ===========================================================================
# 3. Watch tier: 3-cycle chain
# ===========================================================================

test_that("detect_loops() classifies a 3-cycle chain as tier = 'watch'", {
  n <- 4L
  base <- as.POSIXct("2026-04-01 10:00:00", tz = "UTC")
  findings <- make_findings(n_reviews = n, base_time = base)
  runs <- make_agent_runs(between_findings = seq(1L, 2L * (n - 1L), by = 2L), base_time = base)

  result <- detect_loops(findings, runs, window_days = 60L)

  expect_equal(result$tier, "watch")
  expect_equal(result$cycles, 3L)
})

# ===========================================================================
# 4. Below threshold: 2 occurrences — not a loop
# ===========================================================================

test_that("detect_loops() excludes chains with fewer than 3 cycles", {
  findings <- make_findings(n_reviews = 3L)  # 2 pairs max
  runs     <- make_agent_runs(between_findings = c(1L, 3L))

  result <- detect_loops(findings, runs)

  # With 3 findings and fixer runs between each pair: 2 cycles → excluded
  expect_equal(nrow(result), 0L)
})

# ===========================================================================
# 5. Empty input returns empty tibble with correct columns
# ===========================================================================

test_that("detect_loops() returns empty tibble with correct columns on NULL input", {
  result <- detect_loops(NULL)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)

  expected_cols <- c(
    "content_hash", "severity", "primary_file", "summary",
    "first_seen", "last_seen", "cycles", "tier",
    "fix_commit_shas", "estimated_wasted_usd",
    "ack_by", "ack_at", "ack_reason", "ack_until"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("detect_loops() returns empty tibble with correct columns on 0-row input", {
  empty_df <- make_findings(n_reviews = 0L)
  result   <- detect_loops(empty_df)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

# ===========================================================================
# 6. Jaccard collapse: re-phrased findings on the same file
# ===========================================================================

test_that("detect_loops() collapses re-phrased findings via Jaccard", {
  base  <- as.POSIXct("2026-04-01 10:00:00", tz = "UTC")

  # 4 findings on the same file, alternating between two phrasings that are
  # >60% Jaccard-similar.
  text_a <- "The function does not validate NA input before division by zero"
  text_b <- "Function fails to validate NA value before division zero operation"

  findings <- tibble::tibble(
    job_id       = 1L:4L,
    project      = "testpkg",
    branch       = "main",
    commit_sha   = paste0("s", 1L:4L),
    reviewed_at  = base + c(0L, 86400L, 172800L, 259200L),
    severity     = factor(rep("major", 4L)),
    primary_file = "R/math.R",
    full_location = "R/math.R:20",
    problem_text = c(text_a, text_b, text_a, text_b),
    fix_text     = "Fix",
    agent        = "critic",
    verdict      = factor("fail")
  )

  runs <- make_agent_runs(between_findings = c(1L, 3L, 5L), base_time = base)
  result <- detect_loops(findings, runs, jaccard_threshold = 0.5, window_days = 60L)

  # Should be detected as a single loop (Jaccard collapse merges the two hashes)
  expect_equal(nrow(result), 1L)
  expect_gte(result$cycles, 3L)
})

# ===========================================================================
# 7. Cost summation
# ===========================================================================

test_that("detect_loops() sums agent costs correctly", {
  n      <- 5L  # 4 cycles
  base   <- as.POSIXct("2026-04-01 10:00:00", tz = "UTC")
  findings <- make_findings(n_reviews = n, base_time = base)
  cost_per_run <- 0.25
  runs <- make_agent_runs(
    between_findings = seq(1L, 2L * (n - 1L), by = 2L),
    base_time        = base
  ) |> dplyr::mutate(total_cost_usd = cost_per_run)

  result <- detect_loops(findings, runs, window_days = 60L)

  # 4 fixer runs × $0.25 = $1.00
  expect_equal(result$estimated_wasted_usd, cost_per_run * (n - 1L), tolerance = 1e-9)
})

# ===========================================================================
# 8. estimated_wasted_usd is NA when agent_runs_df = NULL
# ===========================================================================

test_that("detect_loops() sets estimated_wasted_usd to NA when no agent_runs", {
  findings <- make_findings(n_reviews = 4L)
  result   <- detect_loops(findings, agent_runs_df = NULL)

  # 3 cycles detected (no agent_runs → every consecutive pair is a cycle)
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$estimated_wasted_usd))
})

# ===========================================================================
# 9. content_hash populated and deterministic
# ===========================================================================

test_that("detect_loops() content_hash is deterministic across calls", {
  findings <- make_findings(n_reviews = 4L)

  r1 <- detect_loops(findings, agent_runs_df = NULL)
  r2 <- detect_loops(findings, agent_runs_df = NULL)

  expect_equal(r1$content_hash, r2$content_hash)
})

# ===========================================================================
# 10. Golden test set: 5 synthetic loop fixtures
# ===========================================================================

test_that("detect_loops() detects all 5 synthetic loop patterns (snapshot)", {
  findings <- make_synth_findings()
  runs     <- make_synth_agent_runs()

  result <- detect_loops(findings, runs, window_days = 60L)

  # All 5 should be detected (each has 3 cycles → 'watch' tier)
  expect_equal(nrow(result), 5L)

  # Snapshot the tier + primary_file to catch regressions
  snap <- result[order(result$primary_file), c("tier", "primary_file", "cycles")]
  expect_snapshot(snap)
})

# ===========================================================================
# 11. upsert_loops() ACK preservation
# ===========================================================================

test_that("upsert_loops() preserves existing ack_by when upserting with NULL ack_by", {
  db <- withr::local_tempfile(fileext = ".duckdb")

  # Build a loop row with an ack
  loop_row <- tibble::tibble(
    content_hash         = "abc123",
    severity             = "major",
    primary_file         = "R/foo.R",
    summary              = "The function has a problem",
    first_seen           = as.POSIXct("2026-04-01", tz = "UTC"),
    last_seen            = as.POSIXct("2026-04-05", tz = "UTC"),
    cycles               = 4L,
    tier                 = "watch",
    fix_commit_shas      = "sha001,sha002",
    estimated_wasted_usd = 0.40,
    ack_by               = "john",
    ack_at               = as.POSIXct("2026-04-06", tz = "UTC"),
    ack_reason           = "manually fixed in PR #99",
    ack_until            = as.POSIXct(NA)
  )

  # First insert
  upsert_loops(loop_row, db)

  # Now upsert the same hash with NULL ack fields (as detect_loops would produce)
  updated_row <- loop_row |>
    dplyr::mutate(
      cycles  = 5L,
      tier    = "block",
      ack_by  = NA_character_,
      ack_at  = as.POSIXct(NA),
      ack_reason = NA_character_,
      ack_until  = as.POSIXct(NA)
    )

  upsert_loops(updated_row, db)

  # Read back and verify ack was preserved
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db, read_only = TRUE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  row <- DBI::dbGetQuery(con, "SELECT * FROM roborev_loops WHERE content_hash = 'abc123'")

  expect_equal(row$ack_by, "john")
  expect_equal(row$ack_reason, "manually fixed in PR #99")
  expect_equal(row$cycles, 5L)    # non-ack fields ARE updated
  expect_equal(row$tier,   "block")
})

# ===========================================================================
# 12. upsert_loops() empty input is a no-op
# ===========================================================================

test_that("upsert_loops() with empty tibble returns 0 invisibly", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  result <- upsert_loops(tibble::tibble(), db)
  expect_equal(result, 0L)
})

# ===========================================================================
# 13. .token_jaccard internal helper
# ===========================================================================

test_that(".token_jaccard() returns 1.0 for identical strings", {
  expect_equal(llmtelemetry:::.token_jaccard("fix the broken code", "fix the broken code"), 1.0)
})

test_that(".token_jaccard() returns 0.0 for completely different strings", {
  j <- llmtelemetry:::.token_jaccard("red car fast", "blue sky quiet")
  expect_equal(j, 0.0)
})

test_that(".token_jaccard() returns value in [0, 1]", {
  j <- llmtelemetry:::.token_jaccard(
    "function does not validate NA input before division",
    "fails to validate NA value before division operation"
  )
  expect_gte(j, 0.0)
  expect_lte(j, 1.0)
  # These strings share 4 of 10 union tokens → Jaccard = 0.4
  expect_gte(j, 0.3)
})

test_that(".token_jaccard() is high (>= 0.6) for very similar strings", {
  j <- llmtelemetry:::.token_jaccard(
    "sanitizer does not handle NULL bytes narrow edge case input",
    "sanitizer fails handle NULL bytes narrow edge case"
  )
  expect_gte(j, 0.6)
})

test_that(".token_jaccard() handles empty strings", {
  expect_equal(llmtelemetry:::.token_jaccard("", ""), 1.0)
  expect_equal(llmtelemetry:::.token_jaccard("hello world", ""), 0.0)
})

# ===========================================================================
# 14. Window filter
# ===========================================================================

test_that("detect_loops() respects window_days and excludes old findings", {
  base_old <- as.POSIXct("2026-01-01 10:00:00", tz = "UTC")
  base_new <- as.POSIXct("2026-04-01 10:00:00", tz = "UTC")

  # 4 old findings (outside 30-day window relative to most recent)
  # plus 2 new findings (inside window)
  old_findings <- make_findings(n_reviews = 4L, base_time = base_old)
  new_findings <- make_findings(n_reviews = 2L, base_time = base_new,
                                file = "R/bar.R")

  combined <- dplyr::bind_rows(old_findings, new_findings)

  # With window_days = 30, only the 2 new findings are in scope for 'bar.R'.
  # The old findings are cut off. The new bar.R group has 2 rows → 1 pair → 1 cycle.
  # The old foo.R findings are outside the window if most recent is base_new + 1 day.
  result <- detect_loops(combined, agent_runs_df = NULL, window_days = 30L)

  # Only bar.R findings are in-window (foo.R's max is 2026-01-04, cut by 30d window).
  # bar.R only has 2 findings → 1 cycle → below threshold of 3 → not a loop.
  expect_equal(nrow(result), 0L)
})
