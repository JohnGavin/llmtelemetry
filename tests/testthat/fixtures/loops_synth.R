# tests/testthat/fixtures/loops_synth.R
#
# Synthetic loop fixtures for detect_loops tests.
#
# Represents 5 known loop patterns surfaced in recent roborev runs
# (details rotated / anonymised for privacy):
#
#   1. rotated-log      — log rotation causes stale path references
#   2. incremental-merge — merge conflict marker left in incrementally updated file
#   3. sanitize-narrow  — sanitizer misses a narrow edge case
#   4. CI-fallback      — CI uses a fallback path that masks a real failure
#   5. irishbuoys-underscore — column name inconsistency (underscore vs dot)
#
# Each fixture has ≥ 4 rows (≥ 3 cycles) so they trigger loop detection.

make_synth_findings <- function() {
  base_time <- as.POSIXct("2026-04-01 10:00:00", tz = "UTC")

  tibble::tibble(
    job_id       = 1L:20L,
    project      = c(
      rep("llmtelemetry", 4L),  # rotated-log
      rep("llmtelemetry", 4L),  # incremental-merge
      rep("llmtelemetry", 4L),  # sanitize-narrow
      rep("llm",          4L),  # CI-fallback
      rep("irishbuoys",   4L)   # irishbuoys-underscore
    ),
    branch       = rep("main", 20L),
    commit_sha   = paste0("abc", sprintf("%04d", 1L:20L)),
    reviewed_at  = c(
      # rotated-log: 4 findings across 4 days
      base_time + c(0, 86400, 172800, 259200),
      # incremental-merge: 4 findings
      base_time + c(1, 86401, 172801, 259201),
      # sanitize-narrow: 4 findings
      base_time + c(2, 86402, 172802, 259202),
      # CI-fallback: 4 findings
      base_time + c(3, 86403, 172803, 259203),
      # irishbuoys-underscore: 4 findings
      base_time + c(4, 86404, 172804, 259204)
    ),
    severity     = factor(c(
      rep("major",    4L),
      rep("critical", 4L),
      rep("major",    4L),
      rep("minor",    4L),
      rep("major",    4L)
    )),
    primary_file = c(
      rep("R/rotate_log.R",     4L),
      rep("R/merge_helper.R",   4L),
      rep("R/sanitize.R",       4L),
      rep(".github/workflows/ci.yml", 4L),
      rep("R/read_buoys.R",     4L)
    ),
    full_location = paste0(
      c(
        rep("R/rotate_log.R",     4L),
        rep("R/merge_helper.R",   4L),
        rep("R/sanitize.R",       4L),
        rep(".github/workflows/ci.yml", 4L),
        rep("R/read_buoys.R",     4L)
      ),
      ":", c(
        rep(c("10", "12", "10", "12"), 5L)
      )
    ),
    problem_text = c(
      # rotated-log (same text x4 — same content_hash)
      rep("Log path reference does not account for rotation suffix in daily log files", 4L),
      # incremental-merge (same text x4)
      rep("Merge conflict marker left in incrementally updated generated file", 4L),
      # sanitize-narrow (same text x4)
      rep("Sanitizer does not handle the narrow edge case where input contains NULL bytes", 4L),
      # CI-fallback (same text x4)
      rep("CI workflow uses fallback path that masks real failure mode in integration test", 4L),
      # irishbuoys-underscore (same text x4)
      rep("Column name inconsistency: function expects underscore separator but data uses dot", 4L)
    ),
    fix_text     = rep("See inline comment for fix guidance.", 20L),
    agent        = rep("critic", 20L),
    verdict      = factor(rep("fail", 20L))
  )
}

make_synth_agent_runs <- function() {
  base_time <- as.POSIXct("2026-04-01 10:00:00", tz = "UTC")
  # Simulate fixer runs between each pair of consecutive findings
  tibble::tibble(
    agent_id       = paste0("run-", 1L:15L),
    agent_type     = rep("fixer", 15L),
    started_at     = c(
      # rotated-log: 3 fixer runs between 4 findings
      base_time + c(43200, 129600, 216000),
      # incremental-merge: 3 fixer runs
      base_time + c(43201, 129601, 216001),
      # sanitize-narrow: 3 fixer runs
      base_time + c(43202, 129602, 216002),
      # CI-fallback: 3 fixer runs
      base_time + c(43203, 129603, 216003),
      # irishbuoys-underscore: 3 fixer runs
      base_time + c(43204, 129604, 216004)
    ),
    ended_at       = c(
      base_time + c(43200, 129600, 216000) + 300L,
      base_time + c(43201, 129601, 216001) + 300L,
      base_time + c(43202, 129602, 216002) + 300L,
      base_time + c(43203, 129603, 216003) + 300L,
      base_time + c(43204, 129604, 216004) + 300L
    ),
    commit_sha     = paste0("fix", sprintf("%04d", 1L:15L)),
    session_id     = paste0("sess-", 1L:15L),
    total_cost_usd = rep(0.15, 15L)
  )
}
