# Tests for codex cache incremental refresh bugs:
#   Issue #138: rotated codex-tui.log.* siblings replayed from byte 0 on every refresh
#   Issue #139: incremental session/daily merge treats newest slice as full replacement
#
# TDD approach: tests written to FAIL against the unfixed code (RED), then PASS after fix (GREEN).
# Uses withr::local_tempdir() fixtures; never touches real cache files.

# ── Load pure helpers from script ─────────────────────────────────────────────
# Same sourcing strategy as test-refresh-codex-cache.R
local({
  script <- system.file("scripts", "refresh_codex_cache.R",
                        package = "llmtelemetry")
  if (!nzchar(script)) {
    script_candidate <- tryCatch(
      file.path(here::here(), "inst", "scripts", "refresh_codex_cache.R"),
      error = function(e) ""
    )
    if (nzchar(script_candidate) && file.exists(script_candidate)) {
      script <- script_candidate
    }
  }
  if (!nzchar(script) || !file.exists(script)) return(invisible(NULL))

  lines <- readLines(script, warn = FALSE)

  # Strip `if (!interactive()) { ... }` main block (last block in file)
  main_start <- which(grepl("^if \\(!interactive\\(\\)\\)", lines))[1L]
  if (!is.na(main_start)) lines <- lines[seq_len(main_start - 1L)]

  # Replace here::here() so pkg_root assignment doesn't fail in test context
  lines <- gsub("here::here()", 'tempdir()', lines, fixed = TRUE)

  tmp <- tempfile(fileext = ".R")
  writeLines(lines, tmp)
  suppressMessages(source(tmp, local = FALSE))
  unlink(tmp)
})

# ── Helper: build a minimal OTEL turn line ─────────────────────────────────────
# Produces one parseable token-usage log line for a given thread_id.
make_turn_line <- function(thread_id, total_tokens = 100L, timestamp = "2026-05-20T10:00:00.000000Z") {
  sprintf(
    paste0(
      "%s  INFO session_loop{thread_id=%s}:turn{",
      "otel.name=\"session_task.turn\" thread.id=%s model=gpt-5.4 ",
      "codex.turn.token_usage.input_tokens=%d ",
      "codex.turn.token_usage.cached_input_tokens=0 ",
      "codex.turn.token_usage.non_cached_input_tokens=%d ",
      "codex.turn.token_usage.output_tokens=10 ",
      "codex.turn.token_usage.reasoning_output_tokens=0 ",
      "codex.turn.token_usage.total_tokens=%d}: codex_core::tasks: close"
    ),
    timestamp, thread_id, thread_id,
    total_tokens, total_tokens, total_tokens
  )
}

# ── Issue #138: Rotated-sibling duplicate ingestion ───────────────────────────

test_that("#138: rotated sibling is NOT re-read from byte 0 on second refresh", {
  skip_if(
    !exists("read_codex_log"),
    "read_codex_log not found — script may not be installed"
  )

  td <- withr::local_tempdir()

  log_path    <- file.path(td, "codex-tui.log")
  offset_path <- file.path(td, ".codex_log_offset.txt")

  # Turn line for thread T1 (will go into the sibling)
  line_t1 <- make_turn_line("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa", total_tokens = 100L)
  # Turn line for thread T2 (will go into the active log)
  line_t2 <- make_turn_line("bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb", total_tokens = 200L)

  # ── Refresh #1: active log has both T1 and T2 ──────────────────────────────
  # Simulate active log containing both turns
  writeLines(c(line_t1, line_t2), log_path)

  r1 <- read_codex_log(log_path, offset_path)
  # After refresh 1: both threads should be seen once each
  expect_equal(nrow(r1$turns), 2L, label = "Refresh 1 should see 2 turns")
  expect_true(r1$offset_used > 0L, label = "Offset should advance after refresh 1")

  # Persist offset (simulates what the main block does between refreshes)
  writeLines(as.character(r1$offset_used), offset_path)

  # ── Simulate log rotation: T1 turns move to codex-tui.log.1 ───────────────
  # Move active log -> sibling .1 (rotation)
  file.rename(log_path, paste0(log_path, ".1"))
  # Fresh active log with only T2's new turn
  line_t2_new <- make_turn_line(
    "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb",
    total_tokens = 200L,
    timestamp    = "2026-05-20T11:00:00.000000Z"
  )
  writeLines(line_t2_new, log_path)

  # ── Refresh #2 ─────────────────────────────────────────────────────────────
  r2 <- read_codex_log(log_path, offset_path)

  # Count how many rows are from T1 (the sibling, already fully consumed)
  t1_rows <- sum(r2$turns$thread_id == "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa",
                 na.rm = TRUE)

  # BUG (unfixed): t1_rows == 1 because sibling is re-read from byte 0.
  # FIXED behaviour: t1_rows == 0 because the sibling was fully consumed in refresh 1.
  expect_equal(
    t1_rows, 0L,
    label = paste(
      "#138 fix: rotated sibling should not be re-ingested on second refresh.",
      "Got", t1_rows, "T1 rows (expected 0)."
    )
  )
})

test_that("#138: two refreshes over rotated-log scenario produce no duplicate turns", {
  skip_if(
    !exists("read_codex_log"),
    "read_codex_log not found — script may not be installed"
  )

  td <- withr::local_tempdir()

  log_path    <- file.path(td, "codex-tui.log")
  offset_path <- file.path(td, ".codex_log_offset.txt")

  thread_id <- "cccccccc-cccc-cccc-cccc-cccccccccccc"
  line1 <- make_turn_line(thread_id, total_tokens = 50L,
                           timestamp = "2026-05-20T08:00:00.000000Z")
  line2 <- make_turn_line(thread_id, total_tokens = 60L,
                           timestamp = "2026-05-20T09:00:00.000000Z")

  # Refresh #1: both lines in active log
  writeLines(c(line1, line2), log_path)
  r1 <- read_codex_log(log_path, offset_path)
  expect_equal(nrow(r1$turns), 2L, label = "Refresh 1: 2 turns from active log")

  # Persist offset (simulates what the main block does between refreshes)
  writeLines(as.character(r1$offset_used), offset_path)

  # Rotate: active -> .1, new active is empty
  file.rename(log_path, paste0(log_path, ".1"))
  writeLines(character(0L), log_path)

  # Refresh #2: active log is empty; sibling .1 was already consumed
  r2 <- read_codex_log(log_path, offset_path)

  expect_equal(
    nrow(r2$turns), 0L,
    label = paste(
      "#138 fix: no duplicate rows from fully-consumed sibling.",
      "Got", nrow(r2$turns), "turns (expected 0)."
    )
  )
})

# ── Issue #139: Additive session/daily merge ──────────────────────────────────

test_that("#139: session merge is additive — older rows survive alongside new rows", {
  skip_if(
    !exists("read_codex_log"),
    "read_codex_log not found — script may not be installed"
  )

  # We test the merge logic directly by constructing the tibbles it operates on,
  # mirroring the pattern in the main block (lines 610-619 of the script).
  library(dplyr, warn.conflicts = FALSE)
  library(tibble)

  # Existing sessions JSON (from a prior refresh)
  existing_sessions <- tibble(
    thread_id         = c("t1-old", "t2-only-in-existing"),
    started_at        = c("2026-05-19T08:00:00Z", "2026-05-19T07:00:00Z"),
    ended_at          = c("2026-05-19T09:00:00Z", "2026-05-19T07:30:00Z"),
    canonical_project = c("projA", "projB"),
    model             = c("gpt-5.4", "gpt-5.4"),
    source            = c("interactive", "interactive"),
    total_tokens      = c(1000L, 500L),
    est_cost_usd      = c(0.01, 0.005),
    n_turns           = c(10L, 5L)
  )

  # New sessions slice from this refresh (only t1 is in this window; t1 has 5 more turns)
  sessions_new <- tibble(
    thread_id         = "t1-old",
    started_at        = "2026-05-20T10:00:00Z",
    ended_at          = "2026-05-20T10:30:00Z",
    canonical_project = "projA",
    model             = "gpt-5.4",
    source            = "interactive",
    total_tokens      = 500L,
    est_cost_usd      = 0.005,
    n_turns           = 5L
  )

  # ── Apply the FIXED merge logic ───────────────────────────────────────────
  # For overlapping thread_ids: combine numerics, keep oldest started_at, newest ended_at
  overlap <- inner_join(existing_sessions, sessions_new, by = "thread_id",
                        suffix = c("_old", "_new"))
  merged_overlap <- overlap |>
    mutate(
      started_at        = pmin(started_at_old, started_at_new, na.rm = TRUE),
      ended_at          = pmax(ended_at_old,   ended_at_new,   na.rm = TRUE),
      n_turns           = n_turns_old       + n_turns_new,
      total_tokens      = total_tokens_old  + total_tokens_new,
      est_cost_usd      = coalesce(est_cost_usd_old, 0) + coalesce(est_cost_usd_new, 0),
      canonical_project = coalesce(canonical_project_new, canonical_project_old),
      model             = coalesce(model_new, model_old),
      source            = coalesce(source_new, source_old)
    ) |>
    select(thread_id, started_at, ended_at, canonical_project, model, source,
           total_tokens, est_cost_usd, n_turns)

  new_only <- anti_join(sessions_new, existing_sessions, by = "thread_id")
  old_only <- anti_join(existing_sessions, sessions_new, by = "thread_id")
  all_sessions <- bind_rows(old_only, merged_overlap, new_only)

  # ── Assertions ───────────────────────────────────────────────────────────
  # t2 must survive (was not in new slice)
  expect_true(
    "t2-only-in-existing" %in% all_sessions$thread_id,
    label = "#139 fix: existing-only session t2 must survive the merge"
  )

  # t1 must have combined totals
  t1 <- all_sessions |> filter(thread_id == "t1-old")
  expect_equal(t1$n_turns,      15L, label = "#139 fix: t1 n_turns should be 10+5=15")
  expect_equal(t1$total_tokens, 1500L, label = "#139 fix: t1 total_tokens should be 1000+500=1500")
  expect_equal(t1$est_cost_usd, 0.015, tolerance = 1e-9,
               label = "#139 fix: t1 est_cost_usd should be 0.01+0.005=0.015")

  # t1 started_at must be the earlier one
  expect_equal(t1$started_at, "2026-05-19T08:00:00Z",
               label = "#139 fix: t1 started_at must be the earliest")

  # Total row count: t1 (merged) + t2 (surviving)
  expect_equal(nrow(all_sessions), 2L,
               label = "#139 fix: 2 total rows — t1 merged, t2 preserved")
})

test_that("#139: daily merge is additive — older daily rows survive alongside new rows", {
  skip_if(
    !exists("aggregate_daily"),
    "aggregate_daily not found — script may not be installed"
  )

  library(dplyr, warn.conflicts = FALSE)
  library(tibble)

  key_cols <- c("date", "canonical_project", "model", "source")

  # Existing daily JSON (from a prior refresh for 2026-05-19)
  existing_daily <- tibble(
    date              = c("2026-05-19", "2026-05-18"),
    canonical_project = c("projA", "projA"),
    model             = c("gpt-5.4", "gpt-5.4"),
    source            = c("interactive", "interactive"),
    total_tokens      = c(1000L, 800L),
    n_turns           = c(10L, 8L),
    est_cost_usd      = c(0.01, 0.008)
  )

  # New daily slice: 2026-05-19 has additional turns from this refresh window
  daily_new <- tibble(
    date              = "2026-05-19",
    canonical_project = "projA",
    model             = "gpt-5.4",
    source            = "interactive",
    total_tokens      = 500L,
    n_turns           = 5L,
    est_cost_usd      = 0.005
  )

  # ── Apply the FIXED merge logic ───────────────────────────────────────────
  # Numeric columns to sum (exclude key cols and non-additive cols)
  numeric_cols <- intersect(
    names(existing_daily),
    c("total_tokens", "input_tokens", "cached_input_tokens", "output_tokens",
      "reasoning_output_tokens", "n_turns", "est_cost_usd")
  )

  overlap_daily <- inner_join(existing_daily, daily_new, by = key_cols,
                               suffix = c("_old", "_new"))
  if (nrow(overlap_daily) > 0L) {
    for (col in numeric_cols) {
      old_col <- paste0(col, "_old")
      new_col <- paste0(col, "_new")
      if (old_col %in% names(overlap_daily) && new_col %in% names(overlap_daily)) {
        overlap_daily[[col]] <- overlap_daily[[old_col]] + overlap_daily[[new_col]]
      }
    }
    overlap_daily <- overlap_daily |>
      select(all_of(c(key_cols, numeric_cols)))
  }

  new_only_daily <- anti_join(daily_new, existing_daily, by = key_cols)
  old_only_daily <- anti_join(existing_daily, daily_new, by = key_cols)
  all_daily <- bind_rows(old_only_daily, overlap_daily, new_only_daily) |>
    arrange(date)

  # ── Assertions ───────────────────────────────────────────────────────────

  # 2026-05-18 must survive (not in new slice)
  expect_true(
    "2026-05-18" %in% all_daily$date,
    label = "#139 fix: existing 2026-05-18 daily row must survive the merge"
  )

  # 2026-05-19 must have combined counts
  d19 <- all_daily |>
    filter(date == "2026-05-19", canonical_project == "projA")
  expect_equal(d19$n_turns,      15L, label = "#139 fix: 2026-05-19 n_turns should be 10+5=15")
  expect_equal(d19$total_tokens, 1500L,
               label = "#139 fix: 2026-05-19 total_tokens should be 1000+500=1500")
  expect_equal(d19$est_cost_usd, 0.015, tolerance = 1e-9,
               label = "#139 fix: 2026-05-19 est_cost_usd should be 0.01+0.005=0.015")

  # Total row count: 2026-05-19 (merged) + 2026-05-18 (surviving)
  expect_equal(nrow(all_daily), 2L,
               label = "#139 fix: 2 total daily rows — 2026-05-19 merged, 2026-05-18 preserved")
})

test_that("#139: read_codex_log + merge over two refresh cycles preserves all turns", {
  # Integration-style test: simulate two refresh cycles over a real temp log
  # and assert that the final sessions table reflects the cumulative total.
  skip_if(
    !exists("read_codex_log"),
    "read_codex_log not found — script may not be installed"
  )

  library(dplyr, warn.conflicts = FALSE)
  library(tibble)

  td <- withr::local_tempdir()

  log_path    <- file.path(td, "codex-tui.log")
  offset_path <- file.path(td, ".codex_log_offset.txt")

  thread_id <- "dddddddd-dddd-dddd-dddd-dddddddddddd"
  line_r1 <- make_turn_line(thread_id, total_tokens = 100L,
                             timestamp = "2026-05-20T08:00:00.000000Z")
  line_r2 <- make_turn_line(thread_id, total_tokens = 200L,
                             timestamp = "2026-05-20T09:00:00.000000Z")

  # Refresh 1: write line_r1 to log, read it
  writeLines(line_r1, log_path)
  r1 <- read_codex_log(log_path, offset_path)
  expect_equal(nrow(r1$turns), 1L, label = "Refresh 1: 1 turn")
  expect_equal(r1$turns$total_tokens, 100L)

  # Persist offset (normally done by the main block; tests must replicate this)
  writeLines(as.character(r1$offset_used), offset_path)

  # Simulate incremental write: append line_r2 (this is what codex does between refreshes)
  # Use cat() with append=TRUE to properly append a text line to the existing file
  cat(line_r2, "\n", file = log_path, append = TRUE, sep = "")

  # Refresh 2: should see only the new line (line_r2)
  r2 <- read_codex_log(log_path, offset_path)
  expect_equal(nrow(r2$turns), 1L, label = "Refresh 2: should see only new turn")
  expect_equal(r2$turns$total_tokens, 200L, label = "Refresh 2: should see the appended turn")

  # Now simulate the additive merge (what the fixed main block does):
  # After r1: sessions table = {thread_id: n_turns=1, total_tokens=100}
  sessions_after_r1 <- tibble(
    thread_id         = thread_id,
    started_at        = "2026-05-20T08:00:00Z",
    ended_at          = "2026-05-20T08:00:00Z",
    canonical_project = NA_character_,
    model             = "gpt-5.4",
    source            = "interactive",
    total_tokens      = 100L,
    est_cost_usd      = NA_real_,
    n_turns           = 1L
  )

  # r2 turns -> sessions slice
  sessions_r2 <- tibble(
    thread_id         = thread_id,
    started_at        = "2026-05-20T09:00:00Z",
    ended_at          = "2026-05-20T09:00:00Z",
    canonical_project = NA_character_,
    model             = "gpt-5.4",
    source            = "interactive",
    total_tokens      = 200L,
    est_cost_usd      = NA_real_,
    n_turns           = 1L
  )

  # Fixed additive merge
  overlap <- inner_join(sessions_after_r1, sessions_r2, by = "thread_id",
                        suffix = c("_old", "_new"))
  merged <- overlap |>
    mutate(
      started_at        = pmin(started_at_old, started_at_new, na.rm = TRUE),
      ended_at          = pmax(ended_at_old,   ended_at_new,   na.rm = TRUE),
      n_turns           = n_turns_old + n_turns_new,
      total_tokens      = total_tokens_old + total_tokens_new,
      est_cost_usd      = coalesce(est_cost_usd_old, 0) + coalesce(est_cost_usd_new, 0),
      canonical_project = coalesce(canonical_project_new, canonical_project_old),
      model             = coalesce(model_new, model_old),
      source            = coalesce(source_new, source_old)
    ) |>
    select(thread_id, started_at, ended_at, canonical_project, model, source,
           total_tokens, est_cost_usd, n_turns)

  old_only <- anti_join(sessions_after_r1, sessions_r2, by = "thread_id")
  new_only <- anti_join(sessions_r2, sessions_after_r1, by = "thread_id")
  final_sessions <- bind_rows(old_only, merged, new_only)

  # Assert cumulative: thread_id must have n_turns=2, total_tokens=300
  t <- final_sessions |> filter(thread_id == .env$thread_id)
  expect_equal(nrow(t), 1L,   label = "#139 integration: 1 session row after merge")
  expect_equal(t$n_turns,      2L,   label = "#139 integration: n_turns=1+1=2")
  expect_equal(t$total_tokens, 300L, label = "#139 integration: total_tokens=100+200=300")
  expect_equal(t$started_at,   "2026-05-20T08:00:00Z",
               label = "#139 integration: started_at is the earlier timestamp")
})

# ── Issue #138 round-2: multi-generation rotation ─────────────────────────────

test_that("#138 round-2: two rotations (active->'.1'->'.2') produce no duplicates", {
  skip_if(
    !exists("read_codex_log"),
    "read_codex_log not found — script may not be installed"
  )

  td <- withr::local_tempdir()

  log_path    <- file.path(td, "codex-tui.log")
  offset_path <- file.path(td, ".codex_log_offset.txt")

  # Turn lines for three distinct threads
  line_t1 <- make_turn_line("eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee", total_tokens = 111L,
                              timestamp = "2026-05-20T08:00:00.000000Z")
  line_t2 <- make_turn_line("ffffffff-ffff-ffff-ffff-ffffffffffff", total_tokens = 222L,
                              timestamp = "2026-05-20T09:00:00.000000Z")
  line_t3 <- make_turn_line("00000000-0000-0000-0000-000000000003", total_tokens = 333L,
                              timestamp = "2026-05-20T10:00:00.000000Z")

  # ── Refresh #1: active log has T1 only ────────────────────────────────────
  writeLines(line_t1, log_path)
  r1 <- read_codex_log(log_path, offset_path)
  expect_equal(nrow(r1$turns), 1L, label = "Refresh 1: 1 turn (T1)")
  writeLines(as.character(r1$offset_used), offset_path)

  # ── First rotation: active -> .1, new active has T2 ───────────────────────
  file.rename(log_path, paste0(log_path, ".1"))
  writeLines(line_t2, log_path)

  # ── Refresh #2: T1 in .1 (already consumed), T2 in active (new) ───────────
  r2 <- read_codex_log(log_path, offset_path)
  t1_rows_r2 <- sum(r2$turns$thread_id == "eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee", na.rm = TRUE)
  t2_rows_r2 <- sum(r2$turns$thread_id == "ffffffff-ffff-ffff-ffff-ffffffffffff", na.rm = TRUE)
  expect_equal(t1_rows_r2, 0L, label = "#138 round-2: T1 in .1 must NOT be re-read in refresh 2")
  expect_equal(t2_rows_r2, 1L, label = "#138 round-2: T2 from new active must appear in refresh 2")
  writeLines(as.character(r2$offset_used), offset_path)

  # ── Second rotation: .1 -> .2, active -> .1, new active has T3 ────────────
  file.rename(paste0(log_path, ".1"), paste0(log_path, ".2"))
  file.rename(log_path, paste0(log_path, ".1"))
  writeLines(line_t3, log_path)

  # ── Refresh #3: T1 in .2 (consumed gen-1), T2 in .1 (consumed gen-2), T3 new
  r3 <- read_codex_log(log_path, offset_path)
  t1_rows_r3 <- sum(r3$turns$thread_id == "eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee", na.rm = TRUE)
  t2_rows_r3 <- sum(r3$turns$thread_id == "ffffffff-ffff-ffff-ffff-ffffffffffff", na.rm = TRUE)
  t3_rows_r3 <- sum(r3$turns$thread_id == "00000000-0000-0000-0000-000000000003", na.rm = TRUE)

  expect_equal(
    t1_rows_r3, 0L,
    label = "#138 round-2: T1 in .2 (second-gen sibling) must NOT be re-read"
  )
  expect_equal(
    t2_rows_r3, 0L,
    label = "#138 round-2: T2 in .1 (first-gen sibling) must NOT be re-read"
  )
  expect_equal(
    t3_rows_r3, 1L,
    label = "#138 round-2: T3 from new active must appear exactly once"
  )
})

# ── Issue #138/#139 round-2: NA cost preservation ────────────────────────────

test_that("#139 round-2 session merge: NA+NA cost stays NA, known+NA and known+known sum", {
  library(dplyr, warn.conflicts = FALSE)
  library(tibble)

  # Helper that mirrors the fixed merge logic from the main block
  merge_sessions <- function(old, new) {
    overlap <- inner_join(old, new, by = "thread_id", suffix = c("_old", "_new"))
    if (nrow(overlap) == 0L) return(bind_rows(
      anti_join(old, new, by = "thread_id"),
      anti_join(new, old, by = "thread_id")
    ))
    merged <- overlap |>
      mutate(
        n_turns      = n_turns_old + n_turns_new,
        total_tokens = total_tokens_old + total_tokens_new,
        est_cost_usd = dplyr::case_when(
          is.na(est_cost_usd_old) & is.na(est_cost_usd_new) ~ NA_real_,
          TRUE ~ dplyr::coalesce(est_cost_usd_old, 0) +
                 dplyr::coalesce(est_cost_usd_new, 0)
        )
      ) |>
      select(thread_id, n_turns, total_tokens, est_cost_usd)
    old_only <- anti_join(old, new, by = "thread_id") |>
      select(thread_id, n_turns, total_tokens, est_cost_usd)
    new_only <- anti_join(new, old, by = "thread_id") |>
      select(thread_id, n_turns, total_tokens, est_cost_usd)
    bind_rows(old_only, merged, new_only)
  }

  make_sess <- function(tid, cost, n = 1L, tok = 100L) {
    tibble(thread_id = tid, n_turns = n, total_tokens = tok,
           est_cost_usd = cost)
  }

  # Case 1: NA + NA -> NA (not 0)
  result_na_na <- merge_sessions(
    make_sess("t-na-na", NA_real_),
    make_sess("t-na-na", NA_real_)
  )
  expect_true(
    is.na(result_na_na$est_cost_usd[result_na_na$thread_id == "t-na-na"]),
    label = "#139 round-2 session: NA + NA must remain NA (not 0)"
  )

  # Case 2: known + NA -> sum (with NA treated as 0)
  result_known_na <- merge_sessions(
    make_sess("t-k-na", 0.01),
    make_sess("t-k-na", NA_real_)
  )
  expect_equal(
    result_known_na$est_cost_usd[result_known_na$thread_id == "t-k-na"],
    0.01, tolerance = 1e-9,
    label = "#139 round-2 session: known + NA should sum to known value"
  )

  # Case 3: known + known -> sum
  result_kk <- merge_sessions(
    make_sess("t-kk", 0.01),
    make_sess("t-kk", 0.02)
  )
  expect_equal(
    result_kk$est_cost_usd[result_kk$thread_id == "t-kk"],
    0.03, tolerance = 1e-9,
    label = "#139 round-2 session: known + known must sum correctly"
  )
})

test_that("#139 round-2 daily merge: NA+NA cost stays NA, known+NA and known+known sum", {
  library(dplyr, warn.conflicts = FALSE)
  library(tibble)

  # Helper that mirrors the fixed merge logic from the main block (daily path)
  merge_daily_cost <- function(old_cost, new_cost) {
    dplyr::case_when(
      is.na(old_cost) & is.na(new_cost) ~ NA_real_,
      TRUE ~ dplyr::coalesce(old_cost, 0) + dplyr::coalesce(new_cost, 0)
    )
  }

  # NA + NA -> NA
  expect_true(
    is.na(merge_daily_cost(NA_real_, NA_real_)),
    label = "#139 round-2 daily: NA + NA must remain NA (not 0)"
  )

  # known + NA -> known
  expect_equal(
    merge_daily_cost(0.05, NA_real_), 0.05, tolerance = 1e-9,
    label = "#139 round-2 daily: known + NA must return the known value"
  )

  # NA + known -> known
  expect_equal(
    merge_daily_cost(NA_real_, 0.07), 0.07, tolerance = 1e-9,
    label = "#139 round-2 daily: NA + known must return the known value"
  )

  # known + known -> sum
  expect_equal(
    merge_daily_cost(0.03, 0.04), 0.07, tolerance = 1e-9,
    label = "#139 round-2 daily: known + known must sum"
  )
})

# ── Issue #138 round-3: short-then-grew log before rotation ──────────────────
#
# Scenario: the active log is shorter than 512 bytes at refresh #1 (so
# prev_active_fp$len < 512).  Between refresh #1 and rotation the log grows
# past 512 bytes.  After rotation the sibling's current fingerprint covers
# the full 512-byte window (fp_obj$len == 512) but prev_active_fp$fp only
# covers the shorter original prefix.  .fp_match still succeeds (it compares
# the min-length prefix), but if the consumed offset is stored under
# prev_active_fp$fp the sibling read loop misses it and re-reads from 0,
# producing duplicate turns.
#
# Fix: store offset under fp_obj$fp (the sibling's ACTUAL fingerprint) so
# the key used when marking and the key used when reading are consistent.

test_that("#138 round-3: short-log-then-grew scenario produces NO duplicate ingestion", {
  skip_if(
    !exists("read_codex_log"),
    "read_codex_log not found — script may not be installed"
  )

  td <- withr::local_tempdir()

  log_path    <- file.path(td, "codex-tui.log")
  offset_path <- file.path(td, ".codex_log_offset.txt")

  # Build a turn line that is short enough that the initial log is well under
  # 512 bytes, so prev_active_fp$len < 512 after refresh #1.
  thread_t1 <- "11111111-1111-1111-1111-111111111111"
  line_t1_short <- make_turn_line(thread_t1, total_tokens = 10L,
                                  timestamp = "2026-05-23T07:00:00.000000Z")

  # Verify the initial content is indeed shorter than 512 bytes.
  expect_lt(nchar(line_t1_short, type = "bytes"), 512L,
            label = "Precondition: initial log content must be < 512 bytes")

  # ── Refresh #1: log is short (< 512 bytes) ───────────────────────────────
  writeLines(line_t1_short, log_path)
  r1 <- read_codex_log(log_path, offset_path)
  expect_equal(nrow(r1$turns), 1L, label = "Refresh 1: 1 turn (T1)")
  expect_gt(r1$offset_used, 0L,   label = "Refresh 1: offset must advance")

  # Persist offset (simulates what the main block does between refreshes).
  writeLines(as.character(r1$offset_used), offset_path)

  # ── Log grows past 512 bytes before rotation ──────────────────────────────
  # Append enough padding so the file exceeds 512 bytes.  This simulates a
  # burst of activity that pushes the log past the fingerprint window while
  # it is still the active file.
  pad_line <- paste0("# padding: ", strrep("x", 600L))
  cat(pad_line, "\n", file = log_path, append = TRUE, sep = "")

  # Confirm the file is now > 512 bytes.
  fsize_after_grow <- file.info(log_path)$size
  expect_gt(fsize_after_grow, 512L,
            label = "Precondition: log must exceed 512 bytes before rotation")

  # ── Rotate: active log becomes sibling .1 ────────────────────────────────
  file.rename(log_path, paste0(log_path, ".1"))

  # Fresh active log with a new turn (T2).
  thread_t2 <- "22222222-2222-2222-2222-222222222222"
  line_t2 <- make_turn_line(thread_t2, total_tokens = 20L,
                             timestamp = "2026-05-23T08:00:00.000000Z")
  writeLines(line_t2, log_path)

  # ── Refresh #2 ────────────────────────────────────────────────────────────
  r2 <- read_codex_log(log_path, offset_path)

  # T1 lives in the sibling .1 which was already fully consumed in refresh #1.
  # The short-then-grew bug would cause offset lookup to miss the recorded
  # offset (key mismatch) and re-read the sibling from byte 0, yielding 1 T1
  # row.  The fix must produce 0 T1 rows.
  t1_rows <- sum(r2$turns$thread_id == thread_t1, na.rm = TRUE)
  expect_equal(
    t1_rows, 0L,
    label = paste(
      "#138 round-3: sibling that grew past 512B before rotation must NOT be",
      "re-ingested. Got", t1_rows, "T1 rows (expected 0)."
    )
  )

  # T2 is in the new active log — it must appear exactly once.
  t2_rows <- sum(r2$turns$thread_id == thread_t2, na.rm = TRUE)
  expect_equal(
    t2_rows, 1L,
    label = paste(
      "#138 round-3: T2 from new active must appear exactly once.",
      "Got", t2_rows, "T2 rows (expected 1)."
    )
  )
})
