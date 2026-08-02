# Tests for Phase 2 (#322): trigger column in sessions schema
#
# Covers:
#   1. rollup_sessions() produces trigger="unknown" for all rows (JSON backfill)
#   2. append_sessions_from_staging() extracts trigger from payload
#   3. Absent trigger in payload -> "unknown" (safe default, NOT "interactive")
#   4. Migration: legacy parquet without trigger -> "unknown" after drain
#   5. aggregate_sessions_by_trigger() produces correct splits on fixture data

# ---------------------------------------------------------------------------
# Helpers (shared)
# ---------------------------------------------------------------------------

make_trigger_fixture_json <- function(n = 4L, dir = tempdir()) {
  df <- data.frame(
    session_id        = paste0("t-sess-", seq_len(n)),
    project           = rep("test_proj", n),
    canonical_project = rep("test_proj", n),
    started_at        = format(
      as.POSIXct("2026-06-01 09:00:00", tz = "UTC") + seq_len(n) * 3600,
      "%Y-%m-%d %H:%M:%S"
    ),
    ended_at          = format(
      as.POSIXct("2026-06-01 09:00:00", tz = "UTC") + seq_len(n) * 3600 + 1800,
      "%Y-%m-%d %H:%M:%S"
    ),
    duration_min      = rep(30.0, n),
    stringsAsFactors  = FALSE
  )
  path <- file.path(dir, paste0("trigger_fixture_", n, ".json"))
  jsonlite::write_json(df, path, auto_unbox = FALSE)
  path
}

make_staging_trigger <- function(events = list()) {
  base <- tempfile()
  dir.create(base, recursive = TRUE)
  if (length(events) > 0L) {
    jsonl <- paste(vapply(events, jsonlite::toJSON, character(1L),
                          auto_unbox = TRUE),
                   collapse = "\n")
    writeLines(jsonl, file.path(base, "events-trigger.jsonl"))
  }
  base
}

make_trigger_envelope <- function(payload) {
  list(ts = "2026-06-01T10:00:00Z", host = "testhost", pid = "1",
       payload = payload)
}

make_trigger_session_stop <- function(
  session_id   = "trigger-sess-001",
  project      = "docs-gh-llmtelemetry",
  started_at   = "2026-06-01T09:00:00Z",
  ended_at     = "2026-06-01T10:00:00Z",
  duration_min = 60,
  trigger      = NULL   # NULL means absent from payload
) {
  payload <- list(
    event_type   = "session_stop",
    session_id   = session_id,
    project      = project,
    started_at   = started_at,
    ended_at     = ended_at,
    duration_min = duration_min,
    source       = "claude-code-hook"
  )
  if (!is.null(trigger)) {
    payload$trigger <- trigger
  }
  make_trigger_envelope(payload)
}

write_legacy_parquet <- function(session_ids, parquet_path) {
  # Simulate pre-Phase-2 parquet: no trigger column
  rows <- data.frame(
    session_id        = session_ids,
    project           = rep("test_proj", length(session_ids)),
    canonical_project = rep("test_proj", length(session_ids)),
    started_at        = as.POSIXct("2026-01-01 09:00:00", tz = "UTC"),
    ended_at          = as.POSIXct("2026-01-01 10:00:00", tz = "UTC"),
    duration_min      = rep(60.0, length(session_ids)),
    agent             = NA_character_,
    source            = rep("unified_duckdb", length(session_ids)),
    working_dir       = NA_character_,
    valid_from        = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    stringsAsFactors  = FALSE
    # NO trigger column — this is the pre-Phase-2 schema
  )
  dir.create(dirname(parquet_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "seed", rows, overwrite = TRUE)
  DBI::dbExecute(con, sprintf(
    "COPY seed TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd')", parquet_path))
  invisible(parquet_path)
}

read_parquet_trigger <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s')", path))
}

# ---------------------------------------------------------------------------
# 1. rollup_sessions() produces trigger="unknown" for JSON backfill rows
# ---------------------------------------------------------------------------

test_that("rollup_sessions() adds trigger='unknown' for all JSON backfill rows", {
  fixture <- make_trigger_fixture_json(4L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_sessions(
    input_path  = fixture,
    output_path = out_f,
    now         = as.POSIXct("2026-06-01", tz = "UTC")
  )

  expect_true("trigger" %in% names(result))
  expect_true(all(result$trigger == "unknown"),
    info = "JSON backfill rows must default to 'unknown', not 'interactive'")
})

test_that("rollup_sessions() writes trigger column to parquet", {
  fixture <- make_trigger_fixture_json(3L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  rollup_sessions(input_path = fixture, output_path = out_f,
                  now = as.POSIXct("2026-06-01", tz = "UTC"))

  back <- read_parquet_trigger(out_f)
  expect_true("trigger" %in% names(back))
  expect_true(all(back$trigger == "unknown"))
})

# ---------------------------------------------------------------------------
# 2. append_sessions_from_staging() extracts trigger when present
# ---------------------------------------------------------------------------

test_that("trigger='scheduled' payload → 'scheduled' in parquet", {
  staging <- make_staging_trigger(list(
    make_trigger_session_stop(trigger = "scheduled")
  ))
  out_f <- withr::local_tempfile(fileext = ".parquet")

  n <- append_sessions_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-06-01 11:00:00", tz = "UTC")
  )

  expect_equal(n, 1L)
  back <- read_parquet_trigger(out_f)
  expect_equal(back$trigger, "scheduled")
})

test_that("trigger='interactive' payload → 'interactive' in parquet", {
  staging <- make_staging_trigger(list(
    make_trigger_session_stop(session_id = "inter-001", trigger = "interactive")
  ))
  out_f <- withr::local_tempfile(fileext = ".parquet")

  append_sessions_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-06-01 11:00:00", tz = "UTC")
  )

  back <- read_parquet_trigger(out_f)
  expect_equal(back$trigger, "interactive")
})

# ---------------------------------------------------------------------------
# 3. Absent trigger in payload → "unknown" (NOT "interactive")
# ---------------------------------------------------------------------------

test_that("absent trigger in payload defaults to 'unknown', never 'interactive'", {
  # trigger = NULL means the key is absent from the payload JSON
  staging <- make_staging_trigger(list(
    make_trigger_session_stop(session_id = "legacy-001", trigger = NULL)
  ))
  out_f <- withr::local_tempfile(fileext = ".parquet")

  append_sessions_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-06-01 11:00:00", tz = "UTC")
  )

  back <- read_parquet_trigger(out_f)
  expect_equal(back$trigger, "unknown",
    info = "absent trigger must map to 'unknown', not 'interactive'")
  expect_false(any(back$trigger == "interactive"),
    info = "no row should be labelled 'interactive' when trigger is absent")
})

# ---------------------------------------------------------------------------
# 4. Migration: legacy parquet without trigger → "unknown" after drain
# ---------------------------------------------------------------------------

test_that("migration: legacy parquet without trigger col gets 'unknown' after drain", {
  out_f <- withr::local_tempfile(fileext = ".parquet")

  # Write a pre-Phase-2 parquet (no trigger column)
  write_legacy_parquet("existing-sess-legacy", out_f)

  # Confirm no trigger column in legacy file
  legacy_back <- read_parquet_trigger(out_f)
  expect_false("trigger" %in% names(legacy_back),
    info = "legacy parquet should not have trigger column before migration")

  # Drain a new session on top — this triggers the migration
  staging <- make_staging_trigger(list(
    make_trigger_session_stop(session_id = "new-sess-001", trigger = "interactive")
  ))

  append_sessions_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-06-01 12:00:00", tz = "UTC")
  )

  back <- read_parquet_trigger(out_f)
  expect_equal(nrow(back), 2L)
  expect_true("trigger" %in% names(back))

  legacy_row <- back[back$session_id == "existing-sess-legacy", , drop = FALSE]
  new_row    <- back[back$session_id == "new-sess-001", , drop = FALSE]

  expect_equal(legacy_row$trigger, "unknown",
    info = "legacy row gets 'unknown' trigger during migration")
  expect_equal(new_row$trigger, "interactive",
    info = "new row preserves its trigger value")
})

test_that("migration is idempotent: draining twice with legacy parquet is stable", {
  out_f <- withr::local_tempfile(fileext = ".parquet")
  write_legacy_parquet("idp-sess-001", out_f)

  staging <- make_staging_trigger(list(
    make_trigger_session_stop(session_id = "idp-new-001", trigger = "scheduled")
  ))

  n1 <- append_sessions_from_staging(
    staging_dir = staging, parquet_path = out_f,
    now = as.POSIXct("2026-06-01 11:00:00", tz = "UTC")
  )
  n2 <- append_sessions_from_staging(
    staging_dir = staging, parquet_path = out_f,
    now = as.POSIXct("2026-06-01 11:05:00", tz = "UTC")
  )

  back <- read_parquet_trigger(out_f)
  expect_equal(nrow(back), 2L, info = "idempotent drain: same row count both times")
  expect_equal(n2, 0L, info = "second drain appends 0 rows")
})

# ---------------------------------------------------------------------------
# 5. aggregate_sessions_by_trigger() on fixture data
# ---------------------------------------------------------------------------

test_that("aggregate_sessions_by_trigger() counts and sums by trigger", {
  sessions <- data.frame(
    session_id   = paste0("s", 1:5),
    started_at   = as.POSIXct(
      c("2026-06-01 09:00:00", "2026-06-01 10:00:00", "2026-06-01 11:00:00",
        "2026-06-01 12:00:00", "2026-06-01 13:00:00"), tz = "UTC"),
    duration_min = c(30, 45, 60, 20, 90),
    trigger      = c("scheduled", "interactive", "interactive", "unknown", "scheduled"),
    stringsAsFactors = FALSE
  )

  result <- aggregate_sessions_by_trigger(sessions_df = sessions)

  expect_equal(nrow(result), 3L, info = "3 distinct triggers")
  expect_true(all(c("trigger", "n_sessions", "total_duration_min") %in% names(result)))

  inter <- result[result$trigger == "interactive", ]
  sched <- result[result$trigger == "scheduled", ]
  unk   <- result[result$trigger == "unknown", ]

  expect_equal(inter$n_sessions, 2L)
  expect_equal(inter$total_duration_min, 105)   # 45 + 60

  expect_equal(sched$n_sessions, 2L)
  expect_equal(sched$total_duration_min, 120)   # 30 + 90

  expect_equal(unk$n_sessions, 1L)
  expect_equal(unk$total_duration_min, 20)
})

test_that("aggregate_sessions_by_trigger() returns empty frame when no rows match", {
  empty_df <- data.frame(
    session_id   = character(0),
    started_at   = as.POSIXct(character(0), tz = "UTC"),
    duration_min = numeric(0),
    trigger      = character(0),
    stringsAsFactors = FALSE
  )

  result <- aggregate_sessions_by_trigger(sessions_df = empty_df)

  expect_equal(nrow(result), 0L)
  expect_true(all(c("trigger", "n_sessions", "total_duration_min") %in% names(result)))
})

test_that("aggregate_sessions_by_trigger() with window_start/window_end filters", {
  sessions <- data.frame(
    session_id   = paste0("w", 1:4),
    started_at   = as.POSIXct(
      c("2026-06-01 08:00:00", "2026-06-02 10:00:00",
        "2026-06-03 11:00:00", "2026-06-04 12:00:00"), tz = "UTC"),
    duration_min = c(30, 60, 45, 20),
    trigger      = c("scheduled", "interactive", "interactive", "scheduled"),
    stringsAsFactors = FALSE
  )

  result <- aggregate_sessions_by_trigger(
    sessions_df  = sessions,
    window_start = as.POSIXct("2026-06-02 00:00:00", tz = "UTC"),
    window_end   = as.POSIXct("2026-06-03 23:59:59", tz = "UTC")
  )

  # Only rows from June 2 and 3 are in window
  total_sessions <- sum(result$n_sessions)
  expect_equal(total_sessions, 2L, info = "only 2 sessions in window")
  inter <- result[result$trigger == "interactive", ]
  expect_equal(inter$n_sessions, 2L)
})

test_that("aggregate_sessions_by_trigger() with missing parquet returns empty frame", {
  result <- aggregate_sessions_by_trigger(
    parquet_path = "/nonexistent/path/sessions.parquet"
  )
  expect_equal(nrow(result), 0L)
  expect_true("trigger" %in% names(result))
  expect_true("n_sessions" %in% names(result))
})

test_that("aggregate_sessions_by_trigger() treats NA trigger as 'unknown'", {
  sessions <- data.frame(
    session_id   = c("na-1", "na-2"),
    started_at   = as.POSIXct(c("2026-06-01 09:00:00", "2026-06-01 10:00:00"),
                               tz = "UTC"),
    duration_min = c(30, 45),
    trigger      = c(NA_character_, "interactive"),
    stringsAsFactors = FALSE
  )

  result <- aggregate_sessions_by_trigger(sessions_df = sessions)

  unk <- result[result$trigger == "unknown", ]
  expect_equal(unk$n_sessions, 1L,
    info = "NA trigger should be treated as 'unknown'")
})
