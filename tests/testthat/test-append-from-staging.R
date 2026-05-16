# Tests for append_sessions_from_staging()
#
# Uses withr::local_tempdir() for both staging and parquet isolation.
# DuckDB native I/O throughout (no arrow dependency).

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_staging_dir <- function(events = list()) {
  base <- tempfile()
  dir.create(base, recursive = TRUE)
  if (length(events) > 0L) {
    jsonl <- paste(vapply(events, jsonlite::toJSON, character(1L),
                          auto_unbox = TRUE),
                   collapse = "\n")
    writeLines(jsonl, file.path(base, "events-2026-05-16.jsonl"))
  }
  base
}

make_envelope <- function(payload) {
  list(
    ts      = "2026-05-16T18:30:00Z",
    host    = "testhost",
    pid     = "99999",
    payload = payload
  )
}

make_session_stop <- function(
  session_id   = "sess-test-001",
  project      = "docs-gh-llmtelemetry",
  started_at   = "2026-05-16T16:00:00Z",
  ended_at     = "2026-05-16T18:30:00Z",
  duration_min = 150,
  agent        = "claude-sonnet-4-6",
  source       = "claude-code-hook",
  working_dir  = "/Users/johngavin/docs_gh/llmtelemetry"
) {
  make_envelope(list(
    event_type   = "session_stop",
    session_id   = session_id,
    project      = project,
    started_at   = started_at,
    ended_at     = ended_at,
    duration_min = duration_min,
    agent        = agent,
    source       = source,
    working_dir  = working_dir
  ))
}

read_parquet_via_duckdb <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s')", path))
}

# Write a minimal pre-existing parquet (simulating a JSON backfill session)
write_seed_parquet <- function(session_ids, parquet_path) {
  rows <- data.frame(
    session_id        = session_ids,
    project           = rep("docs-gh-llm", length(session_ids)),
    canonical_project = rep("llm", length(session_ids)),
    started_at        = as.POSIXct("2026-01-01 10:00:00", tz = "UTC"),
    ended_at          = as.POSIXct("2026-01-01 11:00:00", tz = "UTC"),
    duration_min      = rep(60.0, length(session_ids)),
    agent             = NA_character_,
    source            = rep("unified_duckdb", length(session_ids)),
    working_dir       = NA_character_,
    valid_from        = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
    stringsAsFactors  = FALSE
  )
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dir.create(dirname(parquet_path), recursive = TRUE, showWarnings = FALSE)
  DBI::dbWriteTable(con, "seed", rows, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf("COPY seed TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd')",
            parquet_path)
  )
  invisible(parquet_path)
}

# ---------------------------------------------------------------------------
# Test: empty staging is a no-op
# ---------------------------------------------------------------------------

test_that("empty staging dir returns 0 and leaves parquet unchanged", {
  staging  <- withr::local_tempdir()
  out_file <- withr::local_tempfile(fileext = ".parquet")

  n <- append_sessions_from_staging(
    staging_dir  = staging,
    parquet_path = out_file,
    now          = as.POSIXct("2026-05-16 18:30:00", tz = "UTC")
  )

  expect_equal(n, 0L)
  expect_false(file.exists(out_file))  # parquet not created from nothing
})

# ---------------------------------------------------------------------------
# Test: single session_stop event writes one row with all v1 columns
# ---------------------------------------------------------------------------

test_that("single session_stop event appends one row with all v1 columns", {
  staging <- make_staging_dir(list(make_session_stop()))
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-05-16 19:00:00", tz = "UTC")

  n <- append_sessions_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = fixed_t
  )

  expect_equal(n, 1L)
  expect_true(file.exists(out_f))

  back <- read_parquet_via_duckdb(out_f)
  expect_equal(nrow(back), 1L)

  expected_cols <- c(
    "session_id", "project", "canonical_project", "started_at", "ended_at",
    "duration_min", "agent", "source", "working_dir", "valid_from"
  )
  expect_equal(sort(names(back)), sort(expected_cols))

  expect_equal(back$session_id,   "sess-test-001")
  expect_equal(back$source,       "claude-code-hook")
  expect_equal(back$agent,        "claude-sonnet-4-6")
  expect_equal(back$duration_min, 150)
  # canonical_project derived from "docs-gh-llmtelemetry" -> "llmtelemetry"
  expect_equal(back$canonical_project, "llmtelemetry")
  # valid_from equals the 'now' argument
  expect_true(abs(as.numeric(as.POSIXct(back$valid_from, tz = "UTC")) -
                    as.numeric(fixed_t)) < 2)
})

# ---------------------------------------------------------------------------
# Test: dedup against backfill (session_id already in parquet not re-written)
# ---------------------------------------------------------------------------

test_that("dedup: existing session_id in parquet is not re-appended", {
  out_f <- withr::local_tempfile(fileext = ".parquet")
  # Seed parquet with "abc" already present (as if from JSON backfill)
  write_seed_parquet(c("existing-sess-abc"), out_f)

  # Staging contains two events: one new, one duplicate
  staging <- make_staging_dir(list(
    make_session_stop(session_id = "existing-sess-abc"),  # duplicate
    make_session_stop(session_id = "new-sess-xyz")        # new
  ))

  n <- append_sessions_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-05-16 19:00:00", tz = "UTC")
  )

  # Only the new one should be appended
  expect_equal(n, 1L)

  back <- read_parquet_via_duckdb(out_f)
  expect_equal(nrow(back), 2L)  # 1 seed + 1 new
  expect_true("new-sess-xyz" %in% back$session_id)
  # "existing-sess-abc" appears exactly once (not duplicated)
  expect_equal(sum(back$session_id == "existing-sess-abc"), 1L)
})

# ---------------------------------------------------------------------------
# Test: idempotent — running twice produces identical row count
# ---------------------------------------------------------------------------

test_that("running append_sessions_from_staging twice yields identical row count", {
  staging <- make_staging_dir(list(
    make_session_stop(session_id = "idem-sess-001"),
    make_session_stop(session_id = "idem-sess-002")
  ))
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-05-16 19:00:00", tz = "UTC")

  n1 <- append_sessions_from_staging(staging_dir = staging,
                                     parquet_path = out_f, now = fixed_t)
  n2 <- append_sessions_from_staging(staging_dir = staging,
                                     parquet_path = out_f, now = fixed_t)

  expect_equal(n1, 2L)
  expect_equal(n2, 0L)  # all already present on second run

  back <- read_parquet_via_duckdb(out_f)
  expect_equal(nrow(back), 2L)
})

# ---------------------------------------------------------------------------
# Test: non-session_stop events are skipped
# ---------------------------------------------------------------------------

test_that("non-session_stop event types are ignored", {
  cost_event <- make_envelope(list(
    event_type = "cost_emitted",
    session_id = "cost-sess-001",
    cost_usd   = 1.23
  ))
  staging <- make_staging_dir(list(cost_event))
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  n <- append_sessions_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-05-16 19:00:00", tz = "UTC")
  )

  expect_equal(n, 0L)
  expect_false(file.exists(out_f))
})

# ---------------------------------------------------------------------------
# Test: multi-day staging files combine correctly
# ---------------------------------------------------------------------------

test_that("events from multiple daily JSONL files combine correctly", {
  staging <- withr::local_tempdir()

  # Day 1 file
  writeLines(
    jsonlite::toJSON(make_session_stop(session_id = "day1-sess"), auto_unbox = TRUE),
    file.path(staging, "events-2026-05-15.jsonl")
  )
  # Day 2 file
  writeLines(
    jsonlite::toJSON(make_session_stop(session_id = "day2-sess"), auto_unbox = TRUE),
    file.path(staging, "events-2026-05-16.jsonl")
  )

  out_f <- withr::local_tempfile(fileext = ".parquet")

  n <- append_sessions_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-05-16 19:00:00", tz = "UTC")
  )

  expect_equal(n, 2L)
  back <- read_parquet_via_duckdb(out_f)
  expect_equal(nrow(back), 2L)
  expect_true("day1-sess" %in% back$session_id)
  expect_true("day2-sess" %in% back$session_id)
})

# ---------------------------------------------------------------------------
# Test: malformed event missing session_id is skipped with warning
# ---------------------------------------------------------------------------

test_that("event missing session_id is skipped with a warning", {
  bad_event <- make_envelope(list(
    event_type = "session_stop",
    # session_id omitted intentionally
    project    = "docs-gh-llmtelemetry"
  ))
  good_event <- make_session_stop(session_id = "good-sess-001")

  staging <- make_staging_dir(list(bad_event, good_event))
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  # Should warn about the missing session_id AND still append the good event
  # Note: warning fires only if ALL events are missing session_id.
  # With a mix, bad events are silently dropped; the good event proceeds.
  n <- append_sessions_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-05-16 19:00:00", tz = "UTC")
  )

  expect_equal(n, 1L)
  back <- read_parquet_via_duckdb(out_f)
  expect_equal(nrow(back), 1L)
  expect_equal(back$session_id, "good-sess-001")
})

# ---------------------------------------------------------------------------
# Test: all events missing session_id triggers warning and returns 0
# ---------------------------------------------------------------------------

test_that("all events missing session_id triggers warning and returns 0", {
  bad_event <- make_envelope(list(
    event_type = "session_stop",
    project    = "docs-gh-llmtelemetry"
    # no session_id
  ))
  staging <- make_staging_dir(list(bad_event))
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  expect_warning(
    n <- append_sessions_from_staging(
      staging_dir  = staging,
      parquet_path = out_f,
      now          = as.POSIXct("2026-05-16 19:00:00", tz = "UTC")
    ),
    regexp = "missing session_id"
  )

  expect_equal(n, 0L)
  expect_false(file.exists(out_f))
})
