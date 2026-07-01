# Helper: write a minimal fixture JSON and return its path
make_fixture_json <- function(n = 5L, dir = tempdir()) {
  df <- data.frame(
    session_id        = paste0("sess-", seq_len(n)),
    project           = rep("test_proj", n),
    canonical_project = rep("test_proj", n),
    started_at        = format(
      as.POSIXct("2026-01-01 10:00:00", tz = "UTC") + seq_len(n) * 60,
      "%Y-%m-%d %H:%M:%S"
    ),
    ended_at          = format(
      as.POSIXct("2026-01-01 10:00:00", tz = "UTC") + seq_len(n) * 60 + 300,
      "%Y-%m-%d %H:%M:%S"
    ),
    duration_min      = rep(5.0, n),
    stringsAsFactors  = FALSE
  )
  path <- file.path(dir, paste0("fixture_sessions_", n, ".json"))
  jsonlite::write_json(df, path, auto_unbox = FALSE)
  path
}

# --- Basic correctness ------------------------------------------------------

test_that("rollup_sessions() produces a parquet with the expected row count", {
  n       <- 7L
  fixture <- make_fixture_json(n)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")

  result <- rollup_sessions(
    input_path  = fixture,
    output_path = out_f,
    now         = fixed_t
  )

  expect_equal(nrow(result), n)
})

test_that("rollup_sessions() writes a readable parquet file to the output path", {
  fixture <- make_fixture_json(3L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  rollup_sessions(input_path = fixture, output_path = out_f,
                  now = as.POSIXct("2026-01-01", tz = "UTC"))

  expect_true(file.exists(out_f))
  expect_gt(file.size(out_f), 0L)
})

# --- Column completeness ----------------------------------------------------

test_that("parquet has exactly the v1 column set — no extra, no missing", {
  fixture <- make_fixture_json(4L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  rollup_sessions(input_path = fixture, output_path = out_f,
                  now = as.POSIXct("2026-01-01", tz = "UTC"))

  # Read back via DuckDB (no arrow needed)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  back <- DBI::dbGetQuery(con, sprintf("SELECT * FROM '%s' LIMIT 1", out_f))

  expected_cols <- c(
    "session_id", "project", "canonical_project", "started_at", "ended_at",
    "duration_min", "agent", "source", "working_dir", "valid_from",
    "trigger"   # Phase 2 (#322): per-session provenance tag
  )
  expect_equal(sort(names(back)), sort(expected_cols))
})

# --- valid_from populated ---------------------------------------------------

test_that("valid_from is populated and equals the 'now' argument", {
  fixture <- make_fixture_json(2L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-03-15 12:30:00", tz = "UTC")

  result <- rollup_sessions(
    input_path  = fixture,
    output_path = out_f,
    now         = fixed_t
  )

  expect_false(anyNA(result$valid_from))
  expect_s3_class(result$valid_from, "POSIXct")
  # All rows share the same valid_from passed as 'now'
  expect_true(all(abs(as.numeric(result$valid_from) - as.numeric(fixed_t)) < 1))
})

# --- canonical_project preserved -------------------------------------------

test_that("canonical_project is preserved from input", {
  n       <- 3L
  fixture <- make_fixture_json(n)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_sessions(input_path = fixture, output_path = out_f,
                             now = as.POSIXct("2026-01-01", tz = "UTC"))

  expect_equal(unique(result$canonical_project), "test_proj")
})

# --- source field -----------------------------------------------------------

test_that("source column is set to 'unified_duckdb'", {
  fixture <- make_fixture_json(2L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_sessions(input_path = fixture, output_path = out_f,
                             now = as.POSIXct("2026-01-01", tz = "UTC"))

  expect_true(all(result$source == "unified_duckdb"))
})

# --- Round-trip: write -> read -> equals ------------------------------------

test_that("round-trip: parquet read back matches the returned data frame", {
  fixture <- make_fixture_json(5L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-06-01 00:00:00", tz = "UTC")

  result <- rollup_sessions(
    input_path  = fixture,
    output_path = out_f,
    now         = fixed_t
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  back <- DBI::dbGetQuery(con, sprintf("SELECT * FROM '%s' ORDER BY session_id", out_f))

  result_ordered <- result[order(result$session_id), ]
  row.names(result_ordered) <- NULL
  row.names(back)           <- NULL

  # Compare session_id and row count (timestamp rounding may differ slightly)
  expect_equal(back$session_id, result_ordered$session_id)
  expect_equal(nrow(back), nrow(result_ordered))
  expect_equal(back$source, result_ordered$source)
})

# --- Error on empty input ---------------------------------------------------

test_that("rollup_sessions() errors on empty input data", {
  empty_json <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(data.frame(), empty_json)

  out_f <- withr::local_tempfile(fileext = ".parquet")

  expect_error(
    rollup_sessions(input_path = empty_json, output_path = out_f),
    regexp = "rollup_sessions: input has no rows"
  )
})

# --- Idempotency (row count unchanged on second run) -----------------------
# NOTE: valid_from differs between runs (it reflects when the run happened),
# so we only assert row count and non-valid_from columns are identical.

test_that("running rollup_sessions() twice yields same row count", {
  fixture <- make_fixture_json(6L)
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-01-01", tz = "UTC")

  r1 <- rollup_sessions(input_path = fixture, output_path = out_f, now = fixed_t)
  r2 <- rollup_sessions(input_path = fixture, output_path = out_f, now = fixed_t)

  expect_equal(nrow(r1), nrow(r2))
  expect_equal(r1$session_id, r2$session_id)
})

# ===========================================================================
# Regression tests for append_sessions_from_staging()
# ===========================================================================

# Helper: create a staging dir (persists until R session ends — NOT withr scoped)
# with one JSONL file containing the given events.
make_staging_append_dir <- function(events = list()) {
  base <- tempfile()
  dir.create(base, recursive = TRUE)
  if (length(events) > 0L) {
    jsonl <- paste(vapply(events, jsonlite::toJSON, character(1L),
                          auto_unbox = TRUE),
                   collapse = "\n")
    writeLines(jsonl, file.path(base, "events-2026-01-01.jsonl"))
  }
  base
}

make_envelope_r7 <- function(payload) {
  list(ts = "2026-01-01T10:00:00Z", host = "testhost", pid = "1",
       payload = payload)
}

make_session_stop_r7 <- function(
  session_id   = "sess-001",
  project      = "docs-gh-llmtelemetry",
  started_at   = "2026-01-01T09:00:00Z",
  ended_at     = "2026-01-01T10:00:00Z",
  duration_min = 60
) {
  make_envelope_r7(list(
    event_type   = "session_stop",
    session_id   = session_id,
    project      = project,
    started_at   = started_at,
    ended_at     = ended_at,
    duration_min = duration_min,
    agent        = "claude-sonnet-4-6",
    source       = "claude-code-hook",
    working_dir  = "/tmp/test"
  ))
}

# Helper: write minimal parquet with given session_ids
# include_trigger controls whether the trigger column is present, to allow
# testing the Phase 2 migration path (legacy parquet has no trigger column).
write_parquet_r7 <- function(session_ids, parquet_path, include_trigger = TRUE) {
  rows <- data.frame(
    session_id        = session_ids,
    project           = rep("test_proj", length(session_ids)),
    canonical_project = rep("test_proj", length(session_ids)),
    started_at        = as.POSIXct("2026-01-01 09:00:00", tz = "UTC"),
    ended_at          = as.POSIXct("2026-01-01 10:00:00", tz = "UTC"),
    duration_min      = rep(60.0, length(session_ids)),
    agent             = NA_character_,
    source            = rep("claude-code-hook", length(session_ids)),
    working_dir       = NA_character_,
    valid_from        = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    stringsAsFactors  = FALSE
  )
  if (include_trigger) {
    rows$trigger <- "unknown"
  }
  dir.create(dirname(parquet_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "seed", rows, overwrite = TRUE)
  DBI::dbExecute(con, sprintf(
    "COPY seed TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd')", parquet_path))
  invisible(parquet_path)
}

# ---------------------------------------------------------------------------
# Issue #7 regression: tiebreaker dedup keeps the SECOND (later ended_at) row
# when two duplicate session_stop events share the same started_at.
# ---------------------------------------------------------------------------

test_that("#7 tiebreaker: duplicate sessions sharing started_at — later ended_at retained", {
  # Two events with the same session_id path (same raw path → same sanitized id)
  # and same started_at. The SECOND event has a longer duration — that one wins.
  early_event <- make_session_stop_r7(
    session_id   = "/home/user/proj",
    started_at   = "2026-01-01T09:00:00Z",
    ended_at     = "2026-01-01T09:30:00Z",
    duration_min = 30
  )
  late_event <- make_session_stop_r7(
    session_id   = "/home/user/proj",        # same path → same sanitized id
    started_at   = "2026-01-01T09:00:00Z",   # same started_at (tie)
    ended_at     = "2026-01-01T10:00:00Z",   # longer session — this one wins
    duration_min = 60
  )

  staging <- make_staging_append_dir(list(early_event, late_event))
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-01-01 11:00:00", tz = "UTC")

  n <- append_sessions_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = fixed_t
  )

  expect_equal(n, 1L)  # exactly one unique session written

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  back <- DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s')", out_f))

  expect_equal(nrow(back), 1L)
  # duration_min == 60 confirms the SECOND (later) event was retained
  expect_equal(back$duration_min, 60)
})

# ---------------------------------------------------------------------------
# Issue #8 regression: old-format sanitized IDs in parquet are not re-appended
# Old format: "sanitized@{proj}@{started_at}"  (no @h suffix — 3 fields)
# New format: "sanitized@{proj}@{started_at}@h{hash12}" (4 fields)
# ---------------------------------------------------------------------------

test_that("#8 legacy sanitized id: old-format parquet row not re-appended", {
  out_f <- withr::local_tempfile(fileext = ".parquet")

  # Seed parquet with an OLD-format sanitized id (no hash suffix)
  old_id <- "sanitized@llmtelemetry@2026-01-01T09:00:00Z"
  write_parquet_r7(old_id, out_f)

  # Staging has the same logical session but path produces a NEW-format id
  # (same proj + same started_at, same raw path → same prefix before @h)
  staging <- make_staging_append_dir(list(make_session_stop_r7(
    session_id = "/home/user/llmtelemetry",   # path → sanitized@llmtelemetry@...
    project    = "docs-gh-llmtelemetry",
    started_at = "2026-01-01T09:00:00Z"
  )))

  n <- append_sessions_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-01-01 11:00:00", tz = "UTC")
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  back <- DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s')", out_f))

  # Row count must remain 1 — the old-format row is not duplicated
  expect_equal(nrow(back), 1L)
  expect_equal(n, 0L)
})

# ---------------------------------------------------------------------------
# Issue #9 regression: no hash collisions across 500 distinct paths
# ---------------------------------------------------------------------------

test_that("#9 no hash collisions across 500 distinct sanitized paths", {
  skip_if_not_installed("digest")

  # Generate 500 realistic-looking path-style session ids
  paths <- paste0("/home/user/project_", seq_len(500L), "/session")

  path_hash <- function(p) {
    substr(digest::digest(p, algo = "md5", serialize = FALSE), 1L, 12L)
  }
  hashes <- vapply(paths, path_hash, character(1L))
  expect_equal(length(unique(hashes)), 500L)
})

# ---------------------------------------------------------------------------
# Issue #10: dev-checkout prefers working-tree unified_sessions.json
# (tested via explicit input_path — the dev-detection logic is an internal
# default that chooses here::here() over system.file() in dev mode)
# ---------------------------------------------------------------------------

test_that("#10 rollup_sessions() reads correct data when given working-tree path", {
  # Build a fixture JSON that only contains "wt-sess-*" session_ids.
  # This verifies the function reads what we point it at, not a stale copy.
  n_wt <- 3L
  df_wt <- data.frame(
    session_id        = paste0("wt-sess-", seq_len(n_wt)),
    project           = rep("fake_proj", n_wt),
    canonical_project = rep("fake_proj", n_wt),
    started_at        = format(
      as.POSIXct("2026-01-01 10:00:00", tz = "UTC") + seq_len(n_wt) * 60,
      "%Y-%m-%d %H:%M:%S"
    ),
    ended_at          = format(
      as.POSIXct("2026-01-01 10:00:00", tz = "UTC") + seq_len(n_wt) * 60 + 300,
      "%Y-%m-%d %H:%M:%S"
    ),
    duration_min      = rep(5.0, n_wt),
    stringsAsFactors  = FALSE
  )
  wt_json <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(df_wt, wt_json, auto_unbox = FALSE)
  out_f <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_sessions(
    input_path  = wt_json,
    output_path = out_f,
    now         = as.POSIXct("2026-01-01", tz = "UTC")
  )

  expect_equal(nrow(result), n_wt)
  expect_true(all(startsWith(result$session_id, "wt-sess-")))
})
