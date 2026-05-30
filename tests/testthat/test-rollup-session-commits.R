# Tests for rollup_session_commits()
# Verifies: (a) a known session links to expected commits,
#            (b) commits outside session windows are NOT linked.

# ---------------------------------------------------------------------------
# Helpers: write minimal parquet fixtures via DuckDB
# ---------------------------------------------------------------------------

make_sessions_parquet <- function(rows, dir = tempdir()) {
  path <- file.path(dir, paste0("fix_sessions_", format(Sys.time(), "%H%M%S%OS3"), ".parquet"))
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "t", rows, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf("COPY t TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd')", path)
  )
  path
}

make_commits_parquet <- function(rows, dir = tempdir()) {
  path <- file.path(dir, paste0("fix_commits_", format(Sys.time(), "%H%M%S%OS3"), ".parquet"))
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "t", rows, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf("COPY t TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd')", path)
  )
  path
}

# ---------------------------------------------------------------------------
# Fixture data
# ---------------------------------------------------------------------------

make_test_sessions <- function() {
  data.frame(
    session_id        = c("ses-A", "ses-B"),
    project           = c("llm", "llm"),
    canonical_project = c("llm", "llm"),
    started_at        = as.POSIXct(c("2026-01-10 09:00:00", "2026-01-20 10:00:00"), tz = "UTC"),
    ended_at          = as.POSIXct(c("2026-01-10 11:00:00", "2026-01-20 12:00:00"), tz = "UTC"),
    duration_min      = c(120.0, 120.0),
    agent             = c("claude-code", "codex"),
    source            = c("test", "test"),
    working_dir       = c(NA_character_, NA_character_),
    valid_from        = as.POSIXct("2026-01-01", tz = "UTC"),
    stringsAsFactors  = FALSE
  )
}

make_test_commits <- function() {
  data.frame(
    commit_pk         = c("llm|abc0001", "llm|abc0002", "llm|abc0003", "footbet|xyz0001"),
    project           = c("llm", "llm", "llm", "footbet"),
    canonical_project = c("llm", "llm", "llm", "footbet"),
    hash              = c("abc0001", "abc0002", "abc0003", "xyz0001"),
    # abc0001 inside ses-A window, abc0002 inside ses-B window,
    # abc0003 outside any session window, xyz0001 different project
    date              = as.Date(c("2026-01-10", "2026-01-20", "2026-01-15", "2026-01-10")),
    message           = c("commit 1", "commit 2", "commit 3 (outside)", "other project"),
    lines_added       = c(10L, 20L, 5L, 8L),
    lines_deleted     = c(2L, 4L, 1L, 3L),
    files_changed     = c(1L, 2L, 1L, 1L),
    lines_changed     = c(12L, 24L, 6L, 11L),
    valid_from        = as.POSIXct("2026-01-01", tz = "UTC"),
    stringsAsFactors  = FALSE
  )
}

# ---------------------------------------------------------------------------
# (a) A known session links to expected commits
# ---------------------------------------------------------------------------

test_that("ses-A links to abc0001 (date inside window, matching project)", {
  sessions <- make_sessions_parquet(make_test_sessions())
  commits  <- make_commits_parquet(make_test_commits())
  out_f    <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_session_commits(
    sessions_path = sessions,
    commits_path  = commits,
    output_path   = out_f,
    now           = as.POSIXct("2026-01-01", tz = "UTC")
  )

  ses_a <- result[result$session_id == "ses-A", , drop = FALSE]
  expect_equal(nrow(ses_a), 1L)
  expect_equal(ses_a$commit_sha, "abc0001")
  expect_equal(ses_a$repo, "llm")
  expect_equal(ses_a$agent, "claude-code")
})

test_that("ses-B links to abc0002 (date inside window, matching project)", {
  sessions <- make_sessions_parquet(make_test_sessions())
  commits  <- make_commits_parquet(make_test_commits())
  out_f    <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_session_commits(
    sessions_path = sessions,
    commits_path  = commits,
    output_path   = out_f,
    now           = as.POSIXct("2026-01-01", tz = "UTC")
  )

  ses_b <- result[result$session_id == "ses-B", , drop = FALSE]
  expect_equal(nrow(ses_b), 1L)
  expect_equal(ses_b$commit_sha, "abc0002")
  expect_equal(ses_b$agent, "codex")
})

# ---------------------------------------------------------------------------
# (b) Commits outside session windows are NOT linked
# ---------------------------------------------------------------------------

test_that("abc0003 (date=2026-01-15, between sessions) is NOT linked to any session", {
  sessions <- make_sessions_parquet(make_test_sessions())
  commits  <- make_commits_parquet(make_test_commits())
  out_f    <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_session_commits(
    sessions_path = sessions,
    commits_path  = commits,
    output_path   = out_f,
    now           = as.POSIXct("2026-01-01", tz = "UTC")
  )

  expect_false("abc0003" %in% result$commit_sha,
    info = "commit on 2026-01-15 falls outside both session windows (Jan-10 and Jan-20)")
})

test_that("cross-project commit xyz0001 (footbet) is NOT linked to llm sessions", {
  sessions <- make_sessions_parquet(make_test_sessions())
  commits  <- make_commits_parquet(make_test_commits())
  out_f    <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_session_commits(
    sessions_path = sessions,
    commits_path  = commits,
    output_path   = out_f,
    now           = as.POSIXct("2026-01-01", tz = "UTC")
  )

  expect_false("xyz0001" %in% result$commit_sha,
    info = "footbet commit must not link to llm sessions")
})

# ---------------------------------------------------------------------------
# Output schema
# ---------------------------------------------------------------------------

test_that("output parquet has the expected schema columns", {
  sessions <- make_sessions_parquet(make_test_sessions())
  commits  <- make_commits_parquet(make_test_commits())
  out_f    <- withr::local_tempfile(fileext = ".parquet")

  rollup_session_commits(
    sessions_path = sessions,
    commits_path  = commits,
    output_path   = out_f,
    now           = as.POSIXct("2026-01-01", tz = "UTC")
  )

  expect_true(file.exists(out_f))
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  parquet <- DBI::dbGetQuery(
    con, sprintf("SELECT * FROM read_parquet('%s') LIMIT 0", out_f)
  )
  expected_cols <- c("session_id", "commit_sha", "repo", "commit_at",
                     "duration_min", "agent", "valid_from")
  expect_true(all(expected_cols %in% names(parquet)),
    info = paste("Missing:", paste(setdiff(expected_cols, names(parquet)), collapse = ", ")))
})

# ---------------------------------------------------------------------------
# Privacy filter
# ---------------------------------------------------------------------------

test_that("confidential project rows are excluded from output", {
  ses_rows <- data.frame(
    session_id        = "ses-priv",
    project           = "mycare",
    canonical_project = "mycare",
    started_at        = as.POSIXct("2026-01-10 09:00:00", tz = "UTC"),
    ended_at          = as.POSIXct("2026-01-10 11:00:00", tz = "UTC"),
    duration_min      = 120.0,
    agent             = "claude-code",
    source            = "test",
    working_dir       = NA_character_,
    valid_from        = as.POSIXct("2026-01-01", tz = "UTC"),
    stringsAsFactors  = FALSE
  )
  com_rows <- data.frame(
    commit_pk         = "mycare|prv0001",
    project           = "mycare",
    canonical_project = "mycare",
    hash              = "prv0001",
    date              = as.Date("2026-01-10"),
    message           = "private commit",
    lines_added       = 5L, lines_deleted = 2L,
    files_changed     = 1L, lines_changed  = 7L,
    valid_from        = as.POSIXct("2026-01-01", tz = "UTC"),
    stringsAsFactors  = FALSE
  )

  sessions <- make_sessions_parquet(ses_rows)
  commits  <- make_commits_parquet(com_rows)
  out_f    <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_session_commits(
    sessions_path = sessions,
    commits_path  = commits,
    output_path   = out_f,
    now           = as.POSIXct("2026-01-01", tz = "UTC")
  )

  expect_equal(nrow(result), 0L,
    info = "mycare rows must be filtered by drop_confidential_projects()")
})

# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

test_that("sessions with NULL ended_at are excluded", {
  ses_rows <- data.frame(
    session_id        = c("ses-ok", "ses-null-end"),
    project           = c("llm", "llm"),
    canonical_project = c("llm", "llm"),
    started_at        = as.POSIXct(c("2026-01-10 09:00:00", "2026-01-10 09:00:00"), tz = "UTC"),
    ended_at          = as.POSIXct(c("2026-01-10 11:00:00", NA), tz = "UTC"),
    duration_min      = c(120.0, NA_real_),
    agent             = c("claude-code", NA_character_),
    source            = c("test", "test"),
    working_dir       = c(NA_character_, NA_character_),
    valid_from        = as.POSIXct("2026-01-01", tz = "UTC"),
    stringsAsFactors  = FALSE
  )
  com_rows <- data.frame(
    commit_pk         = "llm|abc9999",
    project           = "llm",
    canonical_project = "llm",
    hash              = "abc9999",
    date              = as.Date("2026-01-10"),
    message           = "test commit",
    lines_added       = 5L, lines_deleted = 2L,
    files_changed     = 1L, lines_changed  = 7L,
    valid_from        = as.POSIXct("2026-01-01", tz = "UTC"),
    stringsAsFactors  = FALSE
  )

  sessions <- make_sessions_parquet(ses_rows)
  commits  <- make_commits_parquet(com_rows)
  out_f    <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_session_commits(
    sessions_path = sessions,
    commits_path  = commits,
    output_path   = out_f,
    now           = as.POSIXct("2026-01-01", tz = "UTC")
  )

  # Only ses-ok should link; ses-null-end has no ended_at
  expect_equal(result$session_id[result$commit_sha == "abc9999"], "ses-ok")
})
