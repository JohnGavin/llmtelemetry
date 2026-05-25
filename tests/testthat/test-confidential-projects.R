# Tests for R/confidential_projects.R — drop_confidential_projects() helper
# and the privacy guard applied inside rollup_sessions() / rollup_costs().
#
# Regression for #83: PIT rollup must never write confidential project rows.

# ---------------------------------------------------------------------------
# drop_confidential_projects() — unit tests
# ---------------------------------------------------------------------------

test_that("drop_confidential_projects() drops exact confidential names", {
  df <- data.frame(
    project           = c("mycare", "llmtelemetry", "crypto_solwatch", "footbet",
                          "crypto_swarms", "solwatch", "swarms", "crypto"),
    canonical_project = c("mycare", "llmtelemetry", "crypto_solwatch", "footbet",
                          "crypto_swarms", "solwatch", "swarms", "crypto"),
    stringsAsFactors  = FALSE
  )
  out <- drop_confidential_projects(df)
  expect_equal(nrow(out), 2L)
  expect_setequal(out$project, c("llmtelemetry", "footbet"))
})

test_that("drop_confidential_projects() drops crypto* prefix variants", {
  df <- data.frame(
    project           = c("crypto_newproject", "llm", "crypto_abc"),
    canonical_project = c("crypto_newproject", "llm", "crypto_abc"),
    stringsAsFactors  = FALSE
  )
  out <- drop_confidential_projects(df)
  expect_equal(nrow(out), 1L)
  expect_equal(out$project, "llm")
})

test_that("drop_confidential_projects() drops mycare* prefix variants", {
  df <- data.frame(
    project           = c("mycare_v2", "llm", "mycare-test"),
    canonical_project = c("mycare_v2", "llm", "mycare-test"),
    stringsAsFactors  = FALSE
  )
  out <- drop_confidential_projects(df)
  expect_equal(nrow(out), 1L)
  expect_equal(out$project, "llm")
})

test_that("drop_confidential_projects() is case-insensitive", {
  df <- data.frame(
    project           = c("MYCARE", "SOLWATCH", "SWARMS", "llm"),
    canonical_project = c("MYCARE", "SOLWATCH", "SWARMS", "llm"),
    stringsAsFactors  = FALSE
  )
  out <- drop_confidential_projects(df)
  expect_equal(nrow(out), 1L)
  expect_equal(out$project, "llm")
})

test_that("drop_confidential_projects() drops if ANY checked column matches", {
  # canonical_project is confidential even when project column is not
  df <- data.frame(
    project           = c("alias_x",    "llm"),
    canonical_project = c("solwatch",   "llm"),
    stringsAsFactors  = FALSE
  )
  out <- drop_confidential_projects(df)
  expect_equal(nrow(out), 1L)
  expect_equal(out$project, "llm")
})

test_that("drop_confidential_projects() returns empty df unchanged", {
  df <- data.frame(project = character(0), canonical_project = character(0),
                   stringsAsFactors = FALSE)
  out <- drop_confidential_projects(df)
  expect_equal(nrow(out), 0L)
})

test_that("drop_confidential_projects() returns df unchanged when no confidential rows", {
  df <- data.frame(
    project           = c("llm", "irishbuoys", "footbet"),
    canonical_project = c("llm", "irishbuoys", "footbet"),
    stringsAsFactors  = FALSE
  )
  out <- drop_confidential_projects(df)
  expect_equal(nrow(out), 3L)
})

test_that("drop_confidential_projects() skips absent columns gracefully", {
  df <- data.frame(project = c("mycare", "llm"), stringsAsFactors = FALSE)
  # canonical_project column is absent — should still filter on project
  out <- drop_confidential_projects(df, cols = c("project", "canonical_project"))
  expect_equal(nrow(out), 1L)
  expect_equal(out$project, "llm")
})

# ---------------------------------------------------------------------------
# rollup_sessions() privacy guard — short-form names must be filtered
# ---------------------------------------------------------------------------

test_that("rollup_sessions() drops solwatch and swarms rows", {
  df <- data.frame(
    session_id        = c("sess-1", "sess-2", "sess-3"),
    project           = c("llm", "solwatch", "swarms"),
    canonical_project = c("llm", "solwatch", "swarms"),
    started_at        = format(
      as.POSIXct("2026-01-01 10:00:00", tz = "UTC") + 1:3 * 60,
      "%Y-%m-%d %H:%M:%S"
    ),
    ended_at          = format(
      as.POSIXct("2026-01-01 10:00:00", tz = "UTC") + 1:3 * 60 + 300,
      "%Y-%m-%d %H:%M:%S"
    ),
    duration_min      = rep(5.0, 3),
    stringsAsFactors  = FALSE
  )
  fixture <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(df, fixture, auto_unbox = FALSE)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_sessions(
    input_path  = fixture,
    output_path = out_f,
    now         = as.POSIXct("2026-01-01", tz = "UTC")
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$project, "llm")
  expect_false(any(result$project %in% c("solwatch", "swarms", "mycare", "crypto")))
})

test_that("rollup_sessions() drops mycare and crypto rows", {
  df <- data.frame(
    session_id        = paste0("s-", 1:4),
    project           = c("mycare", "crypto_solwatch", "llm", "crypto"),
    canonical_project = c("mycare", "crypto_solwatch", "llm", "crypto"),
    started_at        = format(
      as.POSIXct("2026-02-01 10:00:00", tz = "UTC") + 1:4 * 60,
      "%Y-%m-%d %H:%M:%S"
    ),
    ended_at          = format(
      as.POSIXct("2026-02-01 10:00:00", tz = "UTC") + 1:4 * 60 + 300,
      "%Y-%m-%d %H:%M:%S"
    ),
    duration_min      = rep(5.0, 4),
    stringsAsFactors  = FALSE
  )
  fixture <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(df, fixture, auto_unbox = FALSE)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_sessions(
    input_path  = fixture,
    output_path = out_f,
    now         = as.POSIXct("2026-02-01", tz = "UTC")
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$project, "llm")
})

# ---------------------------------------------------------------------------
# rollup_costs() privacy guard — short-form names must be filtered
# ---------------------------------------------------------------------------

test_that("rollup_costs() drops solwatch and swarms rows", {
  df <- data.frame(
    date              = c("2026-01-01", "2026-01-01", "2026-01-01"),
    project           = c("llm", "solwatch", "swarms"),
    est_cost          = c(5.0, 3.0, 2.0),
    duration_min      = c(60.0, 30.0, 20.0),
    share             = c(1.0, 1.0, 1.0),
    canonical_project = c("llm", "solwatch", "swarms"),
    stringsAsFactors  = FALSE
  )
  fixture <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(df, fixture, auto_unbox = FALSE)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_costs(
    input_path  = fixture,
    output_path = out_f,
    now         = as.POSIXct("2026-01-01", tz = "UTC")
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$project, "llm")
  expect_false(any(result$project %in% c("solwatch", "swarms", "mycare", "crypto")))
})

test_that("rollup_costs() drops mycare rows", {
  df <- data.frame(
    date              = c("2026-01-01", "2026-01-01"),
    project           = c("mycare", "llm"),
    est_cost          = c(10.0, 5.0),
    duration_min      = c(90.0, 60.0),
    share             = c(1.0, 1.0),
    canonical_project = c("mycare", "llm"),
    stringsAsFactors  = FALSE
  )
  fixture <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(df, fixture, auto_unbox = FALSE)
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  result <- rollup_costs(
    input_path  = fixture,
    output_path = out_f,
    now         = as.POSIXct("2026-01-01", tz = "UTC")
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$project, "llm")
})

# ---------------------------------------------------------------------------
# Staging-drain privacy guard — append_*_from_staging() must filter (#83)
# These tests verify the drain path leaks described in the issue:
# a mycare (or crypto_solwatch / solwatch) row in staging must NOT survive
# the append step. Regression for the drain gap closed in PR #210 / #83 Phase A.
# ---------------------------------------------------------------------------

# Helper shared across the three drain regression tests
.make_staging_event <- function(event_type, project, ...) {
  payload <- c(list(event_type = event_type, project = project), list(...))
  env     <- list(ts = "2026-01-10T12:00:00Z", host = "testhost",
                  pid = "1", payload = payload)
  jsonlite::toJSON(env, auto_unbox = TRUE)
}

.write_staging_jsonl <- function(lines, dir) {
  writeLines(lines, file.path(dir, "events-2026-01-10.jsonl"))
}

# --- append_sessions_from_staging() drain regression ---------------------------

test_that("append_sessions_from_staging() blocks confidential staged sessions (#83)", {
  staging <- withr::local_tempdir()

  safe_event <- .make_staging_event(
    "session_stop",
    project      = "docs-gh-llmtelemetry",
    session_id   = "sess-safe-001",
    started_at   = "2026-01-10T10:00:00Z",
    ended_at     = "2026-01-10T11:00:00Z",
    duration_min = 60
  )
  mycare_event <- .make_staging_event(
    "session_stop",
    project      = "mycare",
    session_id   = "sess-mycare-001",
    started_at   = "2026-01-10T10:00:00Z",
    ended_at     = "2026-01-10T11:00:00Z",
    duration_min = 60
  )
  solwatch_event <- .make_staging_event(
    "session_stop",
    project      = "crypto_solwatch",
    session_id   = "sess-solwatch-001",
    started_at   = "2026-01-10T10:00:00Z",
    ended_at     = "2026-01-10T11:00:00Z",
    duration_min = 60
  )

  .write_staging_jsonl(
    c(safe_event, mycare_event, solwatch_event),
    staging
  )

  out_f <- withr::local_tempfile(fileext = ".parquet")

  n <- append_sessions_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-01-10 12:00:00", tz = "UTC")
  )

  # Only the safe row survives
  expect_equal(n, 1L)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  back <- DBI::dbGetQuery(
    con, sprintf("SELECT * FROM read_parquet('%s')", out_f)
  )
  expect_equal(nrow(back), 1L)
  expect_false(any(tolower(back$canonical_project) %in%
                     c("mycare", "crypto_solwatch", "solwatch", "swarms", "crypto")))
  expect_true(back$canonical_project[1] == "llmtelemetry")
})

# --- append_costs_from_staging() drain regression ------------------------------

test_that("append_costs_from_staging() blocks confidential staged costs (#83)", {
  staging <- withr::local_tempdir()

  safe_event <- .make_staging_event(
    "cost_emitted",
    project        = "docs-gh-llmtelemetry",
    date           = "2026-01-10",
    source         = "ccusage",
    daily_cost_usd = 1.50,
    n_sessions     = 1L,
    duration_min   = 60.0
  )
  mycare_event <- .make_staging_event(
    "cost_emitted",
    project        = "mycare",
    date           = "2026-01-10",
    source         = "ccusage",
    daily_cost_usd = 2.00,
    n_sessions     = 1L,
    duration_min   = 30.0
  )
  solwatch_event <- .make_staging_event(
    "cost_emitted",
    project        = "solwatch",
    date           = "2026-01-10",
    source         = "ccusage",
    daily_cost_usd = 0.75,
    n_sessions     = 1L,
    duration_min   = 15.0
  )

  .write_staging_jsonl(
    c(safe_event, mycare_event, solwatch_event),
    staging
  )

  out_f <- withr::local_tempfile(fileext = ".parquet")

  n <- append_costs_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-01-10 12:00:00", tz = "UTC")
  )

  expect_equal(n, 1L)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  back <- DBI::dbGetQuery(
    con, sprintf("SELECT * FROM read_parquet('%s')", out_f)
  )
  expect_equal(nrow(back), 1L)
  expect_false(any(tolower(back$canonical_project) %in%
                     c("mycare", "solwatch", "swarms", "crypto", "crypto_solwatch")))
})

# --- append_git_commits_from_staging() drain regression -----------------------

test_that("append_git_commits_from_staging() blocks confidential staged commits (#83)", {
  staging <- withr::local_tempdir()

  safe_event <- .make_staging_event(
    "git_commit",
    project       = "docs-gh-llmtelemetry",
    hash          = "safecommit001abc",
    date          = "2026-01-10",
    message       = "feat: safe commit",
    lines_added   = 10L,
    lines_deleted = 2L,
    files_changed = 1L,
    lines_changed = 12L
  )
  mycare_event <- .make_staging_event(
    "git_commit",
    project       = "mycare",
    hash          = "mycarecommit001x",
    date          = "2026-01-10",
    message       = "fix: mycare commit",
    lines_added   = 5L,
    lines_deleted = 1L,
    files_changed = 1L,
    lines_changed = 6L
  )
  solwatch_event <- .make_staging_event(
    "git_commit",
    project       = "crypto_solwatch",
    hash          = "solwatchcommit01",
    date          = "2026-01-10",
    message       = "chore: solwatch commit",
    lines_added   = 3L,
    lines_deleted = 0L,
    files_changed = 1L,
    lines_changed = 3L
  )

  .write_staging_jsonl(
    c(safe_event, mycare_event, solwatch_event),
    staging
  )

  out_f <- withr::local_tempfile(fileext = ".parquet")

  n <- append_git_commits_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-01-10 12:00:00", tz = "UTC")
  )

  expect_equal(n, 1L)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  back <- DBI::dbGetQuery(
    con, sprintf("SELECT * FROM read_parquet('%s')", out_f)
  )
  expect_equal(nrow(back), 1L)
  expect_false(any(tolower(back$canonical_project) %in%
                     c("mycare", "solwatch", "swarms", "crypto", "crypto_solwatch")))
  expect_equal(back$hash, "safecommit001abc")
})
