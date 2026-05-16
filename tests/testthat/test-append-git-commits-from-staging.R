# Tests for append_git_commits_from_staging()
#
# Mirrors the 7-scenario suite from test-append-from-staging.R (Phase 1E),
# adapted for git_commit events and dedup by commit_pk.
# DuckDB native I/O throughout (no arrow dependency).

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_staging_dir_git <- function(events = list()) {
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

make_envelope_git <- function(payload) {
  list(
    ts      = "2026-05-16T18:30:00Z",
    host    = "testhost",
    pid     = "99999",
    payload = payload
  )
}

make_git_commit <- function(
  project       = "docs-gh-llmtelemetry",
  hash          = "abc1234def5678",
  date          = "2026-05-16",
  message       = "feat: add Phase 1F",
  lines_added   = 100L,
  lines_deleted = 20L,
  files_changed = 5L,
  lines_changed = 120L
) {
  make_envelope_git(list(
    event_type    = "git_commit",
    project       = project,
    hash          = hash,
    date          = date,
    message       = message,
    lines_added   = lines_added,
    lines_deleted = lines_deleted,
    files_changed = files_changed,
    lines_changed = lines_changed
  ))
}

read_parquet_git <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s')", path))
}

# Write a minimal pre-existing git_commits parquet (simulating JSON backfill)
write_seed_git_parquet <- function(commit_pks, parquet_path) {
  # commit_pk format: "canonical_project|hash"
  rows <- data.frame(
    commit_pk         = commit_pks,
    project           = rep("docs-gh-llm", length(commit_pks)),
    canonical_project = rep("llm", length(commit_pks)),
    hash              = sub("^[^|]+\\|", "", commit_pks),
    date              = as.Date("2026-01-01"),
    message           = rep("initial commit", length(commit_pks)),
    lines_added       = rep(10L, length(commit_pks)),
    lines_deleted     = rep(2L, length(commit_pks)),
    files_changed     = rep(1L, length(commit_pks)),
    lines_changed     = rep(12L, length(commit_pks)),
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
# Scenario 1: Empty staging is a no-op
# ---------------------------------------------------------------------------

test_that("empty staging dir returns 0 and leaves parquet unchanged", {
  staging  <- withr::local_tempdir()
  out_file <- withr::local_tempfile(fileext = ".parquet")

  n <- append_git_commits_from_staging(
    staging_dir  = staging,
    parquet_path = out_file,
    now          = as.POSIXct("2026-05-16 18:30:00", tz = "UTC")
  )

  expect_equal(n, 0L)
  expect_false(file.exists(out_file))
})

# ---------------------------------------------------------------------------
# Scenario 2: Single git_commit event writes one row with correct v1 schema
# ---------------------------------------------------------------------------

test_that("single git_commit event appends one row with all v1 columns", {
  staging <- make_staging_dir_git(list(make_git_commit()))
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-05-16 19:00:00", tz = "UTC")

  n <- append_git_commits_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = fixed_t
  )

  expect_equal(n, 1L)
  expect_true(file.exists(out_f))

  back <- read_parquet_git(out_f)
  expect_equal(nrow(back), 1L)

  expected_cols <- c(
    "commit_pk", "project", "canonical_project", "hash", "date", "message",
    "lines_added", "lines_deleted", "files_changed", "lines_changed",
    "valid_from"
  )
  expect_equal(sort(names(back)), sort(expected_cols))

  # commit_pk = paste(canonical_project, hash, sep="|")
  expect_equal(back$commit_pk, "llmtelemetry|abc1234def5678")
  expect_equal(back$canonical_project, "llmtelemetry")
  expect_equal(back$hash, "abc1234def5678")
  expect_equal(back$lines_added, 100L)
  expect_equal(back$lines_deleted, 20L)
  expect_equal(back$files_changed, 5L)
  expect_equal(back$lines_changed, 120L)
  # valid_from equals the 'now' argument
  expect_true(abs(as.numeric(as.POSIXct(back$valid_from, tz = "UTC")) -
                    as.numeric(fixed_t)) < 2)
})

# ---------------------------------------------------------------------------
# Scenario 3: Dedup against backfill (commit_pk already in parquet not re-written)
# ---------------------------------------------------------------------------

test_that("dedup: existing commit_pk in parquet is not re-appended", {
  out_f <- withr::local_tempfile(fileext = ".parquet")
  # Seed parquet with a commit that would match the staged event
  # commit_pk = paste("llm", "deadbeef01234567", sep="|")
  existing_pk <- "llm|deadbeef01234567"
  write_seed_git_parquet(existing_pk, out_f)

  # Staging: one duplicate (same key), one new
  dup_event <- make_git_commit(
    project = "docs-gh-llm",
    hash    = "deadbeef01234567"  # -> commit_pk = "llm|deadbeef01234567"
  )
  new_event <- make_git_commit(
    project = "docs-gh-llmtelemetry",
    hash    = "abc1234def5678"    # -> commit_pk = "llmtelemetry|abc1234def5678"
  )
  staging <- make_staging_dir_git(list(dup_event, new_event))

  n <- append_git_commits_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-05-16 19:00:00", tz = "UTC")
  )

  expect_equal(n, 1L)

  back <- read_parquet_git(out_f)
  expect_equal(nrow(back), 2L)  # 1 seed + 1 new
  expect_true("llmtelemetry|abc1234def5678" %in% back$commit_pk)
  # Duplicate appears exactly once
  expect_equal(sum(back$commit_pk == existing_pk), 1L)
})

# ---------------------------------------------------------------------------
# Scenario 4: Idempotent — running twice yields identical row count
# ---------------------------------------------------------------------------

test_that("running append_git_commits_from_staging twice yields identical row count", {
  staging <- make_staging_dir_git(list(
    make_git_commit(project = "docs-gh-llmtelemetry", hash = "hash0001aaaa"),
    make_git_commit(project = "docs-gh-llmtelemetry", hash = "hash0002bbbb")
  ))
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-05-16 19:00:00", tz = "UTC")

  n1 <- append_git_commits_from_staging(staging_dir = staging,
                                         parquet_path = out_f, now = fixed_t)
  n2 <- append_git_commits_from_staging(staging_dir = staging,
                                         parquet_path = out_f, now = fixed_t)

  expect_equal(n1, 2L)
  expect_equal(n2, 0L)  # all already present on second run

  back <- read_parquet_git(out_f)
  expect_equal(nrow(back), 2L)
})

# ---------------------------------------------------------------------------
# Scenario 5: Non-git_commit event types are ignored
# ---------------------------------------------------------------------------

test_that("non-git_commit event types are ignored", {
  cost_event <- make_envelope_git(list(
    event_type     = "cost_emitted",
    project        = "docs-gh-llmtelemetry",
    date           = "2026-05-16",
    source         = "ccusage",
    daily_cost_usd = 1.50
  ))
  staging <- make_staging_dir_git(list(cost_event))
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  n <- append_git_commits_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-05-16 19:00:00", tz = "UTC")
  )

  expect_equal(n, 0L)
  expect_false(file.exists(out_f))
})

# ---------------------------------------------------------------------------
# Scenario 6: Multi-day staging files combine correctly
# ---------------------------------------------------------------------------

test_that("git_commit events from multiple daily JSONL files combine correctly", {
  staging <- withr::local_tempdir()

  # Day 1 file
  writeLines(
    jsonlite::toJSON(
      make_git_commit(project = "docs-gh-llmtelemetry",
                      hash = "day1commit001abc", date = "2026-05-15"),
      auto_unbox = TRUE
    ),
    file.path(staging, "events-2026-05-15.jsonl")
  )
  # Day 2 file
  writeLines(
    jsonlite::toJSON(
      make_git_commit(project = "docs-gh-llmtelemetry",
                      hash = "day2commit002def", date = "2026-05-16"),
      auto_unbox = TRUE
    ),
    file.path(staging, "events-2026-05-16.jsonl")
  )

  out_f <- withr::local_tempfile(fileext = ".parquet")

  n <- append_git_commits_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-05-16 19:00:00", tz = "UTC")
  )

  expect_equal(n, 2L)
  back <- read_parquet_git(out_f)
  expect_equal(nrow(back), 2L)
  expect_true("llmtelemetry|day1commit001abc" %in% back$commit_pk)
  expect_true("llmtelemetry|day2commit002def" %in% back$commit_pk)
})

# ---------------------------------------------------------------------------
# Scenario 7a: Malformed event missing required fields triggers warning
# ---------------------------------------------------------------------------

test_that("event missing project and hash is skipped with warning (all missing)", {
  bad_event <- make_envelope_git(list(
    event_type = "git_commit",
    date       = "2026-05-16",
    message    = "partial commit"
    # project and hash omitted
  ))
  staging <- make_staging_dir_git(list(bad_event))
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  expect_warning(
    n <- append_git_commits_from_staging(
      staging_dir  = staging,
      parquet_path = out_f,
      now          = as.POSIXct("2026-05-16 19:00:00", tz = "UTC")
    ),
    regexp = "missing required fields"
  )

  expect_equal(n, 0L)
  expect_false(file.exists(out_f))
})

# ---------------------------------------------------------------------------
# Scenario 7b: Mix of malformed and valid events — bad skipped, good appended
# ---------------------------------------------------------------------------

test_that("mix of malformed and valid events: bad skipped, good appended", {
  bad_event <- make_envelope_git(list(
    event_type = "git_commit",
    date       = "2026-05-16"
    # project and hash omitted
  ))
  good_event <- make_git_commit(project = "docs-gh-llmtelemetry",
                                 hash    = "goodhash999xyz")
  staging <- make_staging_dir_git(list(bad_event, good_event))
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  n <- append_git_commits_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-05-16 19:00:00", tz = "UTC")
  )

  expect_equal(n, 1L)
  back <- read_parquet_git(out_f)
  expect_equal(nrow(back), 1L)
  expect_equal(back$commit_pk, "llmtelemetry|goodhash999xyz")
})

# ---------------------------------------------------------------------------
# Scenario 8: lines_changed computed from added + deleted when absent
# ---------------------------------------------------------------------------

test_that("lines_changed is computed from lines_added + lines_deleted when absent", {
  event_no_changed <- make_envelope_git(list(
    event_type    = "git_commit",
    project       = "docs-gh-llmtelemetry",
    hash          = "nolineschanged001",
    date          = "2026-05-16",
    message       = "test",
    lines_added   = 50L,
    lines_deleted = 10L
    # lines_changed intentionally omitted
  ))
  staging <- make_staging_dir_git(list(event_no_changed))
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  n <- append_git_commits_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-05-16 19:00:00", tz = "UTC")
  )

  expect_equal(n, 1L)
  back <- read_parquet_git(out_f)
  expect_equal(back$lines_changed, 60L)  # 50 + 10
})
