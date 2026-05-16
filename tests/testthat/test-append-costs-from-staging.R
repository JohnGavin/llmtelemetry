# Tests for append_costs_from_staging()
#
# Mirrors the 7-scenario suite from test-append-from-staging.R (Phase 1E),
# adapted for cost_emitted events and dedup by cost_id.
# DuckDB native I/O throughout (no arrow dependency).

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_staging_dir_costs <- function(events = list()) {
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

make_envelope_costs <- function(payload) {
  list(
    ts      = "2026-05-16T18:30:00Z",
    host    = "testhost",
    pid     = "99999",
    payload = payload
  )
}

make_cost_emitted <- function(
  project        = "docs-gh-llmtelemetry",
  date           = "2026-05-16",
  source         = "ccusage",
  daily_cost_usd = 1.50,
  n_sessions     = 3L,
  duration_min   = 90.0
) {
  make_envelope_costs(list(
    event_type     = "cost_emitted",
    project        = project,
    date           = date,
    source         = source,
    daily_cost_usd = daily_cost_usd,
    n_sessions     = n_sessions,
    duration_min   = duration_min
  ))
}

read_parquet_costs <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s')", path))
}

# Write a minimal pre-existing costs parquet (simulating a JSON backfill row)
write_seed_costs_parquet <- function(cost_ids, parquet_path) {
  rows <- data.frame(
    cost_id           = cost_ids,
    project           = rep("docs-gh-llm", length(cost_ids)),
    canonical_project = rep("llm", length(cost_ids)),
    date              = as.Date("2026-01-01"),
    source            = rep("estimated", length(cost_ids)),
    daily_cost_usd    = rep(1.0, length(cost_ids)),
    n_sessions        = NA_integer_,
    duration_min      = rep(60.0, length(cost_ids)),
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

  n <- append_costs_from_staging(
    staging_dir  = staging,
    parquet_path = out_file,
    now          = as.POSIXct("2026-05-16 18:30:00", tz = "UTC")
  )

  expect_equal(n, 0L)
  expect_false(file.exists(out_file))
})

# ---------------------------------------------------------------------------
# Scenario 2: Single cost_emitted event writes one row with correct v1 schema
# ---------------------------------------------------------------------------

test_that("single cost_emitted event appends one row with all v1 columns", {
  staging <- make_staging_dir_costs(list(make_cost_emitted()))
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-05-16 19:00:00", tz = "UTC")

  n <- append_costs_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = fixed_t
  )

  expect_equal(n, 1L)
  expect_true(file.exists(out_f))

  back <- read_parquet_costs(out_f)
  expect_equal(nrow(back), 1L)

  expected_cols <- c(
    "cost_id", "project", "canonical_project", "date", "source",
    "daily_cost_usd", "n_sessions", "duration_min", "valid_from"
  )
  expect_equal(sort(names(back)), sort(expected_cols))

  # cost_id is paste(canonical_project, date, source, sep="|")
  expect_equal(back$cost_id, "llmtelemetry|2026-05-16|ccusage")
  expect_equal(back$canonical_project, "llmtelemetry")
  expect_equal(back$source, "ccusage")
  expect_equal(back$daily_cost_usd, 1.50)
  # valid_from equals the 'now' argument
  expect_true(abs(as.numeric(as.POSIXct(back$valid_from, tz = "UTC")) -
                    as.numeric(fixed_t)) < 2)
})

# ---------------------------------------------------------------------------
# Scenario 3: Dedup against backfill (cost_id already in parquet not re-written)
# ---------------------------------------------------------------------------

test_that("dedup: existing cost_id in parquet is not re-appended", {
  out_f <- withr::local_tempfile(fileext = ".parquet")
  # Seed parquet with a cost_id that matches the staged event
  # cost_id = paste("llmtelemetry", "2026-01-15", "estimated", sep="|")
  existing_id <- "llmtelemetry|2026-01-15|estimated"
  write_seed_costs_parquet(existing_id, out_f)

  # Staging contains two events: one duplicate (same key), one new
  dup_event <- make_cost_emitted(
    project = "docs-gh-llmtelemetry",
    date    = "2026-01-15",
    source  = "estimated"   # -> cost_id = "llmtelemetry|2026-01-15|estimated"
  )
  new_event <- make_cost_emitted(
    project = "docs-gh-llmtelemetry",
    date    = "2026-05-16",
    source  = "ccusage"     # -> cost_id = "llmtelemetry|2026-05-16|ccusage"
  )
  staging <- make_staging_dir_costs(list(dup_event, new_event))

  n <- append_costs_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-05-16 19:00:00", tz = "UTC")
  )

  expect_equal(n, 1L)

  back <- read_parquet_costs(out_f)
  expect_equal(nrow(back), 2L)  # 1 seed + 1 new
  expect_true("llmtelemetry|2026-05-16|ccusage" %in% back$cost_id)
  # Duplicate appears exactly once
  expect_equal(sum(back$cost_id == existing_id), 1L)
})

# ---------------------------------------------------------------------------
# Scenario 4: Idempotent — running twice yields identical row count
# ---------------------------------------------------------------------------

test_that("running append_costs_from_staging twice yields identical row count", {
  staging <- make_staging_dir_costs(list(
    make_cost_emitted(project = "docs-gh-llmtelemetry", date = "2026-05-14",
                      source = "ccusage"),
    make_cost_emitted(project = "docs-gh-llmtelemetry", date = "2026-05-15",
                      source = "ccusage")
  ))
  out_f   <- withr::local_tempfile(fileext = ".parquet")
  fixed_t <- as.POSIXct("2026-05-16 19:00:00", tz = "UTC")

  n1 <- append_costs_from_staging(staging_dir = staging,
                                   parquet_path = out_f, now = fixed_t)
  n2 <- append_costs_from_staging(staging_dir = staging,
                                   parquet_path = out_f, now = fixed_t)

  expect_equal(n1, 2L)
  expect_equal(n2, 0L)  # all already present on second run

  back <- read_parquet_costs(out_f)
  expect_equal(nrow(back), 2L)
})

# ---------------------------------------------------------------------------
# Scenario 5: Non-cost_emitted event types are ignored
# ---------------------------------------------------------------------------

test_that("non-cost_emitted event types are ignored", {
  session_event <- make_envelope_costs(list(
    event_type   = "session_stop",
    session_id   = "sess-abc",
    project      = "docs-gh-llmtelemetry",
    duration_min = 30
  ))
  staging <- make_staging_dir_costs(list(session_event))
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  n <- append_costs_from_staging(
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

test_that("cost events from multiple daily JSONL files combine correctly", {
  staging <- withr::local_tempdir()

  # Day 1 file
  writeLines(
    jsonlite::toJSON(
      make_cost_emitted(project = "docs-gh-llmtelemetry", date = "2026-05-15",
                        source = "ccusage"),
      auto_unbox = TRUE
    ),
    file.path(staging, "events-2026-05-15.jsonl")
  )
  # Day 2 file
  writeLines(
    jsonlite::toJSON(
      make_cost_emitted(project = "docs-gh-llmtelemetry", date = "2026-05-16",
                        source = "ccusage"),
      auto_unbox = TRUE
    ),
    file.path(staging, "events-2026-05-16.jsonl")
  )

  out_f <- withr::local_tempfile(fileext = ".parquet")

  n <- append_costs_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-05-16 19:00:00", tz = "UTC")
  )

  expect_equal(n, 2L)
  back <- read_parquet_costs(out_f)
  expect_equal(nrow(back), 2L)
  expect_true("llmtelemetry|2026-05-15|ccusage" %in% back$cost_id)
  expect_true("llmtelemetry|2026-05-16|ccusage" %in% back$cost_id)
})

# ---------------------------------------------------------------------------
# Scenario 7a: Malformed event missing required field skipped with warning
# ---------------------------------------------------------------------------

test_that("event missing required fields is skipped (warning when all missing)", {
  # Event with no project, date, or source — all required fields absent
  bad_event <- make_envelope_costs(list(
    event_type     = "cost_emitted",
    daily_cost_usd = 2.00
    # project, date, source all omitted
  ))
  staging <- make_staging_dir_costs(list(bad_event))
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  expect_warning(
    n <- append_costs_from_staging(
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
  bad_event <- make_envelope_costs(list(
    event_type     = "cost_emitted",
    daily_cost_usd = 2.00
    # project, date, source all omitted
  ))
  good_event <- make_cost_emitted(project = "docs-gh-llmtelemetry",
                                   date    = "2026-05-16",
                                   source  = "ccusage")
  staging <- make_staging_dir_costs(list(bad_event, good_event))
  out_f   <- withr::local_tempfile(fileext = ".parquet")

  n <- append_costs_from_staging(
    staging_dir  = staging,
    parquet_path = out_f,
    now          = as.POSIXct("2026-05-16 19:00:00", tz = "UTC")
  )

  expect_equal(n, 1L)
  back <- read_parquet_costs(out_f)
  expect_equal(nrow(back), 1L)
  expect_equal(back$cost_id, "llmtelemetry|2026-05-16|ccusage")
})
