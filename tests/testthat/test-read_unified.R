# Tests for read_unified.R — unified.duckdb reader functions
# Uses a temporary DuckDB with the schema from unified_log_init.sql

make_test_db <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE sessions (
      session_id VARCHAR PRIMARY KEY,
      project VARCHAR,
      started_at TIMESTAMP,
      ended_at TIMESTAMP,
      duration_min DOUBLE,
      model VARCHAR,
      burn_status VARCHAR
    )")
  DBI::dbExecute(con, "
    CREATE TABLE costs (
      date DATE PRIMARY KEY,
      opus_cost DOUBLE, sonnet_cost DOUBLE, haiku_cost DOUBLE,
      total_cost DOUBLE, opus_pct DOUBLE, sonnet_pct DOUBLE, haiku_pct DOUBLE
    )")
  DBI::dbExecute(con, "
    CREATE TABLE agent_runs (
      id INTEGER PRIMARY KEY,
      session_id VARCHAR,
      agent_type VARCHAR,
      started_at TIMESTAMP,
      status VARCHAR
    )")
  DBI::dbAppendTable(con, "sessions", data.frame(
    session_id = c("s1", "s2"),
    project    = c("llm", "mycare"),
    started_at = as.POSIXct(c("2026-05-01 09:00:00", "2026-05-02 10:00:00")),
    ended_at   = as.POSIXct(c("2026-05-01 09:30:00", "2026-05-02 10:45:00")),
    duration_min = c(30, 45),
    model      = c("claude-sonnet-4-6", "claude-opus-4-7"),
    burn_status = c("ok", "WARN"),
    stringsAsFactors = FALSE
  ))
  DBI::dbAppendTable(con, "costs", data.frame(
    date = as.Date(c("2026-05-01", "2026-05-02")),
    opus_cost = c(0.5, 1.2), sonnet_cost = c(0.3, 0.4), haiku_cost = c(0.1, 0.0),
    total_cost = c(0.9, 1.6), opus_pct = c(55.6, 75.0),
    sonnet_pct = c(33.3, 25.0), haiku_pct = c(11.1, 0.0),
    stringsAsFactors = FALSE
  ))
  invisible(path)
}

# ---- read_unified_sessions() -------------------------------------------------

test_that("read_unified_sessions returns tibble with expected columns", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  make_test_db(db)
  result <- read_unified_sessions(db)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("session_id", "project", "started_at", "ended_at",
                     "duration_min", "date") %in% names(result)))
})

test_that("read_unified_sessions returns 2 rows from test db", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  make_test_db(db)
  result <- read_unified_sessions(db)
  expect_equal(nrow(result), 2L)
})

test_that("read_unified_sessions coerces started_at to POSIXct and adds date column", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  make_test_db(db)
  result <- read_unified_sessions(db)
  expect_s3_class(result$started_at, "POSIXct")
  expect_s3_class(result$date, "Date")
})

test_that("read_unified_sessions errors on missing db file", {
  expect_error(read_unified_sessions("/tmp/does_not_exist.duckdb"))
})

# ---- read_unified_costs() ---------------------------------------------------

test_that("read_unified_costs returns tibble from test db", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  make_test_db(db)
  result <- read_unified_costs(db)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2L)
})

test_that("read_unified_costs returns empty tibble when costs table absent", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  # Create db with only sessions table (no costs)
  con <- DBI::dbConnect(duckdb::duckdb(), db)
  DBI::dbExecute(con, "CREATE TABLE sessions (session_id VARCHAR PRIMARY KEY)")
  DBI::dbDisconnect(con, shutdown = TRUE)
  result <- read_unified_costs(db)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("read_unified_costs errors on missing db file", {
  expect_error(read_unified_costs("/tmp/does_not_exist.duckdb"))
})

# ---- read_unified_agent_runs() ----------------------------------------------

test_that("read_unified_agent_runs returns empty tibble when table absent", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  # DB with sessions only, no agent_runs
  con <- DBI::dbConnect(duckdb::duckdb(), db)
  DBI::dbExecute(con, "CREATE TABLE sessions (session_id VARCHAR PRIMARY KEY)")
  DBI::dbDisconnect(con, shutdown = TRUE)
  result <- read_unified_agent_runs(db)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("read_unified_agent_runs returns tibble with rows when table present", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  make_test_db(db)
  result <- read_unified_agent_runs(db)
  expect_s3_class(result, "tbl_df")
})

# ---- unified_summary() -------------------------------------------------------

test_that("unified_summary returns row for each table", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  make_test_db(db)
  result <- unified_summary(db)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("table", "rows") %in% names(result)))
  expect_true("sessions" %in% result$table)
  expect_true("costs" %in% result$table)
})

test_that("unified_summary row counts match inserted data", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  make_test_db(db)
  result <- unified_summary(db)
  expect_equal(result$rows[result$table == "sessions"], 2L)
  expect_equal(result$rows[result$table == "costs"], 2L)
})
