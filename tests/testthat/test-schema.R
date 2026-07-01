test_that("schema_version() returns 'v1'", {
  expect_equal(schema_version(), "v1")
})

test_that("apply_schema_v1() creates the sessions table in an in-memory DuckDB", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  apply_schema_v1(con)

  tables <- DBI::dbListTables(con)
  expect_true("sessions" %in% tables)
})

test_that("open_telemetry_db(':memory:') returns a connection with the sessions table", {
  con <- open_telemetry_db(":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  tables <- DBI::dbListTables(con)
  expect_true("sessions" %in% tables)
})

test_that("sessions table has the v1 column set with correct types", {
  con <- open_telemetry_db(":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  cols <- DBI::dbGetQuery(con, "DESCRIBE sessions")

  expected_cols <- c(
    "session_id", "project", "canonical_project", "started_at", "ended_at",
    "duration_min", "agent", "source", "working_dir", "valid_from",
    "trigger"   # Phase 2 (#322): per-session provenance tag
  )
  expect_equal(sort(cols$column_name), sort(expected_cols))

  # Check key types
  col_types <- setNames(cols$column_type, cols$column_name)
  expect_equal(col_types[["session_id"]], "VARCHAR")
  expect_equal(col_types[["duration_min"]], "DOUBLE")
  expect_true(grepl("TIMESTAMP", col_types[["started_at"]]))
  expect_true(grepl("TIMESTAMP", col_types[["valid_from"]]))
})

test_that("sessions.valid_from is NOT NULL (enforced by schema)", {
  con <- open_telemetry_db(":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  # Attempt to insert a row with valid_from = NULL — should fail
  expect_error(
    DBI::dbExecute(
      con,
      "INSERT INTO sessions
         (session_id, project, canonical_project, started_at, ended_at,
          duration_min, agent, source, working_dir, valid_from)
       VALUES ('x', 'p', 'p', NULL, NULL, 0, NULL, 's', NULL, NULL)"
    )
  )
})

test_that("sessions table is empty after schema application (no pre-populated rows)", {
  con <- open_telemetry_db(":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM sessions")$n
  expect_equal(n, 0L)
})
