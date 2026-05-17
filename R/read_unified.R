# Read from unified.duckdb — the primary telemetry database
# Uses DBI::dbReadTable() — full-table reads without raw SQL or dbplyr dependency

#' Read sessions from unified.duckdb
#'
#' @param db_path Path to unified.duckdb. Defaults to ~/.claude/logs/unified.duckdb.
#' @return tibble of session data
#' @export
read_unified_sessions <- function(db_path = file.path(Sys.getenv("HOME"), ".claude", "logs", "unified.duckdb")) {
  checkmate::assert_file_exists(db_path)
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbReadTable(con, "sessions") |>
    tibble::as_tibble() |>
    dplyr::mutate(
      started_at = as.POSIXct(started_at),
      ended_at = as.POSIXct(ended_at),
      date = as.Date(started_at)
    )
}

#' Read costs from unified.duckdb
#'
#' @inheritParams read_unified_sessions
#' @return tibble of cost data
#' @export
read_unified_costs <- function(db_path = file.path(Sys.getenv("HOME"), ".claude", "logs", "unified.duckdb")) {
  checkmate::assert_file_exists(db_path)
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  if (!"costs" %in% DBI::dbListTables(con)) return(tibble::tibble())
  DBI::dbReadTable(con, "costs") |> tibble::as_tibble()
}

#' Read agent runs from unified.duckdb
#'
#' @inheritParams read_unified_sessions
#' @return tibble of agent run data
#' @export
read_unified_agent_runs <- function(db_path = file.path(Sys.getenv("HOME"), ".claude", "logs", "unified.duckdb")) {
  checkmate::assert_file_exists(db_path)
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  if (!"agent_runs" %in% DBI::dbListTables(con)) return(tibble::tibble())
  DBI::dbReadTable(con, "agent_runs") |> tibble::as_tibble()
}

#' Summary of unified.duckdb contents
#'
#' @inheritParams read_unified_sessions
#' @return tibble with table name and row count
#' @export
unified_summary <- function(db_path = file.path(Sys.getenv("HOME"), ".claude", "logs", "unified.duckdb")) {
  checkmate::assert_file_exists(db_path)
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  tables <- DBI::dbListTables(con)
  purrr::map_dfr(tables, function(tbl_name) {
    n <- DBI::dbGetQuery(
      con, sprintf("SELECT count(*) FROM %s", tbl_name)
    )[[1]]
    tibble::tibble(table = tbl_name, rows = as.integer(n))
  })
}
