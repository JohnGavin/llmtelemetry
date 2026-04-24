# Read from unified.duckdb — the primary telemetry database
# All access via dplyr::tbl(), never raw SQL (duckdplyr-not-sql rule)

#' Read sessions from unified.duckdb
#'
#' @param db_path Path to unified.duckdb. Defaults to ~/.claude/logs/unified.duckdb.
#' @return tibble of session data
#' @export
read_unified_sessions <- function(db_path = file.path(Sys.getenv("HOME"), ".claude", "logs", "unified.duckdb")) {
  checkmate::assert_file_exists(db_path)
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dplyr::tbl(con, "sessions") |>
    dplyr::collect() |>
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
  dplyr::tbl(con, "costs") |>
    dplyr::collect()
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
  dplyr::tbl(con, "agent_runs") |>
    dplyr::collect()
}

#' Summary of unified.duckdb contents
#'
#' @inheritParams read_unified_sessions
#' @return tibble with table name, row count, and date range
#' @export
unified_summary <- function(db_path = file.path(Sys.getenv("HOME"), ".claude", "logs", "unified.duckdb")) {
  checkmate::assert_file_exists(db_path)
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  tables <- DBI::dbListTables(con)
  purrr::map_dfr(tables, function(tbl_name) {
    n <- dplyr::tbl(con, tbl_name) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::collect() |>
      dplyr::pull(n)
    tibble::tibble(table = tbl_name, rows = n)
  })
}
