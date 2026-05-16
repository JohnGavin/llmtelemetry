#' Return the current schema version string
#'
#' @return A character string, currently `"v1"`.
#' @export
schema_version <- function() "v1"

#' Apply the v1 schema DDL to a DuckDB connection
#'
#' Reads all `.sql` files from `inst/schema/v1/` (installed package) or
#' `inst/schema/v1/` relative to the project root during development, and
#' executes each statement against `con`.
#'
#' @param con A DBI connection to a DuckDB database.
#' @return Invisibly returns `NULL`.
#' @export
apply_schema_v1 <- function(con) {
  ddl_dir <- system.file("schema", "v1", package = "llmtelemetry")
  if (!nzchar(ddl_dir)) {
    # dev fallback: relative to project root
    ddl_dir <- here::here("inst", "schema", "v1")
  }
  sql_files <- list.files(ddl_dir, pattern = "\\.sql$", full.names = TRUE)
  for (f in sql_files) {
    DBI::dbExecute(con, paste(readLines(f, warn = FALSE), collapse = "\n"))
  }
  invisible(NULL)
}

#' Open a DuckDB connection with the v1 schema applied
#'
#' Connects to the specified DuckDB database path, calls [apply_schema_v1()],
#' and returns the open connection. The caller is responsible for closing the
#' connection with [DBI::dbDisconnect()].
#'
#' @param path Path to the DuckDB database file. Defaults to `":memory:"` for
#'   an in-memory database.
#' @return A DBI connection object with the v1 schema applied.
#' @export
open_telemetry_db <- function(path = ":memory:") {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  apply_schema_v1(con)
  con
}
