#' Backfill the v1 sessions parquet from unified_sessions.json
#'
#' Reads `unified_sessions.json`, transforms columns to the v1 sessions
#' schema, and writes a Parquet file using DuckDB's native COPY ... TO
#' (compression: zstd). The write is atomic: output is written to a temporary
#' file in the same directory, then renamed over the final path.
#'
#' `arrow` is not required; DuckDB's built-in Parquet writer is used instead.
#'
#' @param input_path Path to `unified_sessions.json`. Defaults to the package's
#'   `inst/extdata/unified_sessions.json`.
#' @param output_path Where to write `sessions.parquet`. Defaults to
#'   `inst/extdata/telemetry/v1/sessions.parquet`.
#' @param now Override the `valid_from` timestamp. Mainly useful in tests to
#'   produce deterministic output. Defaults to `Sys.time()`.
#' @return Invisibly returns a data frame with the transformed rows (v1 schema).
#' @export
rollup_sessions <- function(
  input_path  = here::here("inst", "extdata", "unified_sessions.json"),
  output_path = here::here("inst", "extdata", "telemetry", "v1", "sessions.parquet"),
  now = Sys.time()
) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  # --- 1. Read input --------------------------------------------------------
  raw <- jsonlite::fromJSON(input_path, simplifyDataFrame = TRUE)
  if (!is.data.frame(raw) || nrow(raw) == 0L) {
    stop("rollup_sessions: input has no rows (", input_path, ")")
  }

  # --- 2. Transform via duckplyr to v1 schema -------------------------------
  valid_from_ts <- as.POSIXct(now, tz = "UTC")

  out <- duckplyr::as_duckdb_tibble(raw) |>
    dplyr::transmute(
      session_id        = as.character(session_id),
      project           = as.character(project),
      canonical_project = as.character(canonical_project),
      started_at        = as.POSIXct(started_at, tz = "UTC"),
      ended_at          = as.POSIXct(ended_at, tz = "UTC"),
      duration_min      = as.numeric(duration_min),
      agent             = NA_character_,       # not present in unified_sessions
      source            = "unified_duckdb",
      working_dir       = NA_character_,
      valid_from        = valid_from_ts
    ) |>
    dplyr::collect()

  # --- 3. Atomic write via DuckDB COPY TO (no arrow dependency) --------------
  tmp_path <- paste0(output_path, ".tmp")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "sessions_export", out, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY sessions_export TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd')",
      tmp_path
    )
  )

  file.rename(tmp_path, output_path)

  invisible(out)
}
