#' Backfill the v1 costs parquet from cost_by_project_estimated.json
#'
#' Reads `cost_by_project_estimated.json`, transforms columns to the v1 costs
#' schema, and writes a Parquet file using DuckDB's native COPY ... TO
#' (compression: zstd). The write is atomic: output is written to a temporary
#' file in the same directory, then renamed over the final path.
#'
#' `arrow` is not required; DuckDB's built-in Parquet writer is used instead.
#'
#' @param input_path Path to `cost_by_project_estimated.json`. Defaults to the
#'   package's `inst/extdata/cost_by_project_estimated.json`.
#' @param output_path Where to write `costs.parquet`. Defaults to
#'   `inst/extdata/telemetry/v1/costs.parquet`.
#' @param now Override the `valid_from` timestamp. Mainly useful in tests to
#'   produce deterministic output. Defaults to `Sys.time()`.
#' @return Invisibly returns a data frame with the transformed rows (v1 schema).
#' @export
rollup_costs <- function(
  input_path  = here::here("inst", "extdata", "cost_by_project_estimated.json"),
  output_path = here::here("inst", "extdata", "telemetry", "v1", "costs.parquet"),
  now = Sys.time()
) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  # --- 1. Read input --------------------------------------------------------
  raw <- jsonlite::fromJSON(input_path, simplifyDataFrame = TRUE)
  if (!is.data.frame(raw) || nrow(raw) == 0L) {
    stop("rollup_costs: input has no rows (", input_path, ")")
  }

  # --- 2. Transform via duckplyr to v1 schema -------------------------------
  valid_from_ts <- as.POSIXct(now, tz = "UTC")

  # cost_by_project_estimated.json columns:
  #   date, project, est_cost, duration_min, share, canonical_project
  out <- duckplyr::as_duckdb_tibble(raw) |>
    dplyr::transmute(
      cost_id           = paste(as.character(canonical_project),
                                as.character(date),
                                "estimated",
                                sep = "|"),
      project           = as.character(project),
      canonical_project = as.character(canonical_project),
      date              = as.Date(date),
      source            = "estimated",
      daily_cost_usd    = as.numeric(est_cost),
      n_sessions        = NA_integer_,
      duration_min      = as.numeric(duration_min),
      valid_from        = valid_from_ts
    ) |>
    dplyr::distinct(cost_id, .keep_all = TRUE) |>
    dplyr::collect()

  # --- 3. Atomic write via DuckDB COPY TO (no arrow dependency) --------------
  tmp_path <- paste0(output_path, ".tmp")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "costs_export", out, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY costs_export TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd')",
      tmp_path
    )
  )

  file.rename(tmp_path, output_path)

  invisible(out)
}
