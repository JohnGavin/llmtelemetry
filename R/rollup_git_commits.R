#' Backfill the v1 git_commits parquet from git_commits_by_project.json
#'
#' Reads `git_commits_by_project.json`, transforms columns to the v1
#' git_commits schema, and writes a Parquet file using DuckDB's native
#' COPY ... TO (compression: zstd). The write is atomic: output is written
#' to a temporary file in the same directory, then renamed over the final path.
#'
#' `arrow` is not required; DuckDB's built-in Parquet writer is used instead.
#'
#' The primary key `commit_pk` is a composite of `canonical_project|hash`
#' because short hashes can collide across repositories.
#'
#' @param input_path Path to `git_commits_by_project.json`. Defaults to the
#'   package's `inst/extdata/git_commits_by_project.json`.
#' @param output_path Where to write `git_commits.parquet`. Defaults to
#'   `inst/extdata/telemetry/v1/git_commits.parquet`.
#' @param now Override the `valid_from` timestamp. Mainly useful in tests to
#'   produce deterministic output. Defaults to `Sys.time()`.
#' @return Invisibly returns a data frame with the transformed rows (v1 schema).
#' @export
rollup_git_commits <- function(
  input_path  = here::here("inst", "extdata", "git_commits_by_project.json"),
  output_path = here::here("inst", "extdata", "telemetry", "v1", "git_commits.parquet"),
  now = Sys.time()
) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  # --- 1. Read input --------------------------------------------------------
  raw <- jsonlite::fromJSON(input_path, simplifyDataFrame = TRUE)
  if (!is.data.frame(raw) || nrow(raw) == 0L) {
    stop("rollup_git_commits: input has no rows (", input_path, ")")
  }

  # --- 2. Transform via duckplyr to v1 schema -------------------------------
  valid_from_ts <- as.POSIXct(now, tz = "UTC")

  # git_commits_by_project.json columns:
  #   project, hash, date, message, lines_added, lines_deleted,
  #   files_changed, lines_changed, canonical_project
  out <- duckplyr::as_duckdb_tibble(raw) |>
    dplyr::transmute(
      commit_pk         = paste(as.character(canonical_project),
                                as.character(hash),
                                sep = "|"),
      project           = as.character(project),
      canonical_project = as.character(canonical_project),
      hash              = as.character(hash),
      date              = as.Date(date),
      message           = as.character(message),
      lines_added       = as.integer(lines_added),
      lines_deleted     = as.integer(lines_deleted),
      files_changed     = as.integer(files_changed),
      lines_changed     = as.integer(lines_changed),
      valid_from        = valid_from_ts
    ) |>
    dplyr::distinct(commit_pk, .keep_all = TRUE) |>
    dplyr::collect()

  # --- 3. Atomic write via DuckDB COPY TO (no arrow dependency) --------------
  tmp_path <- paste0(output_path, ".tmp")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "git_commits_export", out, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY git_commits_export TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd')",
      tmp_path
    )
  )

  file.rename(tmp_path, output_path)

  invisible(out)
}
