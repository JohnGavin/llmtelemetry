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


#' Append staged git_commit events into the v1 git_commits parquet
#'
#' Reads JSONL files in the staging directory, filters to events with
#' `event_type == "git_commit"`, deduplicates by computed `commit_pk`
#' against the existing parquet, and atomically appends new rows.
#'
#' The composite key `commit_pk` is `paste(canonical_project, hash, sep="|")`,
#' matching the key used by `rollup_git_commits()` during the JSON backfill.
#' A staged event whose key already exists in the parquet is silently skipped.
#'
#' Idempotent: running twice over the same staging files yields identical row
#' counts in the parquet.  `arrow` is not required; all Parquet I/O uses
#' DuckDB's native reader/writer.
#'
#' @param staging_dir Path to staging directory.  Defaults to
#'   `~/.claude/logs/llmtelemetry-staging`.
#' @param parquet_path Path to git_commits.parquet.  Defaults to
#'   `inst/extdata/telemetry/v1/git_commits.parquet`.
#' @param now Override the `valid_from` timestamp (mainly for tests).
#'   Defaults to `Sys.time()`.
#' @return Invisibly: integer count of NEW rows appended.
#' @export
append_git_commits_from_staging <- function(
  staging_dir  = file.path(Sys.getenv("HOME"), ".claude", "logs",
                           "llmtelemetry-staging"),
  parquet_path = here::here("inst", "extdata", "telemetry", "v1",
                            "git_commits.parquet"),
  now          = Sys.time()
) {
  # --- 1. Read staged events -------------------------------------------------
  staged <- read_staging(staging_dir)
  if (nrow(staged) == 0L) return(invisible(0L))

  # --- 2. Filter to git_commit events ----------------------------------------
  is_git <- vapply(
    staged$payload,
    function(p) identical(p[["event_type"]], "git_commit"),
    logical(1L)
  )
  git_events <- staged[is_git, , drop = FALSE]
  if (nrow(git_events) == 0L) return(invisible(0L))

  # --- 3. Extract payload fields; skip events missing required fields --------
  chr_field <- function(payloads, field) {
    vapply(payloads, function(p) {
      v <- p[[field]]
      if (is.null(v)) NA_character_ else as.character(v)
    }, character(1L))
  }
  int_field <- function(payloads, field) {
    vapply(payloads, function(p) {
      v <- p[[field]]
      if (is.null(v)) NA_integer_ else as.integer(v)
    }, integer(1L))
  }

  raw_project   <- chr_field(git_events$payload, "project")
  raw_hash      <- chr_field(git_events$payload, "hash")
  raw_date      <- chr_field(git_events$payload, "date")
  message_col   <- chr_field(git_events$payload, "message")
  lines_added   <- int_field(git_events$payload, "lines_added")
  lines_deleted <- int_field(git_events$payload, "lines_deleted")
  files_changed <- int_field(git_events$payload, "files_changed")
  lines_changed <- int_field(git_events$payload, "lines_changed")

  # Compute lines_changed fallback: added + deleted when not supplied
  lines_changed <- ifelse(
    is.na(lines_changed),
    lines_added + lines_deleted,
    lines_changed
  )

  # Skip events where required fields (project, hash) are missing
  valid <- !is.na(raw_project) & !is.na(raw_hash) & nzchar(raw_hash)
  if (!any(valid)) {
    warning("append_git_commits_from_staging: all staged git_commit events ",
            "are missing required fields (project or hash) — nothing to append")
    return(invisible(0L))
  }
  git_events    <- git_events[valid, , drop = FALSE]
  raw_project   <- raw_project[valid]
  raw_hash      <- raw_hash[valid]
  raw_date      <- raw_date[valid]
  message_col   <- message_col[valid]
  lines_added   <- lines_added[valid]
  lines_deleted <- lines_deleted[valid]
  files_changed <- files_changed[valid]
  lines_changed <- lines_changed[valid]

  valid_from_ts      <- as.POSIXct(now, tz = "UTC")
  canonical_projects <- .canonicalize_project_local(raw_project)

  new_rows <- data.frame(
    commit_pk         = paste(canonical_projects, raw_hash, sep = "|"),
    project           = raw_project,
    canonical_project = canonical_projects,
    hash              = raw_hash,
    date              = as.Date(raw_date),
    message           = message_col,
    lines_added       = lines_added,
    lines_deleted     = lines_deleted,
    files_changed     = files_changed,
    lines_changed     = lines_changed,
    valid_from        = valid_from_ts,
    stringsAsFactors  = FALSE
  )

  # --- 3b. Sanitize project: replace raw filesystem paths (#1750) --------------
  # Raw project names from hooks use dash-form paths containing private paths.
  # Replace with canonical form.
  is_path <- grepl("^-|[/\\\\]", new_rows$project)
  if (any(is_path)) {
    new_rows$project[is_path] <- ifelse(
      is.na(new_rows$canonical_project[is_path]),
      "unknown",
      new_rows$canonical_project[is_path]
    )
  }

  # --- 4. Dedup against existing parquet ------------------------------------
  dir.create(dirname(parquet_path), recursive = TRUE, showWarnings = FALSE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  if (file.exists(parquet_path)) {
    existing_pks <- DBI::dbGetQuery(
      con,
      sprintf("SELECT commit_pk FROM read_parquet('%s')",
              gsub("'", "\\'", parquet_path, fixed = TRUE))
    )$commit_pk
    new_rows <- new_rows[!new_rows$commit_pk %in% existing_pks, , drop = FALSE]
  }

  if (nrow(new_rows) == 0L) return(invisible(0L))

  # --- 5. Atomic append: read existing + new -> .tmp -> rename ---------------
  combined <- if (file.exists(parquet_path)) {
    existing <- DBI::dbGetQuery(
      con,
      sprintf("SELECT * FROM read_parquet('%s')",
              gsub("'", "\\'", parquet_path, fixed = TRUE))
    )
    # Coerce POSIXct columns that DuckDB returns as numeric back to POSIXct
    if (!inherits(existing[["valid_from"]], "POSIXct")) {
      existing[["valid_from"]] <- as.POSIXct(existing[["valid_from"]],
                                             origin = "1970-01-01", tz = "UTC")
    }
    # Coerce date column back to Date if needed
    if (!inherits(existing[["date"]], "Date")) {
      existing[["date"]] <- as.Date(existing[["date"]], origin = "1970-01-01")
    }
    rbind(existing, new_rows)
  } else {
    new_rows
  }

  tmp_path <- paste0(parquet_path, ".tmp")
  DBI::dbWriteTable(con, "git_commits_combined", combined, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY git_commits_combined TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd')",
      tmp_path
    )
  )
  file.rename(tmp_path, parquet_path)

  invisible(nrow(new_rows))
}
