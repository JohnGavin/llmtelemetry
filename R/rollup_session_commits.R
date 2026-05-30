#' Build a session-to-commit link table (v1 schema)
#'
#' Joins `sessions.parquet` to `git_commits.parquet` by matching commits whose
#' `date` falls within `[session.started_at::date, session.ended_at::date]` AND
#' whose `canonical_project` matches the session's `canonical_project`.
#'
#' The result is written atomically to `session_commits.parquet` (zstd).
#' Confidential projects are filtered via `drop_confidential_projects()` before
#' writing so the output can be committed to `inst/extdata/`.
#'
#' @section Schema:
#' | Column | Type | Notes |
#' |---|---|---|
#' | `session_id` | character | from sessions.parquet |
#' | `commit_sha` | character | short hash from git_commits.hash |
#' | `repo` | character | canonical_project from git_commits |
#' | `commit_at` | Date | git_commits.date (day granularity) |
#' | `duration_min` | double | session.duration_min |
#' | `agent` | character | session.agent (NA when unknown) |
#'
#' @param sessions_path Path to `sessions.parquet`. Defaults to the package's
#'   `inst/extdata/telemetry/v1/sessions.parquet`.
#' @param commits_path Path to `git_commits.parquet`. Defaults to the package's
#'   `inst/extdata/telemetry/v1/git_commits.parquet`.
#' @param output_path Where to write `session_commits.parquet`. Defaults to
#'   `inst/extdata/telemetry/v1/session_commits.parquet`.
#' @param now Override the `valid_from` timestamp. Mainly useful in tests.
#'   Defaults to `Sys.time()`.
#' @return Invisibly returns a data frame with the linked rows (v1 schema).
#' @export
rollup_session_commits <- function(
  sessions_path = here::here("inst", "extdata", "telemetry", "v1",
                              "sessions.parquet"),
  commits_path  = here::here("inst", "extdata", "telemetry", "v1",
                              "git_commits.parquet"),
  output_path   = here::here("inst", "extdata", "telemetry", "v1",
                              "session_commits.parquet"),
  now           = Sys.time()
) {
  if (!file.exists(sessions_path)) {
    cli::cli_abort(c(
      "x" = "sessions.parquet not found: {.path {sessions_path}}",
      "i" = "Run {.fn rollup_sessions} first."
    ))
  }
  if (!file.exists(commits_path)) {
    cli::cli_abort(c(
      "x" = "git_commits.parquet not found: {.path {commits_path}}",
      "i" = "Run {.fn rollup_git_commits} first."
    ))
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Register parquet files as views (no arrow dependency)
  DBI::dbExecute(
    con,
    sprintf(
      "CREATE VIEW sessions_v AS SELECT * FROM read_parquet('%s')",
      gsub("'", "\\'", sessions_path, fixed = TRUE)
    )
  )
  DBI::dbExecute(
    con,
    sprintf(
      "CREATE VIEW commits_v AS SELECT * FROM read_parquet('%s')",
      gsub("'", "\\'", commits_path, fixed = TRUE)
    )
  )

  # Join logic:
  #   - Match on canonical_project (session project = commit project)
  #   - Commit date must fall within [session.started_at::date, session.ended_at::date]
  #   - Sessions with NULL ended_at are excluded (incomplete sessions)
  #   - Sessions with NULL started_at are excluded (no time window)
  out_raw <- DBI::dbGetQuery(
    con,
    "
    SELECT
      s.session_id,
      c.hash          AS commit_sha,
      c.canonical_project AS repo,
      c.date          AS commit_at,
      s.duration_min,
      s.agent
    FROM sessions_v AS s
    INNER JOIN commits_v AS c
      ON  c.canonical_project = s.canonical_project
      AND c.date >= CAST(s.started_at AS DATE)
      AND c.date <= CAST(s.ended_at   AS DATE)
    WHERE s.started_at IS NOT NULL
      AND s.ended_at   IS NOT NULL
    ORDER BY s.session_id, c.date, c.hash
    "
  )

  if (nrow(out_raw) == 0L) {
    # Produce an empty parquet with the correct schema
    out <- data.frame(
      session_id  = character(0),
      commit_sha  = character(0),
      repo        = character(0),
      commit_at   = as.Date(character(0)),
      duration_min = numeric(0),
      agent       = character(0),
      valid_from  = as.POSIXct(character(0), tz = "UTC"),
      stringsAsFactors = FALSE
    )
  } else {
    valid_from_ts <- as.POSIXct(now, tz = "UTC")

    out <- data.frame(
      session_id   = as.character(out_raw$session_id),
      commit_sha   = as.character(out_raw$commit_sha),
      repo         = as.character(out_raw$repo),
      commit_at    = as.Date(out_raw$commit_at),
      duration_min = as.numeric(out_raw$duration_min),
      agent        = as.character(out_raw$agent),
      valid_from   = valid_from_ts,
      stringsAsFactors = FALSE
    )
  }

  # Privacy: drop confidential projects before writing
  # (uses a synthetic 'project' column as drop_confidential_projects looks at 'project')
  if (nrow(out) > 0L) {
    out$project <- out$repo
    out <- drop_confidential_projects(out)
    out$project <- NULL
  }

  # Atomic write: .tmp -> rename
  tmp_path <- paste0(output_path, ".tmp")
  DBI::dbWriteTable(con, "session_commits_export", out, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY session_commits_export TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd')",
      tmp_path
    )
  )
  file.rename(tmp_path, output_path)

  invisible(out)
}
