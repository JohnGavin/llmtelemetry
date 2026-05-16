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


# Internal helper: derive canonical_project from a raw project path string.
# Mirrors canonicalize_session_project() in inst/scripts/export_dashboard_data.R
# but kept local so this function has no source()-time side effects.
# Hook-emitted project names arrive in dash-form (e.g. "docs-gh-llmtelemetry");
# shorten_project() strips the common prefix before canonicalize_project() runs.
# TODO: factor into a proper package function once epic #83 stabilises.
.shorten_project_local <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)
  x <- gsub("^-Users-johngavin-docs[-_]gh-", "", x)
  x <- gsub("^docs[-_]gh[-_]",               "", x)
  x <- gsub("^llm-",                          "", x)
  x <- gsub("^proj-",                         "", x)
  x <- gsub("-", "/", x, fixed = TRUE)
  x
}

.canonicalize_project_local <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(name)) return(NA_character_)
  # Convert dash-form project names emitted by the hook to slash-form first
  name <- .shorten_project_local(name)

  meta_only <- c(
    "sonnet", "roborev", "worktree",
    "antigravity", "crypto", "data", "github", "hello",
    "knowledge", "simulations", "sport", "subagents",
    "t", "io", "urban_planning", "notmineraft", "telemetry", "football"
  )
  if (name %in% meta_only) return(NA_character_)

  name <- sub("^[A-Za-z0-9_]{8,}/repo/?", "", name)
  if (!nzchar(name)) return(NA_character_)

  overrides <- list("buoy/network" = "irish_buoy_network")
  for (pat in names(overrides)) {
    if (startsWith(name, pat)) return(overrides[[pat]])
  }

  container_prefixes <- c(
    "worktree/", "simulations/", "sport/", "data/", "crypto/",
    "subagents/", "knowledge/", "github/"
  )
  for (pfx in container_prefixes) {
    if (startsWith(name, pfx)) {
      name <- sub(paste0("^", pfx), "", name)
      break
    }
  }
  if (!nzchar(name)) return(NA_character_)

  parts <- strsplit(name, "/", fixed = TRUE)[[1]]
  if (length(parts) == 0L || !nzchar(parts[1L])) return(NA_character_)
  first <- parts[1L]
  if (grepl("^[0-9]+$", first)) return(NA_character_)
  if (first %in% meta_only) return(NA_character_)
  first
}
.canonicalize_project_local <- Vectorize(.canonicalize_project_local,
                                         USE.NAMES = FALSE)


#' Append staged session_stop events into the v1 sessions parquet
#'
#' Reads JSONL files in the staging directory, filters to events with
#' `event_type == "session_stop"`, deduplicates by `session_id` against the
#' existing parquet, and atomically appends new rows.
#'
#' Idempotent: running twice over the same staging files yields identical row
#' counts in the parquet.  `arrow` is not required; all Parquet I/O uses
#' DuckDB's native reader/writer.
#'
#' @param staging_dir Path to staging directory.  Defaults to
#'   `~/.claude/logs/llmtelemetry-staging`.
#' @param parquet_path Path to sessions.parquet.  Defaults to
#'   `inst/extdata/telemetry/v1/sessions.parquet`.
#' @param now Override the `valid_from` timestamp (mainly for tests).
#'   Defaults to `Sys.time()`.
#' @return Invisibly: integer count of NEW rows appended.
#' @export
append_sessions_from_staging <- function(
  staging_dir  = file.path(Sys.getenv("HOME"), ".claude", "logs",
                           "llmtelemetry-staging"),
  parquet_path = here::here("inst", "extdata", "telemetry", "v1",
                            "sessions.parquet"),
  now          = Sys.time()
) {
  # --- 1. Read staged events -------------------------------------------------
  staged <- read_staging(staging_dir)
  if (nrow(staged) == 0L) return(invisible(0L))

  # --- 2. Filter to session_stop events -------------------------------------
  is_stop <- vapply(
    staged$payload,
    function(p) identical(p[["event_type"]], "session_stop"),
    logical(1L)
  )
  session_events <- staged[is_stop, , drop = FALSE]
  if (nrow(session_events) == 0L) return(invisible(0L))

  # --- 3. Extract payload fields; skip events with missing session_id --------
  raw_ids <- vapply(
    session_events$payload,
    function(p) {
      id <- p[["session_id"]]
      if (is.null(id) || !nzchar(id)) NA_character_ else as.character(id)
    },
    character(1L)
  )
  valid <- !is.na(raw_ids)
  if (!any(valid)) {
    warning("append_sessions_from_staging: all staged session_stop events ",
            "are missing session_id — nothing to append")
    return(invisible(0L))
  }
  session_events <- session_events[valid, , drop = FALSE]
  raw_ids        <- raw_ids[valid]

  # Helper: extract a character field from payload, defaulting to NA
  chr_field <- function(payloads, field) {
    vapply(payloads, function(p) {
      v <- p[[field]]
      if (is.null(v)) NA_character_ else as.character(v)
    }, character(1L))
  }
  num_field <- function(payloads, field) {
    vapply(payloads, function(p) {
      v <- p[[field]]
      if (is.null(v)) NA_real_ else as.numeric(v)
    }, numeric(1L))
  }

  raw_project  <- chr_field(session_events$payload, "project")
  started_at_s <- chr_field(session_events$payload, "started_at")
  ended_at_s   <- chr_field(session_events$payload, "ended_at")
  duration_min <- num_field(session_events$payload, "duration_min")
  agent        <- chr_field(session_events$payload, "agent")
  source_col   <- chr_field(session_events$payload, "source")
  source_col   <- ifelse(is.na(source_col), "claude-code-hook", source_col)
  working_dir  <- chr_field(session_events$payload, "working_dir")

  valid_from_ts <- as.POSIXct(now, tz = "UTC")

  new_rows <- data.frame(
    session_id        = raw_ids,
    project           = raw_project,
    canonical_project = .canonicalize_project_local(raw_project),
    started_at        = as.POSIXct(started_at_s,
                                   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    ended_at          = as.POSIXct(ended_at_s,
                                   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    duration_min      = duration_min,
    agent             = agent,
    source            = source_col,
    working_dir       = working_dir,
    valid_from        = valid_from_ts,
    stringsAsFactors  = FALSE
  )

  # --- 4. Dedup against existing parquet ------------------------------------
  dir.create(dirname(parquet_path), recursive = TRUE, showWarnings = FALSE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  if (file.exists(parquet_path)) {
    existing_ids <- DBI::dbGetQuery(
      con,
      sprintf("SELECT session_id FROM read_parquet('%s')",
              gsub("'", "\\'", parquet_path, fixed = TRUE))
    )$session_id
    new_rows <- new_rows[!new_rows$session_id %in% existing_ids, , drop = FALSE]
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
    for (col in c("started_at", "ended_at", "valid_from")) {
      if (!inherits(existing[[col]], "POSIXct")) {
        existing[[col]] <- as.POSIXct(existing[[col]], origin = "1970-01-01",
                                      tz = "UTC")
      }
    }
    rbind(existing, new_rows)
  } else {
    new_rows
  }

  tmp_path <- paste0(parquet_path, ".tmp")
  DBI::dbWriteTable(con, "sessions_combined", combined, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY sessions_combined TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd')",
      tmp_path
    )
  )
  file.rename(tmp_path, parquet_path)

  invisible(nrow(new_rows))
}
