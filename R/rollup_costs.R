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
  #
  # Multiple alias projects (e.g. "llm" and "llm/vignettes") may map to the
  # same canonical_project on the same day.  Using distinct(cost_id) would
  # silently drop those rows and undercount daily costs.  Instead we
  # group_by(canonical_project, date) and sum daily_cost_usd so that all
  # alias rows are correctly aggregated into a single canonical row.
  out <- duckplyr::as_duckdb_tibble(raw) |>
    dplyr::mutate(
      canonical_project = as.character(canonical_project),
      date              = as.Date(date),
      daily_cost_usd    = as.numeric(est_cost),
      duration_min      = as.numeric(duration_min)
    ) |>
    dplyr::group_by(canonical_project, date) |>
    dplyr::summarise(
      project        = dplyr::first(as.character(project)),
      daily_cost_usd = sum(daily_cost_usd, na.rm = TRUE),
      duration_min   = sum(duration_min, na.rm = TRUE),
      .groups        = "drop"
    ) |>
    dplyr::mutate(
      cost_id    = paste(canonical_project, as.character(date), "estimated", sep = "|"),
      source     = "estimated",
      n_sessions = NA_integer_,
      valid_from = valid_from_ts
    ) |>
    dplyr::select(cost_id, project, canonical_project, date, source,
                  daily_cost_usd, n_sessions, duration_min, valid_from) |>
    dplyr::collect()

  # Re-apply canonicalization to catch any stale pre-computed values in the JSON
  # (e.g. "demos", "wiki" confirmed as noise 2026-05-24).
  out$canonical_project <- .canonicalize_project_local(out$canonical_project)
  out <- out[!is.na(out$canonical_project), , drop = FALSE]

  # privacy: PIT store must never contain confidential projects (#83)
  out <- drop_confidential_projects(out)

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


#' Append staged cost_emitted events into the v1 costs parquet
#'
#' Reads JSONL files in the staging directory, filters to events with
#' `event_type == "cost_emitted"`, deduplicates by computed `cost_id`
#' against the existing parquet, and atomically appends new rows.
#'
#' The synthetic key `cost_id` is `paste(canonical_project, date, source, sep="|")`,
#' matching the key used by `rollup_costs()` during the JSON backfill.  A
#' staged event whose key already exists in the parquet is silently skipped.
#'
#' Idempotent: running twice over the same staging files yields identical row
#' counts in the parquet.  `arrow` is not required; all Parquet I/O uses
#' DuckDB's native reader/writer.
#'
#' @param staging_dir Path to staging directory.  Defaults to
#'   `~/.claude/logs/llmtelemetry-staging`.
#' @param parquet_path Path to costs.parquet.  Defaults to
#'   `inst/extdata/telemetry/v1/costs.parquet`.
#' @param now Override the `valid_from` timestamp (mainly for tests).
#'   Defaults to `Sys.time()`.
#' @return Invisibly: integer count of NEW rows appended.
#' @export
append_costs_from_staging <- function(
  staging_dir  = file.path(Sys.getenv("HOME"), ".claude", "logs",
                           "llmtelemetry-staging"),
  parquet_path = here::here("inst", "extdata", "telemetry", "v1",
                            "costs.parquet"),
  now          = Sys.time()
) {
  # --- 1. Read staged events -------------------------------------------------
  staged <- read_staging(staging_dir)
  if (nrow(staged) == 0L) return(invisible(0L))

  # --- 2. Filter to cost_emitted events -------------------------------------
  is_cost <- vapply(
    staged$payload,
    function(p) identical(p[["event_type"]], "cost_emitted"),
    logical(1L)
  )
  cost_events <- staged[is_cost, , drop = FALSE]
  if (nrow(cost_events) == 0L) return(invisible(0L))

  # --- 3. Extract payload fields; skip events missing required fields --------
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
  int_field <- function(payloads, field) {
    vapply(payloads, function(p) {
      v <- p[[field]]
      if (is.null(v)) NA_integer_ else as.integer(v)
    }, integer(1L))
  }

  raw_project    <- chr_field(cost_events$payload, "project")
  raw_date       <- chr_field(cost_events$payload, "date")
  raw_source     <- chr_field(cost_events$payload, "source")
  daily_cost_usd <- num_field(cost_events$payload, "daily_cost_usd")
  n_sessions     <- int_field(cost_events$payload, "n_sessions")
  duration_min   <- num_field(cost_events$payload, "duration_min")

  # Skip events where required fields (project, date, source) are missing
  valid <- !is.na(raw_project) & !is.na(raw_date) & !is.na(raw_source)
  if (!any(valid)) {
    warning("append_costs_from_staging: all staged cost_emitted events ",
            "are missing required fields (project, date, or source) — ",
            "nothing to append")
    return(invisible(0L))
  }
  cost_events    <- cost_events[valid, , drop = FALSE]
  raw_project    <- raw_project[valid]
  raw_date       <- raw_date[valid]
  raw_source     <- raw_source[valid]
  daily_cost_usd <- daily_cost_usd[valid]
  n_sessions     <- n_sessions[valid]
  duration_min   <- duration_min[valid]

  valid_from_ts      <- as.POSIXct(now, tz = "UTC")
  canonical_projects <- .canonicalize_project_local(raw_project)

  new_rows <- data.frame(
    cost_id           = paste(canonical_projects, raw_date, raw_source, sep = "|"),
    project           = raw_project,
    canonical_project = canonical_projects,
    date              = as.Date(raw_date),
    source            = raw_source,
    daily_cost_usd    = daily_cost_usd,
    n_sessions        = n_sessions,
    duration_min      = duration_min,
    valid_from        = valid_from_ts,
    stringsAsFactors  = FALSE
  )

  # --- 3b. Sanitize project: replace raw filesystem paths (#1750) --------------
  # Raw project names from hooks use dash-form paths (e.g. "-Users-johngavin-…")
  # which contain private filesystem paths. Replace with canonical form.
  is_path <- grepl("^-|[/\\\\]", new_rows$project)
  if (any(is_path)) {
    new_rows$project[is_path] <- ifelse(
      is.na(new_rows$canonical_project[is_path]),
      "unknown",
      new_rows$canonical_project[is_path]
    )
  }

  # --- 3c. Privacy filter: drop confidential projects from staging drain (#83) -
  # Mirror the guard applied in rollup_costs() on the JSON path so that the
  # staging drain can never write mycare / crypto / solwatch / swarms rows.
  new_rows <- drop_confidential_projects(new_rows)
  if (nrow(new_rows) == 0L) return(invisible(0L))

  # --- 4. Dedup against existing parquet ------------------------------------
  dir.create(dirname(parquet_path), recursive = TRUE, showWarnings = FALSE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  if (file.exists(parquet_path)) {
    existing_ids <- DBI::dbGetQuery(
      con,
      sprintf("SELECT cost_id FROM read_parquet('%s')",
              gsub("'", "\\'", parquet_path, fixed = TRUE))
    )$cost_id
    new_rows <- new_rows[!new_rows$cost_id %in% existing_ids, , drop = FALSE]
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
  DBI::dbWriteTable(con, "costs_combined", combined, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY costs_combined TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd')",
      tmp_path
    )
  )
  file.rename(tmp_path, parquet_path)

  invisible(nrow(new_rows))
}

# TODO(#83 Phase A): add costs_amendments.parquet SCD2 correction log — each
# amendment emits a new row with valid_from/valid_to/reason, never
# UPDATE/DELETE.  Mirror the design from the epic schema.
#
# DONE(#83 Phase A drain-privacy): append_costs_from_staging() now calls
# drop_confidential_projects() before dedup/write — the staging drain can no
# longer leak mycare/crypto/solwatch/swarms rows.
#
# DONE(#210): export_and_deploy_data.sh now invokes run_rollup.R after the
# JSON export, draining staging and committing fresh parquets every session.
