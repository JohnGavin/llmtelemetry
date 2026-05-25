#' HuggingFace dataset archive push (dry-run by default)
#'
#' Archives the SANITISED v1 PIT parquet files (`sessions.parquet`,
#' `costs.parquet`) to a HuggingFace dataset repository via the official
#' `huggingface-cli upload` command.  By default the function **only** performs
#' a dry-run: it validates the privacy guard, prints the file manifest and
#' target repository that WOULD be pushed, and returns invisibly without
#' touching the network.  A live push only happens with `push = TRUE`.
#'
#' @section Authentication:
#' The `hf` command authenticates automatically from the
#' `HF_TOKEN` environment variable — no git credential helper, no token
#' files, no ASKPASS script needed.  In CI, set the `HF_TOKEN` secret and
#' pass it to the step via `env: HF_TOKEN: ${{ secrets.HF_TOKEN }}`.
#'
#' @section CLI invocation:
#' The live push calls (after staging sanitised parquets to a temp dir):
#' ```
#' hf upload <repo_id> <staging_dir> . \
#'   --repo-type dataset \
#'   --commit-message "telemetry archive <date>"
#' ```
#' The CLI binary is resolved by `.hf_cli_binary()`: tries `hf` first
#' (`huggingface-cli` is deprecated and a non-functional no-op in current
#' `huggingface_hub`), then `huggingface-cli` as fallback for older installs.
#' Aborts if neither is found.
#'
#' @section Archive scope:
#' Only session metadata and cost aggregates are archived.  Raw conversation
#' logs, PHI, and `working_dir` path columns are never included.  The
#' mandatory privacy guard asserts zero confidential rows before any staging.
#'
#' @param sessions_parquet Path to the sessions PIT parquet.  Defaults to
#'   `inst/extdata/telemetry/v1/sessions.parquet` relative to the package
#'   root resolved via [here::here()].
#' @param costs_parquet Path to the costs PIT parquet.  Defaults to
#'   `inst/extdata/telemetry/v1/costs.parquet`.
#' @param hf_repo HuggingFace dataset repo identifier, e.g.
#'   `"JohnGavin/llmtelemetry-metrics"`.  May also be set via the
#'   environment variable `HF_DATASET_REPO`.  Defaults to
#'   `"JohnGavin/llmtelemetry-metrics"`.
#' @param push Logical.  `FALSE` (default) performs a dry-run: validate +
#'   print manifest but do NOT invoke `huggingface-cli` or touch the network.
#'   Set to `TRUE` only to fire the actual push.
#' @param workdir Temporary directory used for staging sanitised parquets.
#'   Created via [tempfile()] if `NULL`.  Deleted on exit (push mode only;
#'   dry-run never creates it).
#' @return Invisibly returns a named list with elements:
#'   \describe{
#'     \item{`dry_run`}{Logical.}
#'     \item{`hf_repo`}{The resolved repository identifier.}
#'     \item{`files`}{Character vector of parquet basenames staged.}
#'     \item{`sessions_rows`}{Row count after privacy guard.}
#'     \item{`costs_rows`}{Row count after privacy guard.}
#'   }
#' @export
hf_push_telemetry <- function(
  sessions_parquet = NULL,
  costs_parquet    = NULL,
  hf_repo          = NULL,
  push             = FALSE,
  workdir          = NULL
) {
  # --- 1. Resolve paths -------------------------------------------------------
  if (is.null(sessions_parquet)) {
    sessions_parquet <- here::here(
      "inst", "extdata", "telemetry", "v1", "sessions.parquet"
    )
  }
  if (is.null(costs_parquet)) {
    costs_parquet <- here::here(
      "inst", "extdata", "telemetry", "v1", "costs.parquet"
    )
  }
  if (is.null(hf_repo)) {
    hf_repo <- Sys.getenv("HF_DATASET_REPO", unset = "JohnGavin/llmtelemetry-metrics")
  }

  # --- 2. Check source files exist --------------------------------------------
  for (p in c(sessions_parquet, costs_parquet)) {
    if (!file.exists(p)) {
      cli::cli_abort(
        c("x" = "Source parquet not found: {.file {p}}",
          "i" = "Run {.code rollup_sessions()} and {.code rollup_costs()} first.")
      )
    }
  }

  # --- 3. Mandatory privacy guard BEFORE any staging -------------------------
  #
  # Load each parquet into R via DuckDB (no arrow dependency), run
  # drop_confidential_projects(), and assert that no rows would be dropped.
  # If any confidential row is detected we abort with a clear message —
  # the parquet must be regenerated via rollup_sessions() / rollup_costs()
  # which already apply the guard at write time.

  sessions_clean <- .hf_load_and_guard(sessions_parquet, "sessions")
  costs_clean    <- .hf_load_and_guard(costs_parquet,    "costs")

  # Drop working_dir column if present (path leak risk)
  sessions_clean <- .hf_drop_path_columns(sessions_clean)
  costs_clean    <- .hf_drop_path_columns(costs_clean)

  staged_files <- c("sessions.parquet", "costs.parquet")

  # --- 4. Dry-run: print manifest and return ----------------------------------
  if (!isTRUE(push)) {
    cli::cli_h1("HuggingFace archive push — DRY RUN")
    cli::cli_inform(c(
      "i" = "Target repo : {.val {hf_repo}}",
      "i" = "URL         : {.url https://huggingface.co/datasets/{hf_repo}}"
    ))
    cli::cli_h2("File manifest (would be staged)")
    for (f in staged_files) {
      cli::cli_inform("  - {.file {f}}")
    }
    cli::cli_h2("Row counts (after privacy guard)")
    cli::cli_inform(c(
      "*" = "sessions : {nrow(sessions_clean)} rows",
      "*" = "costs    : {nrow(costs_clean)} rows"
    ))
    cli::cli_inform(c(
      "v" = "Privacy guard: PASS (zero confidential rows in both files)",
      "i" = "No network call made. Rerun with {.code push = TRUE} to archive."
    ))

    return(invisible(list(
      dry_run      = TRUE,
      hf_repo      = hf_repo,
      files        = staged_files,
      sessions_rows = nrow(sessions_clean),
      costs_rows   = nrow(costs_clean)
    )))
  }

  # --- 5. Live push via huggingface-cli upload --------------------------------
  if (is.null(workdir)) {
    workdir <- tempfile(pattern = "hf_push_")
  }
  staging_dir <- file.path(workdir, "staging")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(workdir, recursive = TRUE), add = TRUE)

  # Write the sanitised parquets to the staging dir
  sessions_out <- file.path(staging_dir, "sessions.parquet")
  costs_out    <- file.path(staging_dir, "costs.parquet")
  .hf_write_parquet_duckdb(sessions_clean, sessions_out)
  .hf_write_parquet_duckdb(costs_clean,    costs_out)

  # Resolve the CLI binary: hf (preferred, current) or huggingface-cli (legacy fallback)
  cli_bin <- .hf_cli_binary()

  commit_msg <- sprintf(
    "telemetry archive %s — %s sessions, %s cost rows",
    format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    nrow(sessions_clean),
    nrow(costs_clean)
  )

  # huggingface-cli upload <repo_id> <local_folder> <path_in_repo>
  #   --repo-type dataset
  #   --commit-message "<msg>"
  #
  # The CLI reads HF_TOKEN from the environment automatically — no credential
  # helper, no ASKPASS script, no token on the command line.
  cli::cli_h1("Uploading to {.val {hf_repo}} ...")
  # suppressWarnings() silences the "running command ... had status N"
  # message that base R emits when system2() captures stdout AND the process
  # exits non-zero.  We check the exit status ourselves below.
  upload_result <- suppressWarnings(system2(
    cli_bin,
    args   = c(
      "upload",
      hf_repo,
      staging_dir,
      ".",
      "--repo-type", "dataset",
      "--commit-message", commit_msg
    ),
    stdout = TRUE,
    stderr = TRUE
  ))

  exit_status <- attr(upload_result, "status")
  if (!is.null(exit_status) && exit_status != 0L) {
    cli::cli_abort(
      c("x" = "{cli_bin} upload failed (exit {exit_status})",
        "i" = paste(upload_result, collapse = "\n"))
    )
  }

  cli::cli_inform(c("v" = "Push complete to {.val {hf_repo}}"))

  invisible(list(
    dry_run       = FALSE,
    hf_repo       = hf_repo,
    files         = staged_files,
    sessions_rows = nrow(sessions_clean),
    costs_rows    = nrow(costs_clean)
  ))
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Resolve the HuggingFace CLI binary.
#'
#' Tries `hf` first (the current binary name in `huggingface_hub` >= 0.24;
#' `huggingface-cli` is deprecated and a non-functional no-op in that version).
#' Falls back to `huggingface-cli` only when `hf` is absent (older installs).
#' Aborts with a clear message if neither is found on PATH.
#'
#' @return Character name of the binary to pass to [system2()].
#' @keywords internal
.hf_cli_binary <- function() {
  for (bin in c("hf", "huggingface-cli")) {
    # Sys.which() is a pure-R lookup — no subprocess, respects the current PATH,
    # and returns "" when the binary is absent (never throws).
    found <- Sys.which(bin)
    if (nzchar(found)) {
      return(bin)
    }
  }
  cli::cli_abort(
    c(
      "x" = "HuggingFace CLI not found on PATH.",
      "i" = paste0(
        "Install it with: {.code pip install --upgrade huggingface_hub}. ",
        "Ensure the resulting {.code hf} (or {.code huggingface-cli}) is on PATH."
      )
    )
  )
}


#' Load a parquet file via DuckDB and run the privacy guard.
#'
#' Aborts if any confidential row is present after the guard would filter.
#'
#' @param parquet_path Character path.
#' @param label Character label for error messages.
#' @return A data frame, confidential rows removed.
#' @keywords internal
.hf_load_and_guard <- function(parquet_path, label) {
  # Use DuckDB to read the parquet (no arrow dependency)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  sql <- sprintf("SELECT * FROM read_parquet('%s')",
                 gsub("'", "''", parquet_path))
  df_raw <- DBI::dbGetQuery(con, sql)

  n_before <- nrow(df_raw)
  df_clean <- drop_confidential_projects(df_raw)
  n_after  <- nrow(df_clean)

  if (n_after < n_before) {
    n_dropped <- n_before - n_after
    cli::cli_abort(
      c("x" = "Privacy guard FAILED for {.val {label}}.parquet",
        "i" = paste0("{n_dropped} confidential row(s) detected ",
                     "(before={n_before}, after={n_after})."),
        "i" = paste0("Regenerate the parquet via rollup_sessions() / ",
                     "rollup_costs() — they apply drop_confidential_projects() ",
                     "at write time and must produce a clean file."))
    )
  }

  df_clean
}


#' Drop path-leaking columns from a data frame.
#'
#' Removes `working_dir` (absolute filesystem paths) if present.
#'
#' @param df Data frame.
#' @return Data frame with path columns removed.
#' @keywords internal
.hf_drop_path_columns <- function(df) {
  path_cols <- c("working_dir")
  present   <- intersect(path_cols, names(df))
  if (length(present) > 0L) {
    df <- df[, setdiff(names(df), present), drop = FALSE]
  }
  df
}


#' Write a data frame to a Parquet file via DuckDB.
#'
#' Uses DuckDB's native COPY ... TO with ZSTD compression.  No arrow
#' dependency.  The write is NOT atomic (the caller manages temp dir cleanup).
#'
#' @param df Data frame.
#' @param out_path Character output path.
#' @keywords internal
.hf_write_parquet_duckdb <- function(df, out_path) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "export_tbl", df, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY export_tbl TO '%s' (FORMAT 'parquet', COMPRESSION 'zstd')",
      gsub("'", "''", out_path)
    )
  )
  invisible(out_path)
}
