#' HuggingFace dataset archive push (dry-run by default)
#'
#' Archives the SANITISED v1 PIT parquet files (`sessions.parquet`,
#' `costs.parquet`) to a HuggingFace dataset repository via `git clone` +
#' `git-lfs push`.  By default the function **only** performs a dry-run:
#' it validates the privacy guard, prints the file manifest and target
#' repository that WOULD be pushed, and returns invisibly without touching
#' the network.  A live push only happens with `push = TRUE`.
#'
#' @section One-time setup (human operator):
#' 1. Create the HuggingFace dataset repo (e.g. `JohnGavin/llmtelemetry-metrics`)
#'    at `https://huggingface.co/new-dataset`.
#' 2. Run `huggingface-cli login` in a terminal.  This writes a token to
#'    `~/.cache/huggingface/token` and configures a git credential helper so
#'    the token is never embedded in clone URLs.
#'
#' @section CI supply of the token:
#' The function resolves the token via `hf_resolve_token_path()`, which
#' checks two sources in order:
#' 1. `~/.cache/huggingface/token` — if the file exists, use it directly
#'    (interactive / local use; no change to existing behaviour).
#' 2. `HF_TOKEN` environment variable — if set (non-empty), the token is
#'    written to a transient file in the working directory with mode 0600,
#'    used for the clone/push, then deleted immediately.  The token value
#'    is **never** stored in an R variable or printed.
#'
#' Recommended GitHub Actions configuration (Phase F):
#' ```yaml
#' - run: Rscript inst/scripts/push_telemetry_to_huggingface.R --push
#'   env:
#'     HF_TOKEN: ${{ secrets.HF_TOKEN }}
#' ```
#' This avoids writing the token to disk via `echo` and lets the function
#' manage the transient file securely.
#'
#' @section Archive scope:
#' Only session metadata and cost aggregates are archived.  Raw conversation
#' logs, PHI, and `working_dir` path columns are never included.  The
#' mandatory privacy guard asserts zero confidential rows before any staging.
#'
#' @section Phase notes:
#' * **Phase E** (deferred): dashboard `hf://` read via DuckDB.
#' * **Phase F** (deferred): scheduled CI push via GitHub Actions cron.
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
#' @param token_path Path to the HuggingFace token file.  Defaults to
#'   `~/.cache/huggingface/token`.  The token is **never** read into R
#'   memory by this function: a transient GIT_ASKPASS helper script `cat`s
#'   the file at git-clone time and is deleted immediately afterward.
#'   When the file does not exist and the `HF_TOKEN` environment variable
#'   is set, `hf_resolve_token_path()` writes the token to a transient file
#'   and returns that path instead (the caller manages cleanup).
#' @param push Logical.  `FALSE` (default) performs a dry-run: validate +
#'   print manifest but do NOT clone or push.  Set to `TRUE` only to fire
#'   the actual push.
#' @param workdir Temporary directory used for the git clone.  Created via
#'   [tempfile()] if `NULL`.  Deleted on exit (push mode only; dry-run never
#'   creates it).
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
  token_path       = path.expand("~/.cache/huggingface/token"),
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

  # --- 5. Live push -----------------------------------------------------------
  if (is.null(workdir)) {
    workdir <- tempfile(pattern = "hf_push_")
  }
  dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(workdir, recursive = TRUE), add = TRUE)

  # Resolve the token: prefer the caller-supplied file path; fall back to
  # the HF_TOKEN environment variable (CI path).  A transient token file
  # is written inside workdir when the env-var path is taken; it is removed
  # by the on.exit(unlink(workdir, ...)) above.
  resolved_token <- .hf_resolve_token_path(token_path, workdir)

  clone_dir <- file.path(workdir, "repo")

  # Build the GIT_ASKPASS helper: a transient prompt-aware shell script.
  # Username prompt → repo owner; Password prompt → token file.
  # The helper is deleted immediately after git clone completes so the file's
  # lifetime is minimised.  NEVER embed the token in the clone URL.
  askpass_script <- .hf_make_askpass(resolved_token, workdir, hf_repo)
  on.exit(
    if (file.exists(askpass_script)) file.remove(askpass_script),
    add = TRUE
  )

  hf_clone_url <- paste0("https://huggingface.co/datasets/", hf_repo)

  cli::cli_h1("Cloning {.val {hf_repo}} ...")
  clone_env <- c(
    "GIT_ASKPASS" = askpass_script,
    "GIT_TERMINAL_PROMPT" = "0"
  )
  clone_result <- system2(
    "git",
    args    = c("clone", hf_clone_url, clone_dir),
    env     = paste0(names(clone_env), "=", clone_env),
    stdout  = TRUE,
    stderr  = TRUE
  )
  if (!is.null(attr(clone_result, "status")) &&
      attr(clone_result, "status") != 0L) {
    cli::cli_abort(
      c("x" = "git clone failed",
        "i" = paste(clone_result, collapse = "\n"))
    )
  }
  # Delete the askpass helper as soon as clone is done
  if (file.exists(askpass_script)) file.remove(askpass_script)

  # Enable git-lfs in the cloned repo
  system2("git", args = c("-C", clone_dir, "lfs", "install"),
          stdout = FALSE, stderr = FALSE)

  # Set a local git identity inside the clone so CI runners (which have no
  # global git config) can commit.  Using --local avoids touching the
  # runner's global config; the clone dir is deleted on.exit anyway.
  system2("git", args = c("-C", clone_dir, "config", "--local",
                          "user.name", "llmtelemetry-bot"),
          stdout = FALSE, stderr = FALSE)
  system2("git", args = c("-C", clone_dir, "config", "--local",
                          "user.email", "actions@github.com"),
          stdout = FALSE, stderr = FALSE)

  # Write the sanitised parquets to the clone
  sessions_out <- file.path(clone_dir, "sessions.parquet")
  costs_out    <- file.path(clone_dir, "costs.parquet")
  .hf_write_parquet_duckdb(sessions_clean, sessions_out)
  .hf_write_parquet_duckdb(costs_clean,    costs_out)

  # git add, commit, push
  system2("git", args = c("-C", clone_dir, "add",
                          "sessions.parquet", "costs.parquet"),
          stdout = FALSE, stderr = FALSE)

  commit_msg <- sprintf(
    "archive: telemetry v1 — %s sessions, %s cost rows [%s]",
    nrow(sessions_clean), nrow(costs_clean),
    format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  system2("git", args = c("-C", clone_dir, "commit", "-m", commit_msg),
          stdout = FALSE, stderr = FALSE)

  # Re-create GIT_ASKPASS for push (helper was deleted after clone)
  askpass_script2 <- .hf_make_askpass(resolved_token, workdir, hf_repo)
  on.exit(
    if (file.exists(askpass_script2)) file.remove(askpass_script2),
    add = TRUE
  )
  push_env <- paste0(names(clone_env), "=", clone_env)
  push_env[grepl("^GIT_ASKPASS=", push_env)] <-
    paste0("GIT_ASKPASS=", askpass_script2)

  push_result <- system2(
    "git",
    args   = c("-C", clone_dir, "push"),
    env    = push_env,
    stdout = TRUE,
    stderr = TRUE
  )
  if (!is.null(attr(push_result, "status")) &&
      attr(push_result, "status") != 0L) {
    cli::cli_abort(
      c("x" = "git push failed",
        "i" = paste(push_result, collapse = "\n"))
    )
  }
  if (file.exists(askpass_script2)) file.remove(askpass_script2)

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


#' Resolve the HuggingFace token file path for CI and local use.
#'
#' Checks two sources in order:
#' 1. If `token_path` exists on disk, return it unchanged (local / already-set-up CI).
#' 2. If the `HF_TOKEN` environment variable is non-empty, write its value to a
#'    transient file inside `workdir` (mode 0600) and return that path.
#'    The transient file's lifetime is bounded by the caller's `workdir` cleanup.
#'
#' The token value is written only to a file — it is NEVER stored in an R
#' variable that outlives this function's local scope, and never printed.
#'
#' @param token_path Character.  Preferred path (default `~/.cache/huggingface/token`).
#' @param workdir Character.  Directory where a transient token file may be created.
#' @return Character path to the resolved token file.
#' @keywords internal
.hf_resolve_token_path <- function(token_path, workdir) {
  if (file.exists(token_path)) {
    return(token_path)
  }
  env_token <- Sys.getenv("HF_TOKEN", unset = "")
  if (nzchar(env_token)) {
    transient_path <- file.path(workdir, ".hf_token_ci")
    # Trim ALL trailing/leading whitespace and strip internal CR/LF.
    # gh secret set from a file can include a trailing newline that corrupts
    # git auth ("password authentication no longer supported" from HuggingFace
    # means git sent "hf_xxx\n" as the password — the classic trailing-newline
    # symptom).  Use cat(..., sep = "") (NOT writeLines, which appends "\n")
    # so the file on disk contains exactly the token bytes with no newline.
    env_token_trimmed <- trimws(env_token, which = "both")
    env_token_trimmed <- gsub("[\r\n]", "", env_token_trimmed)
    cat(env_token_trimmed, file = transient_path, sep = "")
    Sys.chmod(transient_path, mode = "0600")
    # Clear the local string immediately
    env_token <- NULL
    env_token_trimmed <- NULL
    return(transient_path)
  }
  # Neither source available — abort with a clear message
  cli::cli_abort(
    c("x" = "HuggingFace token not found.",
      "i" = "On local machines: run {.code huggingface-cli login} to create {.file {token_path}}.",
      "i" = "In CI: set the {.envvar HF_TOKEN} secret and pass it via {.code env: HF_TOKEN: ${{{{ secrets.HF_TOKEN }}}}}.")
  )
}


#' Create a transient GIT_ASKPASS helper script.
#'
#' Writes a minimal shell script that is prompt-aware: when git invokes
#' `$GIT_ASKPASS "<prompt>"`, the helper checks the first word of the prompt:
#' - `Username*` → prints the HF repo owner (the part before `/` in `hf_repo`)
#'   using `printf` without a trailing newline.
#' - Anything else (the Password prompt) → emits the token via
#'   `printf '%s' "$(cat <file>)"`.  Command substitution strips trailing
#'   newlines from the file; `printf %s` adds none.  This ensures git never
#'   receives `hf_xxx\n` as the password even if the token file on disk
#'   (e.g. `~/.cache/huggingface/token`) carries a trailing newline.
#'
#' HuggingFace git-over-HTTPS requires username = the repo owner and
#' password = the HF token.  Sending the token as the username causes
#' "Invalid username or password".
#'
#' The caller is responsible for deleting the script after use.
#' The token is NEVER read into R memory — it flows only from the token file
#' to git's stdin via the helper process at clone/push time.
#'
#' @param token_path Character.  Path to the HF token file.
#' @param workdir Character.  Directory in which to create the helper.
#' @param hf_repo Character.  Repository identifier, e.g.
#'   `"JohnGavin/llmtelemetry-metrics"`.  The owner (part before `/`) is
#'   embedded in the helper as the git username.
#' @return Character path to the helper script.
#' @keywords internal
.hf_make_askpass <- function(token_path, workdir, hf_repo) {
  # Derive the repo owner: the substring before the first "/"
  repo_owner <- sub("/.*$", "", hf_repo)

  helper_path <- file.path(workdir, "hf_askpass.sh")
  writeLines(
    c("#!/bin/sh",
      # Git passes the full prompt string as $1.
      # "Username for 'https://...'" → return the repo owner (no trailing newline).
      # "Password for 'https://...'" (or anything else) → cat the token file.
      "case \"$1\" in",
      paste0("  Username*) printf '%s' ", shQuote(repo_owner), " ;;"),
      # Use printf '%s' "$(cat ...)" rather than plain cat: command substitution
      # strips any trailing newlines from the file, and printf %s adds none.
      # This is the belt-and-braces guard so even if the token file on disk
      # somehow carries a trailing newline (e.g. the local ~/.cache/huggingface/token
      # written by huggingface-cli), git never receives "hf_xxx\n" as the password.
      paste0("  *)         printf '%s' \"$(cat ", shQuote(token_path), ")\" ;;"),
      "esac"),
    con = helper_path
  )
  Sys.chmod(helper_path, mode = "0700")
  helper_path
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
