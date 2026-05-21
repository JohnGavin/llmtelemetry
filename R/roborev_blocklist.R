# roborev blocklist — read/write/check/ack helpers (Phase 2 of #144).
#
# The blocklist is a JSON array stored at
# ~/.claude/state/roborev_blocklist.json (default).  Each element corresponds
# to a loop entry with tier "block" or "escalate" that has not been
# acknowledged (or whose acknowledgement has expired).
#
# External override: set ROBOREV_BLOCKLIST_PATH env var to point at a
# different file (useful in tests).

# ---- internal ---------------------------------------------------------------

#' Resolve blocklist path
#'
#' Checks `ROBOREV_BLOCKLIST_PATH` env var first; falls back to `path`.
#' @param path Default path.
#' @return Resolved path as character.
#' @keywords internal
.blocklist_path <- function(path) {
  env_override <- Sys.getenv("ROBOREV_BLOCKLIST_PATH", unset = "")
  if (nzchar(env_override)) env_override else path
}

#' Empty blocklist tibble (correct schema)
#' @keywords internal
.empty_blocklist <- function() {
  tibble::tibble(
    content_hash    = character(),
    severity        = character(),
    primary_file    = character(),
    summary         = character(),
    cycles          = integer(),
    tier            = character(),
    ack_by          = character(),
    ack_at          = as.POSIXct(character()),
    ack_reason      = character(),
    ack_until       = as.POSIXct(character()),
    updated_at      = as.POSIXct(character())
  )
}

# ---- exported ---------------------------------------------------------------

#' Read the roborev loop blocklist
#'
#' Parses the JSON blocklist file and returns a tibble.  Returns an empty
#' tibble (with correct columns) if the file does not exist.
#'
#' @param path Path to blocklist JSON.  Overridden by `ROBOREV_BLOCKLIST_PATH`
#'   env var.
#' @return A tibble.
#' @export
read_blocklist <- function(
  path = path.expand("~/.claude/state/roborev_blocklist.json")
) {
  path <- .blocklist_path(path)
  if (!file.exists(path)) return(.empty_blocklist())

  raw <- tryCatch(
    jsonlite::fromJSON(path, simplifyDataFrame = TRUE),
    error = function(e) {
      cli::cli_warn("roborev_blocklist: failed to parse {.path {path}}: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0L) {
    return(.empty_blocklist())
  }

  # Coerce types
  df <- tibble::as_tibble(raw)
  for (col in c("content_hash", "severity", "primary_file", "summary",
                "tier", "ack_by", "ack_reason")) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
    } else {
      df[[col]] <- NA_character_
    }
  }
  if ("cycles" %in% names(df)) {
    df$cycles <- as.integer(df$cycles)
  } else {
    df$cycles <- NA_integer_
  }
  for (col in c("ack_at", "ack_until", "updated_at")) {
    if (col %in% names(df)) {
      df[[col]] <- as.POSIXct(df[[col]], tz = "UTC")
    } else {
      df[[col]] <- as.POSIXct(NA)
    }
  }
  df
}

#' Write the roborev loop blocklist
#'
#' Filters `loops_df` to rows with `tier %in% c("block", "escalate")` that
#' are not acknowledged (or whose acknowledgement has expired), then atomically
#' writes the result as a JSON array.  Atomic write: output first goes to a
#' PID-tagged temp file, then renamed.  Cleans up temp file on failure.
#'
#' @param loops_df Tibble from [detect_loops()] or [read_blocklist()].
#' @param path Destination path.  Overridden by `ROBOREV_BLOCKLIST_PATH`.
#' @return Invisibly returns the number of rows written.
#' @export
write_blocklist <- function(
  loops_df,
  path = path.expand("~/.claude/state/roborev_blocklist.json")
) {
  path <- .blocklist_path(path)

  now_utc <- as.POSIXct(Sys.time(), tz = "UTC")

  # Filter to actionable rows
  to_write <- loops_df |>
    dplyr::filter(.data$tier %in% c("block", "escalate")) |>
    dplyr::filter(
      is.na(.data$ack_by) |
      (!is.na(.data$ack_until) & .data$ack_until < now_utc)
    ) |>
    dplyr::mutate(updated_at = now_utc)

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  tmp_path <- paste0(path, ".", Sys.getpid(), ".tmp")

  # Ensure tmp is cleaned up if write fails
  on.exit({
    if (file.exists(tmp_path)) unlink(tmp_path)
  }, add = TRUE)

  tryCatch(
    jsonlite::write_json(to_write, tmp_path, auto_unbox = TRUE, pretty = TRUE),
    error = function(e) {
      cli::cli_abort(c(
        "x" = "write_blocklist: failed to write temp file {.path {tmp_path}}",
        "i" = conditionMessage(e)
      ))
    }
  )

  ok <- file.rename(tmp_path, path)
  if (!isTRUE(ok)) {
    cli::cli_abort(c(
      "x" = "write_blocklist: file.rename() failed",
      "i" = "Could not rename {.path {tmp_path}} to {.path {path}}"
    ))
  }

  # Successful rename: cancel the on.exit cleanup (tmp is gone)
  # (R evaluates the most-recently registered on.exit first; we add = TRUE above
  #  so we can't easily cancel.  We simply touch tmp_path status: it no longer
  #  exists after rename, so unlink() in on.exit is a no-op.)

  invisible(nrow(to_write))
}

#' Check whether an agent prompt or file list is blocked
#'
#' Matches the blocklist against `files_mentioned` (exact `primary_file`
#' match) and against `agent_prompt_text` (Jaccard similarity ≥ 0.5 on token
#' sets).
#'
#' @param agent_prompt_text Character scalar — the agent's prompt.
#' @param files_mentioned Character vector of file paths mentioned.
#' @param path Blocklist path.  Overridden by `ROBOREV_BLOCKLIST_PATH`.
#' @return A named list: `blocked` (logical), `matched_entries` (tibble).
#' @export
check_blocklist <- function(
  agent_prompt_text,
  files_mentioned = NULL,
  path = path.expand("~/.claude/state/roborev_blocklist.json")
) {
  bl <- read_blocklist(path)
  if (nrow(bl) == 0L) {
    return(list(blocked = FALSE, matched_entries = .empty_blocklist()))
  }

  # Match 1: file path intersection
  file_matches <- if (!is.null(files_mentioned) && length(files_mentioned) > 0L) {
    bl[bl$primary_file %in% files_mentioned, , drop = FALSE]
  } else {
    bl[integer(0L), , drop = FALSE]
  }

  # Match 2: Jaccard similarity on prompt vs summary
  jaccard_matches <- purrr::map_lgl(
    bl$summary,
    function(s) {
      .token_jaccard(agent_prompt_text, s) >= 0.5
    }
  )
  prompt_matches <- bl[jaccard_matches, , drop = FALSE]

  matched <- dplyr::distinct(rbind(file_matches, prompt_matches))

  list(
    blocked         = nrow(matched) > 0L,
    matched_entries = matched
  )
}

#' Acknowledge a loop entry to suppress blocking temporarily
#'
#' Sets `ack_by`, `ack_at`, `ack_reason`, and optionally `ack_until` for the
#' entry with the given `content_hash`.  If `until` is `NULL`, the entry is
#' acknowledged indefinitely (the loop is considered resolved).
#'
#' Also updates the same fields in `roborev_loops` in DuckDB if the database
#' is available.
#'
#' @param content_hash Character scalar — hash to acknowledge.
#' @param reason Character scalar — human-readable reason.
#' @param until POSIXct or NULL.  If a timestamp, the block is suppressed only
#'   until that time.
#' @param by Character scalar — who is acknowledging (default: current user).
#' @param path Blocklist JSON path.  Overridden by `ROBOREV_BLOCKLIST_PATH`.
#' @param db_path Optional path to `unified.duckdb` for audit sync.
#' @return Invisibly returns the updated blocklist tibble.
#' @export
ack_loop <- function(
  content_hash,
  reason,
  until    = NULL,
  by       = Sys.info()[["user"]],
  path     = path.expand("~/.claude/state/roborev_blocklist.json"),
  db_path  = NULL
) {
  checkmate::assert_string(content_hash)
  checkmate::assert_string(reason)
  checkmate::assert_string(by, na.ok = FALSE)

  path <- .blocklist_path(path)

  # Read the full loops tibble (we need to re-write all rows)
  bl <- read_blocklist(path)

  if (!content_hash %in% bl$content_hash) {
    cli::cli_abort(c(
      "x" = "ack_loop: content_hash {.val {content_hash}} not found in blocklist",
      "i" = "Run `read_blocklist()` to see available hashes."
    ))
  }

  now_utc  <- as.POSIXct(Sys.time(), tz = "UTC")
  ack_until_val <- if (!is.null(until)) as.POSIXct(until, tz = "UTC") else as.POSIXct(NA)

  idx <- bl$content_hash == content_hash
  bl$ack_by[idx]     <- by
  bl$ack_at[idx]     <- now_utc
  bl$ack_reason[idx] <- reason
  bl$ack_until[idx]  <- ack_until_val

  # Write all rows back (write_blocklist will re-filter to active block/escalate)
  # but we want to preserve the ack in the full file — write raw
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp_path <- paste0(path, ".", Sys.getpid(), ".tmp")
  on.exit({
    if (file.exists(tmp_path)) unlink(tmp_path)
  }, add = TRUE)

  jsonlite::write_json(bl, tmp_path, auto_unbox = TRUE, pretty = TRUE)
  ok <- file.rename(tmp_path, path)
  if (!isTRUE(ok)) {
    cli::cli_abort(c(
      "x" = "ack_loop: file.rename() failed",
      "i" = "Could not rename {.path {tmp_path}} to {.path {path}}"
    ))
  }

  # Optional DuckDB audit sync
  if (!is.null(db_path) && file.exists(db_path)) {
    tryCatch({
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
      if ("roborev_loops" %in% DBI::dbListTables(con)) {
        DBI::dbExecute(con,
          sprintf(
            "UPDATE roborev_loops
             SET ack_by = %s, ack_at = '%s', ack_reason = %s, ack_until = %s
             WHERE content_hash = '%s'",
            ifelse(is.na(by),    "NULL", paste0("'", by, "'")),
            format(now_utc, "%Y-%m-%d %H:%M:%S"),
            ifelse(is.na(reason), "NULL", paste0("'", gsub("'", "''", reason), "'")),
            ifelse(is.na(ack_until_val), "NULL",
                   paste0("'", format(ack_until_val, "%Y-%m-%d %H:%M:%S"), "'")),
            content_hash
          )
        )
      }
    }, error = function(e) {
      cli::cli_warn("ack_loop: DuckDB sync failed (non-fatal): {conditionMessage(e)}")
    })
  }

  cli::cli_inform(c(
    "v" = "Acknowledged loop {.val {content_hash}}",
    "i" = "Reason: {reason}",
    "i" = if (!is.null(until)) "Until: {format(ack_until_val)}" else "Indefinitely"
  ))

  invisible(bl)
}

# Re-export .token_jaccard so it's visible to check_blocklist above.
# (It is defined in detect_loops.R which loads in the same namespace.)
