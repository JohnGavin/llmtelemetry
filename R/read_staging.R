#' Read all staged events from the llmtelemetry staging directory
#'
#' Reads all `.jsonl` files from the staging directory and returns a tibble
#' with one row per event envelope. Each envelope has a `ts` timestamp, `host`,
#' `pid`, and `payload` (a list-column containing the parsed JSON payload).
#'
#' This function is intentionally read-only and does not modify the staging
#' directory. Integration of staged events into the rollup pipeline (with
#' deduplication) is planned for Phase 1E.
#'
#' @param staging_dir Path to the staging directory. Defaults to
#'   `~/.claude/logs/llmtelemetry-staging`.
#' @return A [tibble::tibble()] with columns:
#'   - `ts` (POSIXct UTC): envelope timestamp
#'   - `host` (character): hostname of the emitting machine
#'   - `pid` (character): process ID at emission time
#'   - `payload` (list): parsed JSON payload (one element per row)
#' @export
read_staging <- function(
  staging_dir = file.path(Sys.getenv("HOME"), ".claude", "logs", "llmtelemetry-staging")
) {
  empty <- tibble::tibble(
    ts      = as.POSIXct(character(), tz = "UTC"),
    host    = character(),
    pid     = character(),
    payload = list()
  )

  if (!dir.exists(staging_dir)) return(empty)

  files <- list.files(staging_dir, pattern = "^events-.*\\.jsonl$",
                      full.names = TRUE)
  if (length(files) == 0L) return(empty)

  rows <- unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE)
  rows <- rows[nzchar(rows)]
  if (length(rows) == 0L) return(empty)

  parsed <- lapply(rows, function(line) {
    tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
  })

  # Drop NULL entries (lines that failed to parse)
  ok <- !vapply(parsed, is.null, logical(1L))
  parsed <- parsed[ok]

  if (length(parsed) == 0L) return(empty)

  tibble::tibble(
    ts      = as.POSIXct(
                vapply(parsed, function(x) x$ts %||% NA_character_, character(1L)),
                format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
              ),
    host    = vapply(parsed, function(x) x$host %||% NA_character_, character(1L)),
    pid     = vapply(parsed, function(x) x$pid  %||% NA_character_, character(1L)),
    payload = lapply(parsed, function(x) x$payload)
  )
}

#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a
