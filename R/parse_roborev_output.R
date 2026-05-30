#' Parse Location: lines from roborev review output text
#'
#' Extracts `**Location**: <file>:<line>` markup entries from a roborev review
#' output blob and returns them as a structured tibble. Each review may contain
#' zero or more Location lines. Line numbers are optional.
#'
#' @param output_text character(1). The raw output text of a single roborev
#'   review (the `output` column in the reviews table).
#' @param review_id integer(1). The review ID to attach to every extracted row.
#' @param severity character(1). The severity label for the review
#'   (e.g. `"High"`, `"Medium"`, `"Low"`). Attached to every row.
#' @param repo character(1). The repository name for the review (denormalised).
#'   Attached to every row.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{review_id}{integer — FK to the source review}
#'     \item{file_path}{character — repo-relative or absolute file path from the Location: line}
#'     \item{line}{integer — line number, or `NA_integer_` when not specified}
#'     \item{severity}{character — severity label from the review}
#'     \item{repo}{character — repository name}
#'   }
#'   Returns a zero-row tibble with the correct columns when `output_text` is
#'   `NA`, empty, or contains no Location lines.
#'
#' @examples
#' txt <- paste0(
#'   "- **Severity**: High\n",
#'   "- **Location**: R/foo.R:42\n",
#'   "- **Problem**: Example problem\n"
#' )
#' parse_review_locations(txt, review_id = 1L, severity = "High", repo = "llm")
#'
#' @export
parse_review_locations <- function(output_text,
                                   review_id = NA_integer_,
                                   severity  = NA_character_,
                                   repo      = NA_character_) {
  # Typed empty result for all early-exit paths.
  empty_result <- tibble::tibble(
    review_id = integer(0L),
    file_path = character(0L),
    line      = integer(0L),
    severity  = character(0L),
    repo      = character(0L)
  )

  if (is.null(output_text) || length(output_text) == 0L ||
        is.na(output_text) || !nzchar(trimws(output_text))) {
    return(empty_result)
  }

  # Split on newlines to allow per-line matching.
  lines_vec <- strsplit(output_text, "\n", fixed = TRUE)[[1L]]

  # Pattern: optional leading whitespace and bullet dash, then **Location**: <body>
  # Group 1: the entire location body (path + optional :line or :range)
  # We extract the body then parse path and line number in a second pass.
  loc_re <- paste0(
    "^\\s*-?\\s*",               # optional leading whitespace + bullet dash
    "\\*{1,2}Location\\*{1,2}",  # **Location** or *Location*
    ":\\s*",                     # colon + optional space
    "(.+?)\\s*$"                 # group 1: entire body (trimmed)
  )

  m <- regexec(loc_re, lines_vec, perl = TRUE, ignore.case = FALSE)
  matched_rows <- which(vapply(m, function(x) x[1L] > 0L, logical(1L)))

  if (length(matched_rows) == 0L) {
    return(empty_result)
  }

  # Helper: parse one Location body string into (file_path, line).
  # The body may be: `path:line`, `path:line-range`, `path`, or plain path.
  # Only a SINGLE trailing :digits (no dash) is extracted as the line number.
  parse_body <- function(body) {
    # Strip backticks from the body first (may wrap entire expression).
    body <- gsub("`", "", body, fixed = TRUE)

    # Take only the first semicolon-segment (ignore secondary locations).
    body <- trimws(strsplit(body, ";", fixed = TRUE)[[1L]][1L])
    body <- trimws(body)

    # Extract trailing single-integer line: ":42" but not ":42-67".
    ln <- NA_integer_
    m_ln <- regexpr(":(\\d+)\\s*$", body, perl = TRUE)
    if (m_ln > 0L) {
      # Confirm it is NOT a range (no dash immediately before end)
      matched_str <- regmatches(body, m_ln)
      digit_part  <- sub("^:", "", matched_str, fixed = FALSE)
      # Check there's no range marker anywhere before the last colon that
      # belongs to a different segment — simple check: the digit captured
      # must not be immediately preceded by a dash.
      char_before <- if (m_ln > 1L) substr(body, m_ln - 1L, m_ln - 1L) else ""
      if (char_before != "-") {
        ln   <- as.integer(digit_part)
        body <- substr(body, 1L, m_ln - 1L)
      }
    }

    # Strip any remaining trailing line-range ":287-307".
    body <- sub(":\\d+-\\d+\\s*$", "", body, perl = TRUE)

    list(file_path = trimws(body), line = ln)
  }

  results <- lapply(matched_rows, function(i) {
    # regmatches() requires x and m to have the same length.
    caps <- regmatches(lines_vec[i], m[i])
    # caps is a length-1 list; caps[[1]]: c(full_match, group1)
    body <- caps[[1L]][2L]

    parsed <- parse_body(body)
    fp     <- parsed$file_path
    ln     <- parsed$line

    tibble::tibble(
      review_id = as.integer(review_id),
      file_path = fp,
      line      = ln,
      severity  = as.character(severity),
      repo      = as.character(repo)
    )
  })

  do.call(rbind, results)
}
