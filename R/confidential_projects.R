#' Canonical list of confidential project names
#'
#' Returns the complete set of project names (both full and short forms) that
#' must never appear in public outputs — parquet files, JSON exports, or the
#' deployed dashboard.  The list covers exact names AND their short-form aliases
#' (e.g. "solwatch" is the short form of "crypto_solwatch").
#'
#' Add new entries here; `drop_confidential_projects()` and the CI deploy gate
#' both consume this list so there is a single source of truth.
#'
#' @return A character vector of lower-cased confidential project names.
#' @keywords internal
confidential_project_names <- function() {
  c(
    "mycare",
    "crypto",
    "crypto_solwatch",
    "crypto_swarms",
    "solwatch",     # short-form alias of crypto_solwatch
    "swarms"        # short-form alias of crypto_swarms
  )
}


#' Drop rows that contain confidential project names
#'
#' Removes any row from `df` where a project-name column (default:
#' `project` and `canonical_project`) case-insensitively matches any entry in
#' [confidential_project_names()], OR whose value starts with `"crypto"` or
#' `"mycare"` (catches future sub-projects without requiring a list update).
#' Columns that are absent from `df` are silently skipped.
#'
#' The function is deliberately conservative: a row is dropped if ANY of the
#' specified columns matches.  This prevents short-name leaks where only one of
#' the two columns carries the confidential identifier.
#'
#' @param df A data frame to filter.
#' @param cols Character vector of column names to check.  Defaults to
#'   `c("project", "canonical_project")`.
#' @return The filtered data frame (same class as input).
#' @keywords internal
drop_confidential_projects <- function(
  df,
  cols = c("project", "canonical_project")
) {
  if (!is.data.frame(df) || nrow(df) == 0L) return(df)

  conf_lower <- tolower(confidential_project_names())

  # Build a logical "keep" mask: TRUE = safe to keep, FALSE = confidential.
  keep <- rep(TRUE, nrow(df))

  for (col in cols) {
    if (!col %in% names(df)) next
    val_lower <- tolower(as.character(df[[col]]))
    # Exact match OR starts with a known confidential prefix
    is_conf <- val_lower %in% conf_lower |
               startsWith(val_lower, "crypto") |
               startsWith(val_lower, "mycare")
    keep <- keep & !is_conf
  }

  df[keep, , drop = FALSE]
}
