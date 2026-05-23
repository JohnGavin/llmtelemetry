# email_helpers.R
# Shared formatting helpers for daily email report.
# Sourced by inst/scripts/send_daily_email.R and directly exercised by tests.
#
# All helpers treat NaN / NA / Inf / NULL as missing and return a safe fallback
# string, preventing those tokens from ever reaching sprintf() and appearing as
# literal "NaN" / "NA" / "Inf" in the emailed HTML.

#' Format a cost value as a USD string.
#'
#' @param x Numeric scalar.
#' @return Character string: "$N.NN" or "-" for missing/non-finite inputs.
dollar <- function(x) {
  if (is.null(x) || length(x) == 0) return("-")
  if (is.na(x) || is.nan(x) || !is.finite(x)) return("-")
  sprintf("$%.2f", x)
}

#' Format a number with thousands separator.
#'
#' @param x Numeric scalar.
#' @return Character string or "-" for missing/non-finite inputs.
comma <- function(x) {
  if (is.null(x) || length(x) == 0) return("-")
  if (is.na(x) || is.nan(x) || !is.finite(x)) return("-")
  format(x, big.mark = ",", scientific = FALSE)
}

#' Format a token count in millions (e.g. "1.2M").
#'
#' @param x Numeric scalar (raw token count).
#' @return Character string or "-" for missing/non-finite inputs.
millions <- function(x) {
  if (is.null(x) || length(x) == 0) return("-")
  if (is.na(x) || is.nan(x) || !is.finite(x)) return("-")
  sprintf("%.1fM", x / 1e6)
}

#' Safe integer formatter for raw count fields (%s slots in sprintf).
#'
#' Used for fields like cc_days, gm_days, cm_days, gm_sessions_count,
#' n_sessions, cx_days, cx_sessions that bypass the dollar/comma/millions
#' wrappers and are inserted directly via %s.  Without this guard, NaN or NA
#' would render as the literal string "NaN" or "NA" in the HTML.
#'
#' @param x Numeric scalar.
#' @param fallback Character string returned when x is missing or non-finite.
#' @return Character string or fallback.
email_safe_num <- function(x, fallback = "-") {
  if (is.null(x) || length(x) == 0) return(fallback)
  if (is.na(x) || is.nan(x) || !is.finite(x)) return(fallback)
  as.character(as.integer(x))
}
