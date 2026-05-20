# Internal helper for sanitizing private filesystem paths in session_id columns.
#
# Both rollup_sessions.R and inst/scripts/export_dashboard_data.R previously
# used INCOMPATIBLE hash algorithms (polynomial-%%-1e6 vs digest-md5-12hex).
# This single source-of-truth helper is now called from both code paths so
# the same (path, project, started_at) triple always produces the same id.
#
# Format: "sanitized@{canonical_project}@{iso8601_utc}@h{hash12}"
#
# References: Issue #9 (12-hex hash space), Issue #8 (ISO 8601 UTC timestamp).

#' Compute a stable 12-hex hash for a raw path string.
#'
#' Uses `digest::digest()` with MD5 when available; falls back to a weighted
#' integer sum approach so the function works without the `digest` package.
#'
#' @param p A single character string (the raw path).
#' @return A 12-character lowercase hex string.
#' @keywords internal
.path_hash12 <- function(p) {
  if (requireNamespace("digest", quietly = TRUE)) {
    substr(digest::digest(p, algo = "md5", serialize = FALSE), 1L, 12L)
  } else {
    bytes   <- utf8ToInt(p)
    raw_val <- sum(as.numeric(bytes) * seq_along(bytes))
    sprintf("%012x", bitwAnd(abs(as.integer(raw_val %% .Machine$integer.max)),
                             as.integer(.Machine$integer.max)))
  }
}

#' Sanitize path-style session_id values.
#'
#' Replaces raw filesystem path session IDs (those starting with `"-"` or
#' containing `"/"`) with a deterministic stable identifier that does not
#' expose private paths.  Non-path IDs are returned unchanged.
#'
#' @param session_ids   Character vector of session IDs (may be mixed path /
#'   non-path).
#' @param canonical_projects  Character vector (same length) of canonical
#'   project names; use `NA_character_` when unknown.
#' @param started_at    POSIXct vector (same length) of session start times
#'   used as the timestamp component.  When `NA`, falls back to the positional
#'   index cast to character.
#'
#' @return A character vector of the same length.  Path-style IDs are replaced
#'   with `"sanitized@{project}@{iso8601_utc}@h{hash12}"`.  Non-path IDs pass
#'   through unchanged.
#'
#' @keywords internal
.sanitize_session_id_local <- function(session_ids, canonical_projects, started_at) {
  stopifnot(
    length(session_ids) == length(canonical_projects),
    length(session_ids) == length(started_at)
  )

  is_path <- grepl("^-|[/\\\\]", session_ids)
  if (!any(is_path)) return(session_ids)

  path_indices <- which(is_path)
  raw_paths    <- session_ids[is_path]

  # Stable ISO 8601 UTC timestamps; never locale-dependent as.character().
  started_at_iso <- vapply(
    seq_along(path_indices),
    function(k) {
      sa <- started_at[path_indices[k]]
      if (inherits(sa, "POSIXct") && !is.na(sa)) {
        format(sa, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      } else {
        as.character(path_indices[k])
      }
    },
    character(1L)
  )

  cp_values <- ifelse(
    is.na(canonical_projects[is_path]), "unknown",
    canonical_projects[is_path]
  )

  result <- session_ids
  result[is_path] <- paste0(
    "sanitized@",
    cp_values,
    "@",
    started_at_iso,
    "@h", vapply(raw_paths, .path_hash12, character(1L))
  )
  result
}
