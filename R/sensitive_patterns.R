#' Sensitive pattern constants for privacy sanitization
#'
#' @description
#' Single source of truth for all private-filesystem-path and username patterns.
#' Used by both the sanitization script (`inst/scripts/sanitize_ccusage_all.R`)
#' and the regression gate (`tests/testthat/test-no-path-leak.R`).
#'
#' Pattern classes (must stay in sync between `sensitive_id_pattern()` and
#' `sensitive_verify_patterns()`):
#'
#' | Class            | Sanitizer regex   | Verify substrings         |
#' |------------------|-------------------|---------------------------|
#' | home-dir prefix  | `^-Users-`        | `Users-johngavin`         |
#' | absolute path    | `^/`              | `/Users/`, `/private/`, `/tmp/`, `/var/` |
#' | macOS tmp        | `^-private-tmp-`  | `-private-tmp-`           |
#' | generic tmp      | `^-tmp-`          | `-tmp-`                   |
#' | any worktree     | `-worktree-`      | `-worktree-`              |
#' | named agent wt   | `worktree-agent-` | `worktree-agent-`         |
#' | username         | `johngavin`       | `johngavin`               |
#'
#' INVARIANT: every class in `sensitive_id_pattern()` has at least one
#' corresponding entry in `sensitive_verify_patterns()`.  If you add a class
#' to the sanitizer, add the matching verify substring(s) here.
#'
#' @name sensitive_patterns
NULL

#' Regex for detecting sensitive filesystem-path identifiers
#'
#' Returns a single OR-joined regex suitable for [grepl()] with `perl = TRUE`.
#' Matches any string value that could reveal private filesystem structure:
#' raw home-dir prefixes, Unix absolute paths, macOS/generic tmp worktree IDs,
#' agent worktree references, or the bare username.
#'
#' Used by the sanitizer to decide which IDs need canonicalization.
#'
#' @return character(1) — an OR-joined regex
#' @export
#' @examples
#' grepl(sensitive_id_pattern(), "-Users-johngavin-docs-gh-llm", perl = TRUE)
#' grepl(sensitive_id_pattern(), "/private/tmp/roborev-worktree-1234", perl = TRUE)
#' grepl(sensitive_id_pattern(), "llmtelemetry", perl = TRUE) # FALSE
sensitive_id_pattern <- function() {
  paste0(
    "^-Users-",           # raw home-dir prefix (dashed form)
    "|^/",                # Unix absolute path: /Users/..., /private/tmp/..., /var/..., /tmp/...
    "|^-private-tmp-",    # macOS /private/tmp/ in dashed form
    "|^-tmp-",            # generic /tmp/ in dashed form
    "|-worktree-",        # any *-worktree-* substring (numeric, named, generic)
    "|worktree-agent-",   # .claude/worktrees/agent-... (belt-and-suspenders)
    "|johngavin"          # username anywhere
  )
}

#' Grep-able substrings for post-sanitization verification
#'
#' Returns a character vector of anchor-free substrings used to scan file
#' content (not just string starts) after sanitization.  Every class in
#' [sensitive_id_pattern()] has at least one corresponding entry here.
#'
#' The verify set is a TRUE SUPERSET of the sanitizer: it catches not only the
#' forms the sanitizer rewrites but also any form that might have leaked through
#' a different code path (e.g. a raw `/private/tmp/plain-id` written directly
#' to a field without going through `derive_canonical_id()`).
#'
#' **Coverage of the `^/` (absolute-path) class:**
#' A bare `"/"` would match URLs and JSON separators.  Instead we enumerate the
#' known dangerous absolute-path prefixes: `/Users/`, `/private/`, `/tmp/`,
#' `/var/`.  This catches home-dirs, macOS tmp mounts, and Linux tmp/var
#' paths without generating noise from HTTPS URLs or JSON structure.
#'
#' Used by:
#' - The script's final verification gate (whole-file content scan)
#' - `tests/testthat/test-no-path-leak.R` as `forbidden_patterns`
#'
#' @return character vector of substrings (grep-style, no anchors)
#' @export
#' @examples
#' patterns <- sensitive_verify_patterns()
#' any(vapply(patterns, function(p) grepl(p, "/Users/foo", perl = TRUE), logical(1)))
#' any(vapply(patterns, function(p) grepl(p, "/private/tmp/foo", perl = TRUE), logical(1)))
sensitive_verify_patterns <- function() {
  c(
    # home-dir prefix class (^-Users-)
    "Users-johngavin",
    # absolute-path class (^/) — specific dangerous prefixes, avoids URL/JSON noise
    "/Users/",
    "/private/",
    "/tmp/",
    "/var/",
    # macOS tmp class (^-private-tmp-)
    "-private-tmp-",
    # generic tmp class (^-tmp-)
    "-tmp-",
    # any worktree class (-worktree-)
    "-worktree-",
    # named agent worktree (worktree-agent-)
    "worktree-agent-",
    # username class
    "johngavin"
  )
}

#' Patterns that identify genuine filesystem path leaks in any field
#'
#' These patterns are specific enough to distinguish an actual filesystem path
#' from a commit message describing a concept.  They are applied to every
#' field, including commit `message` fields that have a bare-token allowlist.
#'
#' Examples of what each catches:
#' - `/Users/johngavin/docs_gh/...`  — actual home-dir path
#' - `/private/tmp/roborev-worktree-...` — actual macOS tmp path
#' - `/tmp/fixer-worktree-...` — actual Linux tmp path
#' - `-private-tmp-roborev-worktree-...` — dashed form of macOS tmp
#' - `-tmp-fixer-worktree-...` — dashed form of Linux tmp
#' - `Users-johngavin` — dashed form of home-dir prefix in ID fields
#'
#' What these do NOT catch (intentional):
#' - `"feat: add hardcoded /Users/ path check"` — describes the concept, not
#'   a real path (no username following /Users/)
#'
#' @return character vector of path-shape substrings (no anchors)
#' @export
path_shape_patterns <- function() {
  c(
    "/Users/johngavin",   # actual home-dir path (username present — not just concept)
    "/private/tmp/",      # actual macOS tmp absolute path
    "/tmp/",              # actual Linux tmp absolute path
    "-private-tmp-",      # dashed form of macOS tmp (in session ID fields)
    "-tmp-",              # dashed form of Linux tmp (in session ID fields)
    "Users-johngavin"     # dashed form of home-dir prefix in session IDs
  )
}
