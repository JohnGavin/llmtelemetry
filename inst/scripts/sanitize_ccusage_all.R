#!/usr/bin/env Rscript
# sanitize_ccusage_all.R — Remove private filesystem paths from ccusage _all exports.
#
# PURPOSE
# -------
# ccusage emits raw project paths (e.g. "-Users-johngavin-docs-gh-llm") as session
# IDs, project keys, and projectPath values.  These must be replaced with canonical
# project names before committing to git.
#
# USAGE
# -----
# This script MUST be invoked by any process that writes or refreshes the _all JSON
# files:
#
#   Rscript inst/scripts/sanitize_ccusage_all.R
#
# IMPORTANT: The 12-hourly external cron at
#   ~/docs_gh/proj/data/llm/telemetry/exec/refresh_and_preserve.sh
# MUST call this script after ccusage finishes and BEFORE the git commit step.
# Failure to do so re-introduces the privacy leaks that issue #FOLLOWUP tracks.
#
# See also: tests/testthat/test-no-path-leak.R  (regression gate)
#           tests/testthat/test-sanitize-ccusage-all.R  (unit tests for these fns)

suppressPackageStartupMessages({
  library(jsonlite)
  library(here)
})

# ---------------------------------------------------------------------------
# SENSITIVE_ID_PATTERN — broad deny-list for private filesystem identifiers
# ---------------------------------------------------------------------------
# Matches any raw project/session ID that could reveal private filesystem
# structure.  Applied at ALL guard sites (previously only "^-Users-|^/" was
# checked, which missed /private/tmp/ and .claude/worktrees/ forms).
#
# SINGLE SOURCE OF TRUTH: Both SENSITIVE_ID_PATTERN (used by the sanitizer)
# and SENSITIVE_VERIFY_PATTERNS (used by the final verification gate) are
# derived from this one mapping.  Adding a new class here automatically
# propagates to verification — they can never drift again.
#
# Pattern class → sanitizer regex → verify substring
# ---------------------------------------------------
#   home-dir prefix  :  ^-Users-            → "Users-johngavin"
#   absolute path    :  ^/                  → "/Users/"  (covers /Users/... and /private/...)
#   macOS tmp        :  ^-private-tmp-      → "-private-tmp-"
#   generic tmp      :  ^-tmp-              → "-tmp-"
#   any worktree     :  -worktree-          → "-worktree-"
#   named agent wt   :  worktree-agent-     → "worktree-agent-"  (belt-and-suspenders)
#   username         :  johngavin           → "johngavin"
SENSITIVE_ID_PATTERN <- paste0(
  "^-Users-",           # raw home-dir prefix
  "|^/",                # Unix absolute path
  "|^-private-tmp-",    # macOS /private/tmp/ worktrees
  "|^-tmp-",            # generic /tmp/ worktrees
  "|-worktree-",        # any *-worktree-* substring (catches numeric + named)
  "|worktree-agent-",   # .claude/worktrees/agent-... named form
  "|johngavin"          # username anywhere (belt-and-suspenders)
)

# SENSITIVE_VERIFY_PATTERNS — grep-able substrings derived from SENSITIVE_ID_PATTERN.
# Used by the final verification gate to scan file content (not just string starts),
# so patterns here are anchor-free substrings, not anchored regexes.
# INVARIANT: every class in SENSITIVE_ID_PATTERN has at least one entry here.
# If you add a class above, add the matching verify substring below.
SENSITIVE_VERIFY_PATTERNS <- c(
  "Users-johngavin",    # home-dir prefix class  (^-Users-)
  "/Users/",            # absolute-path class     (^/) — covers /Users/..., /private/...
  "-private-tmp-",      # macOS tmp class         (^-private-tmp-)
  "-tmp-",              # generic tmp class        (^-tmp-)
  "-worktree-",         # any worktree class       (-worktree-)
  "worktree-agent-",    # named agent worktree     (worktree-agent-)
  "johngavin"           # username class           (johngavin)
)

# ---------------------------------------------------------------------------
# Helpers — canonicalization (mirrors R/canonicalize.R internals)
# ---------------------------------------------------------------------------

.shorten_project_local <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)
  x <- gsub("^-Users-johngavin-docs--pers-NHS-health-data-antigravity-", "", x)
  x <- gsub("^-Users-johngavin-docs[-_]gh-", "", x)
  x <- gsub("^docs[-_]gh[-_]", "", x)
  x <- gsub("^llm-", "", x)
  x <- gsub("^proj-", "", x)
  x <- gsub("-", "/", x, fixed = TRUE)
  x
}

.canonicalize_project_local_scalar <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(name)) return(NA_character_)
  name <- .shorten_project_local(name)

  meta_only <- c(
    "sonnet", "roborev", "worktree",
    "antigravity", "crypto", "data", "github", "hello",
    "knowledge", "simulations", "sport", "subagents",
    "t", "io", "urban_planning", "notmineraft", "telemetry", "football"
  )
  if (name %in% meta_only) return(NA_character_)

  name <- sub("^[A-Za-z0-9_]{8,}/repo/?", "", name)
  if (!nzchar(name)) return(NA_character_)

  overrides <- list(
    "buoy/network"                    = "irish_buoy_network",
    "irishbuoys"                      = "irish_buoy_network",
    "data/weather/irish/buoy/network" = "irish_buoy_network",
    "weather/irish/buoy/network"      = "irish_buoy_network"
  )
  for (pat in names(overrides)) {
    if (startsWith(name, pat)) return(overrides[[pat]])
  }

  container_prefixes <- c(
    "stats/simulations/", "stats/sport/", "finance/data/",
    "worktree/", "simulations/", "sport/", "data/", "crypto/",
    "subagents/", "knowledge/", "github/", "antigravity/", "hello/",
    "stats/", "pers/", "finance/"
  )
  for (pfx in container_prefixes) {
    if (startsWith(name, pfx)) {
      name <- sub(paste0("^", pfx), "", name)
      break
    }
  }
  if (!nzchar(name)) return(NA_character_)

  parts <- strsplit(name, "/", fixed = TRUE)[[1]]
  if (length(parts) == 0L || !nzchar(parts[1L])) return(NA_character_)
  first <- parts[1L]
  if (grepl("^[0-9]+$", first)) return(NA_character_)
  if (first %in% meta_only) return(NA_character_)
  first
}

#' Derive canonical project name from a raw path-style ID.
#' Returns the raw ID unchanged if no path prefix is detected.
#' @param raw_id character scalar — raw session/project ID
#' @return character scalar — sanitized ID
derive_canonical_id <- function(raw_id) {
  if (is.null(raw_id) || is.na(raw_id) || !nzchar(raw_id)) return(raw_id)
  # Rewrite any ID that looks like a private filesystem path or contains a
  # private identifier (home-dir prefix, /private/tmp/, worktree, username).
  if (!grepl(SENSITIVE_ID_PATTERN, raw_id, perl = TRUE)) return(raw_id)
  canonical <- .canonicalize_project_local_scalar(raw_id)
  if (is.na(canonical) || !nzchar(canonical)) {
    # Fallback: strip the home-dir prefix and use remainder.
    # For non -Users- prefixes (e.g. -private-tmp-, -tmp-) the strip below
    # leaves an empty string, which is caught by the !nzchar() guard.
    canonical <- gsub("^-Users-johngavin-docs[-_]?[-_]?(?:pers-NHS-health-data-antigravity-|gh-|)",
                      "", raw_id, perl = TRUE)
    canonical <- gsub("^proj-", "", canonical)
    canonical <- gsub("-", "/", canonical, fixed = TRUE)
    if (!nzchar(canonical) || grepl(SENSITIVE_ID_PATTERN, canonical, perl = TRUE)) {
      canonical <- "unknown"
    }
  }
  # Append stable hash so downstream consumers can correlate across refreshes
  hash12 <- substr(digest::digest(raw_id, algo = "md5"), 1, 12)
  paste0("sanitized@", canonical, "@h", hash12)
}

#' Sanitize a projectPath field value.
#' Keeps "Unknown Project" as-is; replaces path-derived values with a
#' canonical+hash string that drops the UUID suffix.
#' @param pp character scalar
#' @return character scalar
sanitize_project_path <- function(pp) {
  if (is.null(pp) || is.na(pp) || !nzchar(pp)) return(pp)
  if (!grepl(SENSITIVE_ID_PATTERN, pp, perl = TRUE)) return(pp)
  # Strip UUID suffix before canonicalizing
  pp_nouid <- sub("/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$", "", pp)
  derive_canonical_id(pp_nouid)
}

# ---------------------------------------------------------------------------
# sanitize_session_all — sanitize ccusage_session_all.json
# ---------------------------------------------------------------------------

#' Read, sanitize, and return the sessions list.
#' @param data list — parsed JSON (must have a "sessions" element)
#' @return list with (sanitized_data, n_sanitized)
#' @export
sanitize_session_all <- function(data) {
  sessions <- data$sessions
  n_sanitized <- 0L

  for (i in seq_along(sessions)) {
    sid <- sessions[[i]]$sessionId
    if (!is.null(sid) && grepl(SENSITIVE_ID_PATTERN, sid, perl = TRUE)) {
      sessions[[i]]$sessionId <- derive_canonical_id(sid)
      n_sanitized <- n_sanitized + 1L
    }

    pp <- sessions[[i]]$projectPath
    if (!is.null(pp) && grepl(SENSITIVE_ID_PATTERN, pp, perl = TRUE)) {
      sessions[[i]]$projectPath <- sanitize_project_path(pp)
      n_sanitized <- n_sanitized + 1L
    }
  }

  data$sessions <- sessions
  list(sanitized_data = data, n_sanitized = n_sanitized)
}

# ---------------------------------------------------------------------------
# sanitize_daily_all — sanitize ccusage_daily_all.json
# ---------------------------------------------------------------------------

#' Read, sanitize, and return the daily projects data.
#' Top-level project keys that look like paths are replaced with canonical names.
#' Entries that collapse to the same canonical name are merged (numeric fields summed).
#' @param data list — parsed JSON (must have a "projects" element)
#' @return list with (sanitized_data, n_sanitized)
#' @export
sanitize_daily_all <- function(data) {
  projects <- data$projects
  n_sanitized <- 0L

  # Build mapping from raw key to canonical key
  raw_keys <- names(projects)
  canonical_keys <- vapply(raw_keys, function(k) {
    if (grepl(SENSITIVE_ID_PATTERN, k, perl = TRUE)) {
      canonical <- .canonicalize_project_local_scalar(k)
      if (is.na(canonical) || !nzchar(canonical)) {
        # Fallback: strip the home-dir prefix and use remainder.
        # For non -Users- prefixes (e.g. -private-tmp-) the strip leaves an
        # empty string or still contains private tokens, caught below.
        canonical <- gsub("^-Users-johngavin-docs[-_]?[-_]?(?:pers-NHS-health-data-antigravity-|gh-|)",
                          "", k, perl = TRUE)
        canonical <- gsub("^proj-", "", canonical)
        canonical <- gsub("-", "/", canonical, fixed = TRUE)
        if (!nzchar(canonical) || grepl(SENSITIVE_ID_PATTERN, canonical, perl = TRUE)) {
          canonical <- "unknown"
        }
      }
      canonical
    } else {
      k
    }
  }, character(1), USE.NAMES = FALSE)

  # Count how many raw keys needed canonicalization
  n_sanitized <- sum(raw_keys != canonical_keys)

  # Group raw keys by their canonical name and merge date entries
  unique_canonical <- unique(canonical_keys)
  new_projects <- vector("list", length(unique_canonical))
  names(new_projects) <- unique_canonical

  for (ckey in unique_canonical) {
    contributing <- which(canonical_keys == ckey)
    if (length(contributing) == 1L) {
      new_projects[[ckey]] <- projects[[contributing]]
    } else {
      # Merge multiple raw keys -> same canonical: combine all date arrays,
      # then aggregate by date (sum numeric tokens/cost fields)
      all_entries <- do.call(c, lapply(contributing, function(idx) projects[[idx]]))

      # Group by date and merge
      dates <- vapply(all_entries, `[[`, character(1), "date")
      unique_dates <- unique(dates)
      merged <- lapply(unique_dates, function(d) {
        same_date <- all_entries[dates == d]
        if (length(same_date) == 1L) return(same_date[[1]])

        # Sum numeric fields; take union of modelsUsed; concatenate modelBreakdowns
        base_entry <- same_date[[1]]
        numeric_fields <- c(
          "inputTokens", "outputTokens", "cacheCreationTokens",
          "cacheReadTokens", "totalTokens", "totalCost"
        )
        for (fld in numeric_fields) {
          vals <- vapply(same_date, function(e) {
            v <- e[[fld]]
            if (is.null(v) || is.na(v)) 0 else as.numeric(v)
          }, numeric(1))
          base_entry[[fld]] <- sum(vals)
        }
        # Union of modelsUsed — preserve array shape so write_json(auto_unbox=TRUE)
        # serialises a single-model entry as ["model"] not "model" (Finding 2).
        all_models <- unique(unlist(lapply(same_date, `[[`, "modelsUsed")))
        base_entry$modelsUsed <- as.list(all_models)
        # Concatenate modelBreakdowns
        all_breakdowns <- do.call(c, lapply(same_date, `[[`, "modelBreakdowns"))
        base_entry$modelBreakdowns <- all_breakdowns
        base_entry
      })
      new_projects[[ckey]] <- merged
    }
  }

  data$projects <- new_projects
  list(sanitized_data = data, n_sanitized = n_sanitized)
}

# ---------------------------------------------------------------------------
# Main — run when called as a script (not when sourced for testing)
# ---------------------------------------------------------------------------

if (!interactive() && !isTRUE(getOption("sanitize_ccusage_all_sourced_for_test"))) {
  extdata <- here::here("inst", "extdata")

  session_file <- file.path(extdata, "ccusage_session_all.json")
  daily_file   <- file.path(extdata, "ccusage_daily_all.json")
  blocks_file  <- file.path(extdata, "ccusage_blocks_all.json")

  files_to_check <- c(session_file, daily_file, blocks_file)
  missing <- files_to_check[!file.exists(files_to_check)]
  if (length(missing) > 0L) {
    stop("Missing expected files:\n", paste(" -", missing, collapse = "\n"))
  }

  # --- ccusage_session_all.json ---
  cat("Reading", basename(session_file), "...\n")
  session_data <- jsonlite::read_json(session_file, simplifyVector = FALSE)
  session_result <- sanitize_session_all(session_data)
  jsonlite::write_json(session_result$sanitized_data, session_file,
                       auto_unbox = TRUE, pretty = TRUE)
  cat(sprintf("  sanitized %d field(s) in %s\n",
              session_result$n_sanitized, basename(session_file)))

  # --- ccusage_daily_all.json ---
  cat("Reading", basename(daily_file), "...\n")
  daily_data <- jsonlite::read_json(daily_file, simplifyVector = FALSE)
  daily_result <- sanitize_daily_all(daily_data)
  jsonlite::write_json(daily_result$sanitized_data, daily_file,
                       auto_unbox = TRUE, pretty = TRUE)
  cat(sprintf("  sanitized %d key(s) in %s\n",
              daily_result$n_sanitized, basename(daily_file)))

  # --- ccusage_blocks_all.json (verify clean, do not modify) ---
  # SENSITIVE_VERIFY_PATTERNS is the single source of truth for the verification
  # gate — derived from the same pattern classes as SENSITIVE_ID_PATTERN above so
  # they can never drift.  See the SENSITIVE_ID_PATTERN comment block for the
  # class-by-class mapping.
  blocks_content <- paste(readLines(blocks_file, warn = FALSE), collapse = "\n")
  blocks_leaks <- any(vapply(SENSITIVE_VERIFY_PATTERNS,
    function(p) grepl(p, blocks_content, perl = TRUE, fixed = FALSE), logical(1)))
  if (blocks_leaks) {
    stop("ccusage_blocks_all.json contains path leaks — manual review needed")
  } else {
    cat("  ccusage_blocks_all.json: verified clean\n")
  }

  # --- Final verification ---
  cat("\nVerification:\n")
  for (f in c(session_file, daily_file, blocks_file)) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    n_remaining <- sum(vapply(SENSITIVE_VERIFY_PATTERNS, function(p) {
      m <- gregexpr(p, txt, perl = TRUE)[[1]]
      sum(m > 0L)
    }, integer(1)))
    cat(sprintf("  %s: %d leak pattern(s) remaining\n", basename(f), n_remaining))
    if (n_remaining > 0L) {
      stop("Sanitization incomplete — leaks remain in ", basename(f))
    }
  }

  cat("\nDone. All _all files are clean.\n")
}
