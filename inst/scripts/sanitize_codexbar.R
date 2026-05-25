#!/usr/bin/env Rscript
# sanitize_codexbar.R — Strip PII from CodexBar usage JSON before persistence.
#
# PURPOSE
# -------
# `codexbar usage --provider all --json` emits raw PII fields:
#   accountEmail, accountOrganization, identity, loginMethod
# These MUST be removed before writing to inst/extdata/ or committing to git.
#
# `codexbar cost --provider all --json` contains NO PII and is written as-is
# after path-leak verification.
#
# USAGE
# -----
# Called by exec/refresh_codexbar.sh after capturing raw CLI output:
#
#   Rscript inst/scripts/sanitize_codexbar.R <raw_usage.json> <raw_cost.json>
#
# OUTPUT FILES (written to inst/extdata/)
#   codexbar_usage.json      — sanitised usage (PII stripped)
#   codexbar_cost_daily.json — cost data (no PII; verified clean)
#
# See also: tests/testthat/test-codexbar.R (regression gate for PII leaks)
#           R/codexbar.R                   (parsers for these outputs)

# Only load jsonlite if it is not already available (e.g. package namespace)
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required but not installed.")
}
if (!isTRUE(getOption("sanitize_codexbar_sourced_for_test"))) {
  suppressPackageStartupMessages(library(jsonlite))
}

# ---------------------------------------------------------------------------
# PII fields to strip from the usage payload
# ---------------------------------------------------------------------------

PII_USAGE_FIELDS <- c(
  "accountEmail",
  "accountOrganization",
  "identity",
  "loginMethod"
)

# Error message field: contains local filesystem paths in some provider error responses
# (e.g. "Kilo CLI session not found at /Users/johngavin/...").  We strip the message
# but preserve error.kind and error.code for diagnostics.
PATH_LEAK_FIELDS_IN_ERROR <- c("message")

# ---------------------------------------------------------------------------
# SENSITIVE_VERIFY_PATTERNS — single source of truth (mirrors sanitize_ccusage_all.R)
# ---------------------------------------------------------------------------
# When running inside the package, llmtelemetry::sensitive_verify_patterns() is
# available.  When running as a standalone Rscript (e.g. from exec/refresh_codexbar.sh),
# we define an inline fallback that mirrors R/sensitive_patterns.R exactly.

if (!exists("sensitive_verify_patterns", mode = "function")) {
  sensitive_verify_patterns <- function() {
    c(
      "Users-johngavin",
      "/Users/",
      "/private/",
      "/tmp/",
      "/var/",
      "-private-tmp-",
      "-tmp-",
      "-worktree-",
      "worktree-agent-",
      "johngavin"
    )
  }
}

SENSITIVE_VERIFY_PATTERNS <- sensitive_verify_patterns()

# ---------------------------------------------------------------------------
# strip_pii_from_usage_provider
# ---------------------------------------------------------------------------

#' Strip PII fields from a single provider usage object.
#'
#' Removes `accountEmail`, `accountOrganization`, `identity`, and `loginMethod`
#' from the top-level `usage` block of a provider entry.  Provider-level fields
#' (`provider`, `version`, `source`, `status`, `credits`) are preserved.
#'
#' @param prov list — a single provider object from the `codexbar usage --json` array
#' @return list — the same object with PII fields removed from `usage`
strip_pii_from_usage_provider <- function(prov) {
  if (!is.list(prov)) return(prov)

  # Strip from usage sub-object
  if (is.list(prov[["usage"]])) {
    for (field in PII_USAGE_FIELDS) {
      prov[["usage"]][[field]] <- NULL
    }
  }

  # Belt-and-suspenders: strip from top level in case a provider embeds them there
  for (field in PII_USAGE_FIELDS) {
    prov[[field]] <- NULL
  }

  # Strip error.message — some providers embed local filesystem paths in their
  # error messages (e.g. "session not found at /Users/...").
  if (is.list(prov[["error"]])) {
    for (field in PATH_LEAK_FIELDS_IN_ERROR) {
      prov[["error"]][[field]] <- NULL
    }
  }

  prov
}

#' Sanitize the raw codexbar usage JSON array.
#'
#' @param providers_list list — parsed JSON array from `codexbar usage --provider all --json`
#' @return list with elements:
#'   \describe{
#'     \item{sanitized}{list — the cleaned provider array}
#'     \item{n_stripped}{integer — total PII field occurrences removed}
#'   }
sanitize_usage <- function(providers_list) {
  if (!is.list(providers_list)) {
    stop("providers_list must be a list (parsed JSON array)")
  }

  n_stripped <- 0L
  sanitized <- lapply(providers_list, function(prov) {
    before_fields <- c(
      names(prov[["usage"]]),
      names(prov)
    )
    cleaned <- strip_pii_from_usage_provider(prov)
    after_fields <- c(
      names(cleaned[["usage"]]),
      names(cleaned)
    )
    n_stripped <<- n_stripped + sum(PII_USAGE_FIELDS %in% before_fields) -
                               sum(PII_USAGE_FIELDS %in% after_fields)
    cleaned
  })

  list(sanitized = sanitized, n_stripped = n_stripped)
}

# ---------------------------------------------------------------------------
# verify_no_pii_leak — scan text for forbidden patterns
# ---------------------------------------------------------------------------

#' Verify that sanitised JSON text contains no forbidden path/PII patterns.
#'
#' @param text character(1) — JSON text to scan
#' @param label character(1) — filename or label for error messages
#' @return invisible(NULL) — stops on the first pattern hit
verify_no_pii_leak <- function(text, label) {
  # Check PII field names (belt-and-suspenders)
  for (field in PII_USAGE_FIELDS) {
    if (grepl(paste0('"', field, '"'), text, fixed = TRUE)) {
      stop(sprintf(
        "%s still contains PII field '%s' after sanitization — aborting",
        label, field
      ))
    }
  }

  # Check path/username leaks
  for (pat in SENSITIVE_VERIFY_PATTERNS) {
    if (grepl(pat, text, perl = TRUE, fixed = FALSE)) {
      stop(sprintf(
        "%s contains forbidden path/PII pattern '%s' — aborting",
        label, pat
      ))
    }
  }

  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Main — run when invoked as a script
# ---------------------------------------------------------------------------

if (!interactive() &&
    !isTRUE(getOption("sanitize_codexbar_sourced_for_test"))) {

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2L) {
    stop(
      "Usage: Rscript inst/scripts/sanitize_codexbar.R <raw_usage.json> <raw_cost.json>"
    )
  }

  raw_usage_path <- args[[1L]]
  raw_cost_path  <- args[[2L]]

  # Determine output directory.
  # Arg 3 (explicit out-dir) takes priority — ensures the caller controls where
  # output lands regardless of cwd.  here::here() is the second choice (works
  # when run from the package root); cwd-relative is a last resort.
  extdata_dir <- if (length(args) >= 3L && nzchar(args[[3L]])) {
    args[[3L]]
  } else {
    tryCatch(
      here::here("inst", "extdata"),
      error = function(e) file.path("inst", "extdata")
    )
  }
  dir.create(extdata_dir, recursive = TRUE, showWarnings = FALSE)

  out_usage_path <- file.path(extdata_dir, "codexbar_usage.json")
  out_cost_path  <- file.path(extdata_dir, "codexbar_cost_daily.json")

  # --- Process usage ---
  cat("Reading", raw_usage_path, "...\n")
  if (!file.exists(raw_usage_path)) {
    stop("Raw usage file not found: ", raw_usage_path)
  }
  usage_raw <- tryCatch(
    jsonlite::read_json(raw_usage_path, simplifyVector = FALSE),
    error = function(e) stop("Failed to parse usage JSON: ", conditionMessage(e))
  )

  usage_result <- sanitize_usage(usage_raw)
  usage_json   <- jsonlite::toJSON(usage_result$sanitized, auto_unbox = TRUE, pretty = TRUE)

  # Verify before writing
  verify_no_pii_leak(usage_json, "codexbar_usage.json")

  writeLines(usage_json, out_usage_path)
  cat(sprintf("  Stripped %d PII field(s) -> %s\n",
              usage_result$n_stripped, out_usage_path))

  # --- Process cost ---
  cat("Reading", raw_cost_path, "...\n")
  if (!file.exists(raw_cost_path)) {
    stop("Raw cost file not found: ", raw_cost_path)
  }
  cost_raw  <- tryCatch(
    jsonlite::read_json(raw_cost_path, simplifyVector = FALSE),
    error = function(e) stop("Failed to parse cost JSON: ", conditionMessage(e))
  )
  cost_json <- jsonlite::toJSON(cost_raw, auto_unbox = TRUE, pretty = TRUE)

  # Cost payload has no PII — just verify path leaks
  verify_no_pii_leak(cost_json, "codexbar_cost_daily.json")

  writeLines(cost_json, out_cost_path)
  cat(sprintf("  No PII fields expected -> %s\n", out_cost_path))

  # --- Final verification ---
  cat("\nVerification:\n")
  for (info in list(
    list(path = out_usage_path, label = "codexbar_usage.json"),
    list(path = out_cost_path,  label = "codexbar_cost_daily.json")
  )) {
    txt <- paste(readLines(info$path, warn = FALSE), collapse = "\n")
    n_remaining <- sum(vapply(SENSITIVE_VERIFY_PATTERNS, function(p) {
      m <- gregexpr(p, txt, perl = TRUE)[[1L]]
      sum(m > 0L)
    }, integer(1L)))
    pii_remaining <- sum(vapply(PII_USAGE_FIELDS, function(f) {
      grepl(paste0('"', f, '"'), txt, fixed = TRUE)
    }, logical(1L)))

    cat(sprintf("  %s: %d path leak(s), %d PII field(s) remaining\n",
                info$label, n_remaining, pii_remaining))
    if (n_remaining > 0L || pii_remaining > 0L) {
      stop("Sanitization incomplete — leaks remain in ", info$label)
    }
  }

  cat("\nDone. Both CodexBar output files are clean.\n")
}
