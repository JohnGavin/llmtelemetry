# Tests for .sanitize_session_id_local() in R/sanitize_session_id.R
#
# Validates that:
#   1. Path-style session_id values are replaced with a sanitized form
#   2. Non-path IDs pass through unchanged
#   3. The same (path, project, started_at) triple yields the same hash from
#      both the R/ helper AND the inline fallback in export_dashboard_data.R
#      (roborev round V finding b — unified hash format)

# Load sanitize_session_id helper: try source from development tree first
# (devtools test workflow), fall back to package namespace (R CMD check workflow).
local({
  r_file <- tryCatch(
    normalizePath(file.path(test_path(), "..", "..", "R", "sanitize_session_id.R"),
                  mustWork = TRUE),
    error = function(e) ""
  )
  if (nzchar(r_file) && file.exists(r_file)) {
    source(r_file, local = FALSE)
  } else {
    # In R CMD check, functions are available via the package namespace.
    assign(".path_hash12",
           llmtelemetry:::.path_hash12, envir = .GlobalEnv)
    assign(".sanitize_session_id_local",
           llmtelemetry:::.sanitize_session_id_local, envir = .GlobalEnv)
  }
})

# Fixed reference timestamp so snapshot output is deterministic
# Use format= to ensure the time portion is parsed (plain as.POSIXct() without
# format= only parses the date portion for ISO 8601 strings on some platforms).
REF_TIME <- as.POSIXct("2026-05-01T10:00:00Z",
                        format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
REF_PATH <- "-Users-johngavin-docs-gh-llmtelemetry"
REF_PROJ <- "llmtelemetry"

# ── Non-path IDs pass through unchanged ───────────────────────────────────────

test_that("non-path session_id is returned unchanged", {
  plain_ids <- c("abc-123", "def-456", "2026-xyz")
  result <- .sanitize_session_id_local(
    session_ids        = plain_ids,
    canonical_projects = rep("llm", 3L),
    started_at         = rep(REF_TIME, 3L)
  )
  expect_equal(result, plain_ids)
})

# ── Path-style IDs are sanitized ──────────────────────────────────────────────

test_that("dash-prefix path is replaced with sanitized@... form", {
  result <- .sanitize_session_id_local(
    session_ids        = REF_PATH,
    canonical_projects = REF_PROJ,
    started_at         = REF_TIME
  )
  expect_match(result, "^sanitized@llmtelemetry@2026-05-01T10:00:00Z@h[0-9a-f]{12}$")
})

test_that("slash-containing path is replaced with sanitized@... form", {
  path_with_slash <- "/Users/johngavin/docs_gh/mycare"
  result <- .sanitize_session_id_local(
    session_ids        = path_with_slash,
    canonical_projects = "mycare",
    started_at         = REF_TIME
  )
  expect_match(result, "^sanitized@mycare@2026-05-01T10:00:00Z@h[0-9a-f]{12}$")
})

test_that("NA canonical_project falls back to 'unknown'", {
  result <- .sanitize_session_id_local(
    session_ids        = REF_PATH,
    canonical_projects = NA_character_,
    started_at         = REF_TIME
  )
  expect_match(result, "^sanitized@unknown@")
})

# ── Identity across both code paths (rollup_sessions.R uses helper directly; ──
# ── export_dashboard_data.R uses inline fallback when helper not loaded). ─────

test_that("helper and inline fallback produce identical hash for same inputs", {
  # Compute via the shared helper
  result_helper <- .sanitize_session_id_local(
    session_ids        = REF_PATH,
    canonical_projects = REF_PROJ,
    started_at         = REF_TIME
  )

  # Reproduce the inline fallback logic from export_dashboard_data.R
  path_hash12_fallback <- function(p) {
    if (requireNamespace("digest", quietly = TRUE)) {
      substr(digest::digest(p, algo = "md5", serialize = FALSE), 1L, 12L)
    } else {
      bytes   <- utf8ToInt(p)
      raw_val <- sum(as.numeric(bytes) * seq_along(bytes))
      sprintf("%012x", bitwAnd(abs(as.integer(raw_val %% .Machine$integer.max)),
                               as.integer(.Machine$integer.max)))
    }
  }
  sat_iso   <- format(REF_TIME, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  result_fb <- paste0("sanitized@", REF_PROJ, "@", sat_iso,
                      "@h", path_hash12_fallback(REF_PATH))

  expect_equal(result_helper, result_fb)
})

# ── Snapshot: stable output for known inputs ──────────────────────────────────

test_that("sanitize output format is stable (snapshot)", {
  inputs <- c(
    "-Users-johngavin-docs-gh-llmtelemetry",
    "/Users/johngavin/docs_gh/mycare",
    "plain-non-path-id",
    NA_character_
  )
  projs  <- c("llmtelemetry", "mycare", "llm", NA_character_)
  times  <- c(REF_TIME, REF_TIME, REF_TIME, REF_TIME)

  result <- .sanitize_session_id_local(
    session_ids        = inputs,
    canonical_projects = projs,
    started_at         = times
  )
  # Snapshot the structure (prefix up to the hash is deterministic; the hash
  # itself is also deterministic for fixed inputs, so we can snapshot the full
  # output — but we transform to remove any session-local path artifacts).
  expect_snapshot(result)
})

test_that("NA session_id does not cause paste0 recycling-length mismatch", {
  # Regression for critic C4: NA in session_ids caused logical-subscript recycling
  # when is_path subscript was used instead of which()-derived path_indices.
  ids   <- c(NA_character_, REF_PATH, "plain-id")
  projs <- c(NA_character_, REF_PROJ, "llm")
  times <- c(REF_TIME, REF_TIME, REF_TIME)

  expect_no_warning(
    result <- .sanitize_session_id_local(ids, projs, times)
  )
  # NA session_id is not path-like — should pass through unchanged
  expect_true(is.na(result[1L]))
  expect_match(result[2L], "^sanitized@")
  expect_equal(result[3L], "plain-id")
})

test_that("mixed path/non-path vector only sanitizes path entries", {
  ids    <- c("plain-id", REF_PATH, "another-plain")
  projs  <- c("llm", REF_PROJ, "footbet")
  times  <- rep(REF_TIME, 3L)

  result <- .sanitize_session_id_local(ids, projs, times)
  expect_equal(result[1L], "plain-id")
  expect_match(result[2L], "^sanitized@")
  expect_equal(result[3L], "another-plain")
})
