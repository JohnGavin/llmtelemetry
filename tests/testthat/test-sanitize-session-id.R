# Tests for .sanitize_session_id_local() in R/sanitize_session_id.R
#
# Validates that:
#   1. Path-style session_id values are replaced with a sanitized form
#   2. Non-path IDs pass through unchanged
#   3. .path_hash12() uses digest::digest() directly (no fallback; digest is
#      in Imports — roborev round V finding b, critic m8)

# Load sanitize_session_id helper: try source from development tree first
# (devtools test workflow), fall back to package namespace (R CMD check workflow).
local({
  r_file <- tryCatch(
    normalizePath(file.path(test_path(), "..", "..", "R", "sanitize_session_id.R"),
                  mustWork = TRUE),
    error = function(e) ""
  )
  assigned_names <- character(0L)
  if (nzchar(r_file) && file.exists(r_file)) {
    source(r_file, local = FALSE)
    assigned_names <- c(".path_hash12", ".sanitize_session_id_local")
  } else {
    # In R CMD check, functions are available via the package namespace.
    assign(".path_hash12",
           llmtelemetry:::.path_hash12, envir = .GlobalEnv)
    assign(".sanitize_session_id_local",
           llmtelemetry:::.sanitize_session_id_local, envir = .GlobalEnv)
    assigned_names <- c(".path_hash12", ".sanitize_session_id_local")
  }
  # Restore .GlobalEnv when the test file finishes.
  withr::defer(
    rm(list = intersect(assigned_names, ls(envir = .GlobalEnv, all.names = TRUE)),
       envir = .GlobalEnv),
    envir = testthat::teardown_env()
  )
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

# ── digest is in Imports: the direct path is the only code path ───────────────

test_that(".path_hash12 produces a 12-hex MD5 substring (digest always available)", {
  # digest is in DESCRIPTION Imports, so requireNamespace fallback is dead code.
  # Verify the direct path: first 12 hex chars of MD5.
  result <- .path_hash12(REF_PATH)
  expect_match(result, "^[0-9a-f]{12}$")
  # Must equal what digest::digest() produces directly
  expected <- substr(digest::digest(REF_PATH, algo = "md5", serialize = FALSE), 1L, 12L)
  expect_equal(result, expected)
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
