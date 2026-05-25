# tests/testthat/test-hf-push.R
#
# Tests for the HuggingFace archive push mechanism (R/hf_push.R).
# All tests use dry-run mode (push = FALSE) and never touch the network.
#
# Coverage:
#   1. Privacy guard aborts when a confidential row (mycare) is present.
#   2. Dry-run returns the expected file manifest without network calls.
#   3. working_dir column is dropped from the staged copy.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Write a tiny Parquet via DuckDB and return the path.
# (No arrow dependency — mirrors .hf_write_parquet_duckdb pattern.)
# Uses tempfile() so multiple calls to the same dir never overwrite each other.
.make_test_parquet <- function(df, dir = withr::local_tempdir()) {
  path <- tempfile(tmpdir = dir, fileext = ".parquet")
  con  <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "tbl", df, overwrite = TRUE)
  DBI::dbExecute(
    con,
    paste0("COPY tbl TO '", path, "' (FORMAT 'parquet', COMPRESSION 'zstd')")
  )
  path
}

# Minimal clean sessions data frame (v1 schema, no confidential rows).
.clean_sessions <- function() {
  data.frame(
    session_id        = c("s1", "s2"),
    project           = c("llm", "footbet"),
    canonical_project = c("llm", "footbet"),
    started_at        = c("2026-01-01 10:00:00", "2026-01-02 10:00:00"),
    ended_at          = c("2026-01-01 11:00:00", "2026-01-02 11:00:00"),
    duration_min      = c(60.0, 60.0),
    source            = c("unified", "unified"),
    valid_from        = as.POSIXct(c("2026-01-01", "2026-01-01"), tz = "UTC"),
    stringsAsFactors  = FALSE
  )
}

# Minimal clean costs data frame (v1 schema, no confidential rows).
.clean_costs <- function() {
  data.frame(
    cost_id           = c("llm|2026-01-01|estimated", "footbet|2026-01-01|estimated"),
    project           = c("llm", "footbet"),
    canonical_project = c("llm", "footbet"),
    date              = as.Date(c("2026-01-01", "2026-01-01")),
    source            = c("estimated", "estimated"),
    daily_cost_usd    = c(5.0, 2.5),
    n_sessions        = c(3L, 1L),
    duration_min      = c(90.0, 30.0),
    valid_from        = as.POSIXct(c("2026-01-01", "2026-01-01"), tz = "UTC"),
    stringsAsFactors  = FALSE
  )
}

# ---------------------------------------------------------------------------
# Test 1: Privacy guard aborts when a confidential row is present
# ---------------------------------------------------------------------------

test_that("hf_push_telemetry() aborts when sessions parquet contains mycare row", {
  # Fixture: add a confidential mycare row
  sessions_bad <- rbind(
    .clean_sessions(),
    data.frame(
      session_id        = "s-mycare-1",
      project           = "mycare",
      canonical_project = "mycare",
      started_at        = "2026-01-03 10:00:00",
      ended_at          = "2026-01-03 11:00:00",
      duration_min      = 60.0,
      source            = "unified",
      valid_from        = as.POSIXct("2026-01-03", tz = "UTC"),
      stringsAsFactors  = FALSE
    )
  )

  tmpdir        <- withr::local_tempdir()
  sessions_path <- .make_test_parquet(sessions_bad, tmpdir)
  costs_path    <- .make_test_parquet(.clean_costs(), tmpdir)

  # Must abort with a clear privacy guard message
  expect_error(
    hf_push_telemetry(
      sessions_parquet = sessions_path,
      costs_parquet    = costs_path,
      push             = FALSE
    ),
    regexp = "Privacy guard FAILED"
  )
})

test_that("hf_push_telemetry() aborts when costs parquet contains confidential row", {
  costs_bad <- rbind(
    .clean_costs(),
    data.frame(
      cost_id           = "mycare|2026-01-01|estimated",
      project           = "mycare",
      canonical_project = "mycare",
      date              = as.Date("2026-01-01"),
      source            = "estimated",
      daily_cost_usd    = 9.99,
      n_sessions        = 1L,
      duration_min      = 45.0,
      valid_from        = as.POSIXct("2026-01-01", tz = "UTC"),
      stringsAsFactors  = FALSE
    )
  )

  tmpdir        <- withr::local_tempdir()
  sessions_path <- .make_test_parquet(.clean_sessions(), tmpdir)
  costs_path    <- .make_test_parquet(costs_bad, tmpdir)

  expect_error(
    hf_push_telemetry(
      sessions_parquet = sessions_path,
      costs_parquet    = costs_path,
      push             = FALSE
    ),
    regexp = "Privacy guard FAILED"
  )
})

# ---------------------------------------------------------------------------
# Test 2: Dry-run returns expected manifest without network calls
# ---------------------------------------------------------------------------

test_that("hf_push_telemetry() dry-run returns correct manifest", {
  tmpdir        <- withr::local_tempdir()
  sessions_path <- .make_test_parquet(.clean_sessions(), tmpdir)
  costs_path    <- .make_test_parquet(.clean_costs(),    tmpdir)

  result <- hf_push_telemetry(
    sessions_parquet = sessions_path,
    costs_parquet    = costs_path,
    hf_repo          = "JohnGavin/llmtelemetry-metrics",
    push             = FALSE
  )

  # dry_run flag
  expect_true(result$dry_run)

  # repo matches
  expect_equal(result$hf_repo, "JohnGavin/llmtelemetry-metrics")

  # manifest contains both parquet basenames
  expect_setequal(result$files, c("sessions.parquet", "costs.parquet"))

  # row counts match clean fixtures (2 sessions, 2 cost rows)
  expect_equal(result$sessions_rows, 2L)
  expect_equal(result$costs_rows, 2L)
})

test_that("hf_push_telemetry() dry-run honours HF_DATASET_REPO env var", {
  withr::with_envvar(c(HF_DATASET_REPO = "JohnGavin/test-repo"), {
    tmpdir        <- withr::local_tempdir()
    sessions_path <- .make_test_parquet(.clean_sessions(), tmpdir)
    costs_path    <- .make_test_parquet(.clean_costs(),    tmpdir)

    result <- hf_push_telemetry(
      sessions_parquet = sessions_path,
      costs_parquet    = costs_path,
      push             = FALSE
    )

    expect_equal(result$hf_repo, "JohnGavin/test-repo")
  })
})

# ---------------------------------------------------------------------------
# Test 3: working_dir column is dropped from the staged copy
# ---------------------------------------------------------------------------

test_that("hf_push_telemetry() drops working_dir column if present", {
  # Add a working_dir column with absolute paths to the sessions fixture
  sessions_with_path <- cbind(
    .clean_sessions(),
    data.frame(
      working_dir = c("/Users/johngavin/docs_gh/llm",
                      "/Users/johngavin/docs_gh/footbet"),
      stringsAsFactors = FALSE
    )
  )

  tmpdir        <- withr::local_tempdir()
  sessions_path <- .make_test_parquet(sessions_with_path, tmpdir)
  costs_path    <- .make_test_parquet(.clean_costs(), tmpdir)

  # Dry-run must succeed (working_dir is not a confidential PROJECT — just
  # a path leak risk that is stripped before staging)
  result <- hf_push_telemetry(
    sessions_parquet = sessions_path,
    costs_parquet    = costs_path,
    push             = FALSE
  )

  # Success; row count preserved (neither row is from a confidential project)
  expect_true(result$dry_run)
  expect_equal(result$sessions_rows, 2L)

  # Verify the column is gone from the in-memory cleaned data.
  # We test this by calling the internal guard helper directly.
  df_loaded <- llmtelemetry:::.hf_load_and_guard(sessions_path, "sessions")
  df_clean  <- llmtelemetry:::.hf_drop_path_columns(df_loaded)
  expect_false("working_dir" %in% names(df_clean))
})

# ---------------------------------------------------------------------------
# Test 4: Missing source parquet produces a clear error
# ---------------------------------------------------------------------------

test_that("hf_push_telemetry() errors when sessions parquet does not exist", {
  tmpdir     <- withr::local_tempdir()
  costs_path <- .make_test_parquet(.clean_costs(), tmpdir)

  expect_error(
    hf_push_telemetry(
      sessions_parquet = file.path(tmpdir, "nonexistent_sessions.parquet"),
      costs_parquet    = costs_path,
      push             = FALSE
    ),
    regexp = "Source parquet not found"
  )
})

# ---------------------------------------------------------------------------
# Test 5: Token resolver — HF_TOKEN env-var fallback (CI path)
# ---------------------------------------------------------------------------

test_that(".hf_resolve_token_path() returns existing file path unchanged", {
  tmpdir <- withr::local_tempdir()
  token_file <- file.path(tmpdir, "token")
  writeLines("fake-local-token", token_file)

  result <- llmtelemetry:::.hf_resolve_token_path(token_file, tmpdir)
  expect_equal(result, token_file)
})

test_that(".hf_resolve_token_path() falls back to HF_TOKEN env var when file absent", {
  tmpdir <- withr::local_tempdir()
  absent_path <- file.path(tmpdir, "nonexistent_token")

  withr::with_envvar(c(HF_TOKEN = "ci-test-token-value"), {
    result <- llmtelemetry:::.hf_resolve_token_path(absent_path, tmpdir)

    # Returned path must exist and be inside tmpdir (transient file)
    expect_true(file.exists(result))
    expect_true(startsWith(result, tmpdir))

    # File permissions: owner-only read/write (0600 octal = 384 decimal).
    # file.info()$mode stores the full mode integer; mask with 0777 octal
    # (511 decimal) to isolate the permission bits, then compare to 0600
    # octal (384 decimal).
    info <- file.info(result)
    perm_bits <- bitwAnd(as.integer(info$mode), 511L)  # 511L = 0777 octal
    expect_equal(perm_bits, 384L)                       # 384L = 0600 octal

    # Content must equal what was in the env var (token written to file)
    written <- readLines(result, warn = FALSE)
    expect_equal(written, "ci-test-token-value")
  })
})

test_that(".hf_resolve_token_path() aborts when neither file nor HF_TOKEN present", {
  tmpdir      <- withr::local_tempdir()
  absent_path <- file.path(tmpdir, "nonexistent_token")

  withr::with_envvar(c(HF_TOKEN = ""), {
    expect_error(
      llmtelemetry:::.hf_resolve_token_path(absent_path, tmpdir),
      regexp = "HuggingFace token not found"
    )
  })
})

test_that("hf_push_telemetry() dry-run succeeds with HF_TOKEN env var (no local token file)", {
  # Simulate a CI environment where no local token file exists.
  # Because push = FALSE (dry-run) the token resolver is not reached —
  # the function returns before the live-push block.  This test verifies
  # the dry-run path is unaffected by the absence of the token file.
  tmpdir        <- withr::local_tempdir()
  sessions_path <- .make_test_parquet(.clean_sessions(), tmpdir)
  costs_path    <- .make_test_parquet(.clean_costs(),    tmpdir)
  absent_token  <- file.path(tmpdir, "no_token_here")

  # No HF_TOKEN in env, no file — dry-run must still succeed (token not needed)
  result <- hf_push_telemetry(
    sessions_parquet = sessions_path,
    costs_parquet    = costs_path,
    token_path       = absent_token,
    push             = FALSE   # dry-run: token path never checked
  )

  expect_true(result$dry_run)
  expect_equal(result$sessions_rows, 2L)
})

# ---------------------------------------------------------------------------
# Test 6: GIT_ASKPASS helper is prompt-aware (username vs password)
# ---------------------------------------------------------------------------

test_that(".hf_make_askpass() returns repo owner for Username prompt", {
  tmpdir     <- withr::local_tempdir()
  token_file <- file.path(tmpdir, "token")
  writeLines("fake-token-value", token_file)

  script <- llmtelemetry:::.hf_make_askpass(
    token_path = token_file,
    workdir    = tmpdir,
    hf_repo    = "JohnGavin/llmtelemetry-metrics"
  )

  # Invoke the helper with a Username prompt (as git does)
  result <- system2(script, args = "Username for 'https://huggingface.co'",
                    stdout = TRUE, stderr = FALSE)
  expect_equal(result, "JohnGavin")
})

test_that(".hf_make_askpass() returns token content for Password prompt", {
  tmpdir     <- withr::local_tempdir()
  token_file <- file.path(tmpdir, "token")
  # Write token without trailing newline (trimmed, as CI path produces)
  writeLines("fake-token-value", token_file)

  script <- llmtelemetry:::.hf_make_askpass(
    token_path = token_file,
    workdir    = tmpdir,
    hf_repo    = "JohnGavin/llmtelemetry-metrics"
  )

  # Invoke with a Password prompt
  result <- system2(script, args = "Password for 'https://huggingface.co'",
                    stdout = TRUE, stderr = FALSE)
  # writeLines adds a newline; system2 stdout strips trailing newlines per line
  expect_equal(result, "fake-token-value")
})

test_that(".hf_make_askpass() extracts correct owner from multi-part repo names", {
  tmpdir     <- withr::local_tempdir()
  token_file <- file.path(tmpdir, "token")
  writeLines("tok", token_file)

  script <- llmtelemetry:::.hf_make_askpass(
    token_path = token_file,
    workdir    = tmpdir,
    hf_repo    = "MyOrg/my-dataset-repo"
  )

  result <- system2(script, args = "Username for 'https://huggingface.co'",
                    stdout = TRUE, stderr = FALSE)
  expect_equal(result, "MyOrg")
})

# ---------------------------------------------------------------------------
# Test 7: Token trimming — trailing whitespace stripped before writing
# ---------------------------------------------------------------------------

test_that(".hf_resolve_token_path() trims trailing newline from HF_TOKEN env var", {
  tmpdir      <- withr::local_tempdir()
  absent_path <- file.path(tmpdir, "nonexistent_token")

  # Simulate a token with a trailing newline (common from gh secret set)
  withr::with_envvar(c(HF_TOKEN = "my-real-token\n"), {
    result <- llmtelemetry:::.hf_resolve_token_path(absent_path, tmpdir)

    written <- readLines(result, warn = FALSE)
    # Must be stripped — no empty line at end
    expect_equal(written, "my-real-token")
  })
})
