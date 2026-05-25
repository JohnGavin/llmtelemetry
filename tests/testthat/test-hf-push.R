# tests/testthat/test-hf-push.R
#
# Tests for the HuggingFace archive push mechanism (R/hf_push.R).
# All tests use dry-run mode (push = FALSE) or mock the CLI invocation so
# they never touch the network.
#
# Coverage:
#   1. Privacy guard aborts when a confidential row (mycare) is present.
#   2. Dry-run returns the expected file manifest without network calls.
#   3. working_dir column is dropped from the staged copy.
#   4. Missing source parquet produces a clear error.
#   5. .hf_cli_binary() resolves the correct binary name.
#   6. Live-push path invokes huggingface-cli with correct args (mocked).

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
# Test 5: .hf_cli_binary() resolves the correct binary
# ---------------------------------------------------------------------------

test_that(".hf_cli_binary() returns a non-empty string for a known-available binary", {
  # We can't guarantee huggingface-cli is installed in the test environment,
  # but we CAN verify the function returns something when the binary exists.
  # Mock by shimming PATH with a fake binary.
  shim_dir <- withr::local_tempdir()
  fake_bin <- file.path(shim_dir, "huggingface-cli")
  writeLines("#!/bin/sh\necho 'fake-hf-cli'", fake_bin)
  Sys.chmod(fake_bin, mode = "0755")

  withr::with_envvar(c(PATH = paste0(shim_dir, ":", Sys.getenv("PATH"))), {
    result <- llmtelemetry:::.hf_cli_binary()
    expect_equal(result, "huggingface-cli")
  })
})

test_that(".hf_cli_binary() falls back to 'hf' when huggingface-cli absent", {
  shim_dir <- withr::local_tempdir()
  # Only provide 'hf', not 'huggingface-cli'
  fake_hf <- file.path(shim_dir, "hf")
  writeLines("#!/bin/sh\necho 'fake-hf'", fake_hf)
  Sys.chmod(fake_hf, mode = "0755")

  # Override PATH so 'which huggingface-cli' fails but 'which hf' succeeds
  withr::with_envvar(c(PATH = paste0(shim_dir, ":", Sys.getenv("PATH"))), {
    result <- llmtelemetry:::.hf_cli_binary()
    expect_equal(result, "hf")
  })
})

test_that(".hf_cli_binary() aborts when neither binary is found", {
  # Use a PATH with no huggingface-cli or hf
  empty_dir <- withr::local_tempdir()
  withr::with_envvar(c(PATH = empty_dir), {
    expect_error(
      llmtelemetry:::.hf_cli_binary(),
      regexp = "HuggingFace CLI not found"
    )
  })
})

# ---------------------------------------------------------------------------
# Test 6: Live-push path invokes huggingface-cli with correct args (mocked)
# ---------------------------------------------------------------------------

test_that("hf_push_telemetry() push=TRUE calls huggingface-cli upload with correct args", {
  # Strategy: shim the PATH with a fake huggingface-cli that captures its
  # argv to a temp file and exits 0, so hf_push_telemetry(push=TRUE) runs
  # without any network call.

  shim_dir <- withr::local_tempdir()
  argv_log <- file.path(shim_dir, "argv.txt")

  fake_cli <- file.path(shim_dir, "huggingface-cli")
  writeLines(
    c("#!/bin/sh",
      paste0("echo \"$@\" >> '", argv_log, "'"),
      "exit 0"),
    fake_cli
  )
  Sys.chmod(fake_cli, mode = "0755")

  tmpdir        <- withr::local_tempdir()
  sessions_path <- .make_test_parquet(.clean_sessions(), tmpdir)
  costs_path    <- .make_test_parquet(.clean_costs(),    tmpdir)

  withr::with_envvar(c(PATH = paste0(shim_dir, ":", Sys.getenv("PATH"))), {
    result <- hf_push_telemetry(
      sessions_parquet = sessions_path,
      costs_parquet    = costs_path,
      hf_repo          = "JohnGavin/llmtelemetry-metrics",
      push             = TRUE
    )
  })

  # Result must report push (not dry-run)
  expect_false(result$dry_run)
  expect_equal(result$hf_repo, "JohnGavin/llmtelemetry-metrics")
  expect_equal(result$sessions_rows, 2L)
  expect_equal(result$costs_rows, 2L)

  # Verify CLI was called with expected arguments
  expect_true(file.exists(argv_log),
    info = "huggingface-cli shim must have been invoked")
  logged <- readLines(argv_log)
  expect_length(logged, 1L)

  # Args must include: upload, repo_id, path, ., --repo-type, dataset,
  # --commit-message
  expect_match(logged, "upload")
  expect_match(logged, "JohnGavin/llmtelemetry-metrics")
  expect_match(logged, "--repo-type dataset")
  expect_match(logged, "--commit-message")
})

test_that("hf_push_telemetry() push=TRUE aborts when CLI exits non-zero", {
  # Shim that exits 1 (simulates upload failure)
  shim_dir <- withr::local_tempdir()
  fake_cli <- file.path(shim_dir, "huggingface-cli")
  writeLines(
    c("#!/bin/sh",
      "echo 'simulated upload error' >&2",
      "exit 1"),
    fake_cli
  )
  Sys.chmod(fake_cli, mode = "0755")

  tmpdir        <- withr::local_tempdir()
  sessions_path <- .make_test_parquet(.clean_sessions(), tmpdir)
  costs_path    <- .make_test_parquet(.clean_costs(),    tmpdir)

  withr::with_envvar(c(PATH = paste0(shim_dir, ":", Sys.getenv("PATH"))), {
    expect_error(
      hf_push_telemetry(
        sessions_parquet = sessions_path,
        costs_parquet    = costs_path,
        push             = TRUE
      ),
      regexp = "upload failed"
    )
  })
})
