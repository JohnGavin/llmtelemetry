# test-blocklist.R — Tests for read_blocklist(), write_blocklist(),
# check_blocklist(), and ack_loop()

# Helper: build a minimal loops tibble for blocklist tests
make_loop_row <- function(
  hash        = "abc123",
  severity    = "major",
  file        = "R/foo.R",
  tier        = "block",
  cycles      = 6L,
  ack_by      = NA_character_,
  ack_reason  = NA_character_,
  ack_until   = as.POSIXct(NA)
) {
  tibble::tibble(
    content_hash         = hash,
    severity             = severity,
    primary_file         = file,
    summary              = paste("Problem in", file, "needs attention urgently"),
    first_seen           = as.POSIXct("2026-04-01", tz = "UTC"),
    last_seen            = as.POSIXct("2026-04-07", tz = "UTC"),
    cycles               = cycles,
    tier                 = tier,
    fix_commit_shas      = "sha001",
    estimated_wasted_usd = 0.60,
    ack_by               = ack_by,
    ack_at               = as.POSIXct(NA),
    ack_reason           = ack_reason,
    ack_until            = ack_until,
    updated_at           = as.POSIXct(NA)
  )
}

# ============================================================================
# 1. read_blocklist(): returns empty tibble on missing file
# ============================================================================

test_that("read_blocklist() returns empty tibble on missing file", {
  withr::with_envvar(
    c(ROBOREV_BLOCKLIST_PATH = "/tmp/does_not_exist_xyz987.json"),
    {
      result <- read_blocklist()
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 0L)
      # Schema columns must be present
      expected_cols <- c(
        "content_hash", "severity", "primary_file", "summary",
        "cycles", "tier", "ack_by", "ack_at", "ack_reason", "ack_until"
      )
      expect_true(all(expected_cols %in% names(result)))
    }
  )
})

# ============================================================================
# 2. write_blocklist(): roundtrip read-back
# ============================================================================

test_that("write_blocklist() writes block/escalate rows and read_blocklist() reads them back", {
  tmp_path <- withr::local_tempfile(fileext = ".json")

  loop_row <- make_loop_row(tier = "block")
  withr::with_envvar(
    c(ROBOREV_BLOCKLIST_PATH = tmp_path),
    {
      write_blocklist(loop_row)
      back <- read_blocklist()
    }
  )

  expect_equal(nrow(back), 1L)
  expect_equal(back$content_hash, "abc123")
  expect_equal(back$tier, "block")
})

test_that("write_blocklist() excludes 'watch' tier rows", {
  tmp_path <- withr::local_tempfile(fileext = ".json")

  watch_row <- make_loop_row(tier = "watch")
  withr::with_envvar(
    c(ROBOREV_BLOCKLIST_PATH = tmp_path),
    {
      write_blocklist(watch_row)
      back <- read_blocklist()
    }
  )

  # watch rows should not appear in the blocklist
  expect_equal(nrow(back), 0L)
})

test_that("write_blocklist() excludes acknowledged rows (indefinite ack)", {
  tmp_path <- withr::local_tempfile(fileext = ".json")

  acked_row <- make_loop_row(
    tier       = "block",
    ack_by     = "john",
    ack_reason = "fixed in PR #50"
  )
  withr::with_envvar(
    c(ROBOREV_BLOCKLIST_PATH = tmp_path),
    {
      write_blocklist(acked_row)
      back <- read_blocklist()
    }
  )

  expect_equal(nrow(back), 0L)
})

test_that("write_blocklist() includes rows whose ack_until has expired", {
  tmp_path <- withr::local_tempfile(fileext = ".json")

  expired_ack_row <- make_loop_row(
    tier      = "block",
    ack_by    = "john",
    ack_until = as.POSIXct("2026-01-01", tz = "UTC")  # past date
  )
  withr::with_envvar(
    c(ROBOREV_BLOCKLIST_PATH = tmp_path),
    {
      write_blocklist(expired_ack_row)
      back <- read_blocklist()
    }
  )

  expect_equal(nrow(back), 1L)
})

# ============================================================================
# 3. write_blocklist() atomicity: tmp file cleaned on failure
# ============================================================================

test_that("write_blocklist() tmp file does not persist after successful write", {
  tmp_dir  <- withr::local_tempdir()
  bl_path  <- file.path(tmp_dir, "blocklist.json")

  loop_row <- make_loop_row()
  withr::with_envvar(
    c(ROBOREV_BLOCKLIST_PATH = bl_path),
    write_blocklist(loop_row)
  )

  # The PID-tagged tmp file should be gone after rename
  pid_tmp_pattern <- paste0(bl_path, ".*\\.tmp$")
  tmp_files <- list.files(tmp_dir, pattern = "\\.tmp$", full.names = TRUE)
  expect_equal(length(tmp_files), 0L)
})

# ============================================================================
# 4. check_blocklist(): match by file path
# ============================================================================

test_that("check_blocklist() matches by primary_file", {
  tmp_path <- withr::local_tempfile(fileext = ".json")

  loop_row <- make_loop_row(file = "R/sanitize.R", tier = "block")
  withr::with_envvar(
    c(ROBOREV_BLOCKLIST_PATH = tmp_path),
    {
      write_blocklist(loop_row)
      result <- check_blocklist(
        agent_prompt_text = "Please fix something generic",
        files_mentioned   = c("R/sanitize.R", "R/other.R")
      )
    }
  )

  expect_true(result$blocked)
  expect_equal(nrow(result$matched_entries), 1L)
  expect_equal(result$matched_entries$primary_file, "R/sanitize.R")
})

test_that("check_blocklist() returns not blocked when file not in list", {
  tmp_path <- withr::local_tempfile(fileext = ".json")

  loop_row <- make_loop_row(file = "R/sanitize.R", tier = "block")
  withr::with_envvar(
    c(ROBOREV_BLOCKLIST_PATH = tmp_path),
    {
      write_blocklist(loop_row)
      result <- check_blocklist(
        agent_prompt_text = "Some unrelated work",
        files_mentioned   = c("R/rollup_costs.R")
      )
    }
  )

  expect_false(result$blocked)
})

# ============================================================================
# 5. check_blocklist(): match by Jaccard prompt similarity
# ============================================================================

test_that("check_blocklist() matches by Jaccard prompt similarity above 0.5", {
  tmp_path <- withr::local_tempfile(fileext = ".json")

  # Summary in the blocklist row
  loop_row <- make_loop_row(tier = "block")
  # Override summary to something specific
  loop_row$summary <- "sanitizer fails handle NULL bytes narrow edge case"

  withr::with_envvar(
    c(ROBOREV_BLOCKLIST_PATH = tmp_path),
    {
      jsonlite::write_json(loop_row, tmp_path, auto_unbox = TRUE, pretty = TRUE)
      # Prompt that is highly similar to the summary
      result <- check_blocklist(
        agent_prompt_text = "The sanitizer function fails to handle NULL bytes in the narrow edge case",
        files_mentioned   = NULL
      )
    }
  )

  expect_true(result$blocked)
})

test_that("check_blocklist() does NOT match low-similarity prompt", {
  tmp_path <- withr::local_tempfile(fileext = ".json")

  loop_row <- make_loop_row(tier = "block")
  loop_row$summary <- "sanitizer fails handle NULL bytes narrow edge case"

  withr::with_envvar(
    c(ROBOREV_BLOCKLIST_PATH = tmp_path),
    {
      jsonlite::write_json(loop_row, tmp_path, auto_unbox = TRUE, pretty = TRUE)
      result <- check_blocklist(
        agent_prompt_text = "Update vignette caption with new data source reference",
        files_mentioned   = NULL
      )
    }
  )

  expect_false(result$blocked)
})

# ============================================================================
# 6. ack_loop(): sets ack fields and removes from active block
# ============================================================================

test_that("ack_loop() sets ack fields and acknowledges the loop entry", {
  tmp_path <- withr::local_tempfile(fileext = ".json")

  loop_row <- make_loop_row(tier = "block")
  withr::with_envvar(
    c(ROBOREV_BLOCKLIST_PATH = tmp_path),
    {
      # Write the row first
      jsonlite::write_json(loop_row, tmp_path, auto_unbox = TRUE, pretty = TRUE)

      # Acknowledge it
      ack_loop(
        content_hash = "abc123",
        reason       = "fixed in PR #101",
        by           = "test-user"
      )

      # Read back the full JSON
      back <- read_blocklist()
    }
  )

  # After ack, the ack fields are set
  expect_equal(back$ack_by,     "test-user")
  expect_equal(back$ack_reason, "fixed in PR #101")
  expect_true(is.na(back$ack_until))  # indefinite ack
})

test_that("ack_loop() errors on unknown content_hash", {
  tmp_path <- withr::local_tempfile(fileext = ".json")
  loop_row <- make_loop_row(tier = "block")
  withr::with_envvar(
    c(ROBOREV_BLOCKLIST_PATH = tmp_path),
    {
      jsonlite::write_json(loop_row, tmp_path, auto_unbox = TRUE, pretty = TRUE)
      expect_error(
        ack_loop("unknown-hash-999", reason = "test"),
        regexp = "not found in blocklist"
      )
    }
  )
})

test_that("ack_loop() with 'until' sets ack_until correctly", {
  tmp_path <- withr::local_tempfile(fileext = ".json")
  loop_row <- make_loop_row(tier = "block")

  withr::with_envvar(
    c(ROBOREV_BLOCKLIST_PATH = tmp_path),
    {
      jsonlite::write_json(loop_row, tmp_path, auto_unbox = TRUE, pretty = TRUE)

      ack_loop(
        content_hash = "abc123",
        reason       = "suppressed until next sprint",
        until        = as.POSIXct("2026-06-01", tz = "UTC"),
        by           = "test-user"
      )

      back <- read_blocklist()
    }
  )

  expect_equal(as.Date(back$ack_until), as.Date("2026-06-01"))
})

# ============================================================================
# 7. write_blocklist() includes escalate rows
# ============================================================================

test_that("write_blocklist() includes escalate tier rows", {
  tmp_path <- withr::local_tempfile(fileext = ".json")

  esc_row <- make_loop_row(tier = "escalate", cycles = 12L)
  withr::with_envvar(
    c(ROBOREV_BLOCKLIST_PATH = tmp_path),
    {
      write_blocklist(esc_row)
      back <- read_blocklist()
    }
  )

  expect_equal(nrow(back), 1L)
  expect_equal(back$tier, "escalate")
})
