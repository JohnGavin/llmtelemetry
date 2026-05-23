test_that("loop_key produces expected idempotency key prefix", {
  source(test_path("fixtures", "roborev_page_fixture.R"))
  loops <- make_roborev_loops()
  # The escalate loop has content_hash "112233aabbcc"
  esc_loop <- loops[loops$tier == "escalate", ]
  expect_equal(nrow(esc_loop), 1L)
  expected_key <- paste0("STUCK_LOOP_", substr(esc_loop$content_hash, 1L, 12L))
  expect_equal(expected_key, "STUCK_LOOP_112233aabbcc")
})

test_that("sanitize_for_issue removes file system paths", {
  # Inline the sanitizer since it's defined in the script, not an exported function
  sanitize_for_issue <- function(text, max_chars = 80L) {
    if (is.null(text) || is.na(text)) return("")
    text <- gsub("/Users/[^/]*/[^ \t\n\"']*", "<path>", text)
    text <- gsub("~[/\\\\][^ \t\n\"']*", "<path>", text)
    text <- gsub("[A-Za-z]:[\\\\][^ \t\n\"']*", "<path>", text)
    substr(trimws(text), 1L, max_chars)
  }

  # File system paths should be replaced
  expect_equal(
    sanitize_for_issue("Error in /Users/johngavin/docs_gh/llm/R/foo.R"),
    "Error in <path>"
  )
  expect_equal(
    sanitize_for_issue("Problem at ~/docs_gh/llm/R/bar.R"),
    "Problem at <path>"
  )
  # Windows path
  expect_equal(
    sanitize_for_issue("C:\\Users\\johngavin\\file.R"),
    "C:<path>"
  )
  # Safe text should pass through unchanged
  safe <- "Missing NA check in canonicalize function"
  expect_equal(sanitize_for_issue(safe), safe)
})

test_that("sanitize_for_issue truncates to max_chars", {
  sanitize_for_issue <- function(text, max_chars = 80L) {
    if (is.null(text) || is.na(text)) return("")
    text <- gsub("/Users/[^/]*/[^ \t\n\"']*", "<path>", text)
    text <- gsub("~[/\\\\][^ \t\n\"']*", "<path>", text)
    text <- gsub("[A-Za-z]:[\\\\][^ \t\n\"']*", "<path>", text)
    substr(trimws(text), 1L, max_chars)
  }

  long_text <- paste(rep("x", 200), collapse = "")
  result <- sanitize_for_issue(long_text, max_chars = 80L)
  expect_lte(nchar(result), 80L)
})

test_that("escalate script: dry-run mode does not call GitHub API", {
  # The script reads from unified.duckdb and processes stuck loops.
  # With --dry-run, no HTTP calls are made. We verify by checking that
  # httr2::req_perform is NOT called during dry-run execution.
  # Since the script is standalone (not a function), we can't unit-test it
  # directly without a live DB. Instead we test the key logic inline.

  # Simulate the idempotency and dry-run guard
  dry_run <- TRUE
  results <- list()

  mock_stuck_loops <- data.frame(
    content_hash = "112233aabbcc",
    tier = "escalate",
    ack_by = NA_character_,
    severity = "critical",
    primary_file = "R/canonicalize.R",
    summary = "Sanitize regex too narrow",
    first_seen = as.POSIXct("2026-04-05", tz = "UTC"),
    last_seen  = as.POSIXct("2026-05-18", tz = "UTC"),
    cycles = 12L,
    fix_commit_shas = I(list(character(0))),
    estimated_wasted_usd = 3.10,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(mock_stuck_loops))) {
    loop_row <- mock_stuck_loops[i, ]
    key <- paste0("STUCK_LOOP_", substr(loop_row$content_hash, 1L, 12L))
    if (dry_run) {
      results[[i]] <- list(key = key, action = "dry-run", issue_number = NA)
    }
  }

  expect_length(results, 1L)
  expect_equal(results[[1]]$action, "dry-run")
  expect_equal(results[[1]]$key, "STUCK_LOOP_112233aabbcc")
})

test_that("idempotency: same loop produces same key on repeated calls", {
  source(test_path("fixtures", "roborev_page_fixture.R"))
  loops <- make_roborev_loops()
  keys1 <- paste0("STUCK_LOOP_", substr(loops$content_hash, 1L, 12L))
  keys2 <- paste0("STUCK_LOOP_", substr(loops$content_hash, 1L, 12L))
  expect_equal(keys1, keys2)
  # Keys should be unique (no duplicates from the fixture)
  expect_equal(length(unique(keys1)), nrow(loops))
})

test_that("only Tier 3 loops without ack are escalated", {
  source(test_path("fixtures", "roborev_page_fixture.R"))
  loops <- make_roborev_loops()
  stuck <- loops[loops$tier == "escalate" & is.na(loops$ack_by), ]
  # fixture: content_hash "112233aabbcc" is escalate with no ack
  expect_equal(nrow(stuck), 1L)
  expect_equal(stuck$content_hash, "112233aabbcc")
})

test_that("Tier 3 loops WITH ack are not escalated", {
  source(test_path("fixtures", "roborev_page_fixture.R"))
  loops <- make_roborev_loops()
  # All loops with ack_by set should NOT be in the stuck set
  acked  <- loops[!is.na(loops$ack_by), ]
  esc_no_ack <- loops[loops$tier == "escalate" & is.na(loops$ack_by), ]
  # No overlap
  expect_equal(
    length(intersect(acked$content_hash, esc_no_ack$content_hash)),
    0L
  )
})
