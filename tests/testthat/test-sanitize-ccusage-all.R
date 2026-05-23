# Tests for sanitize_ccusage_all.R helper functions.
# These unit tests construct fixture data with known path leaks and verify that
# sanitize_session_all() and sanitize_daily_all() produce clean output.

# Source the script in test mode so only functions are loaded, not the main block.
options(sanitize_ccusage_all_sourced_for_test = TRUE)
source(system.file("scripts", "sanitize_ccusage_all.R", package = "llmtelemetry"))

# ---------------------------------------------------------------------------
# Fixture helpers
# ---------------------------------------------------------------------------

make_session_fixture <- function() {
  list(
    sessions = list(
      list(
        sessionId        = "-Users-johngavin-docs-gh-llm",
        inputTokens      = 100L,
        outputTokens     = 200L,
        totalCost        = 0.5,
        lastActivity     = "2026-05-01",
        modelsUsed       = list("claude-opus-4-6"),
        modelBreakdowns  = list(),
        projectPath      = "Unknown Project"
      ),
      list(
        sessionId        = "subagents",
        inputTokens      = 50L,
        outputTokens     = 80L,
        totalCost        = 0.1,
        lastActivity     = "2026-05-02",
        modelsUsed       = list("claude-sonnet-4-6"),
        modelBreakdowns  = list(),
        projectPath      = "-Users-johngavin-docs-gh-llmtelemetry/abc12345-dead-beef-0000-111111111111"
      ),
      list(
        sessionId        = "-Users-johngavin-docs--pers-NHS-health-data-antigravity-mycare",
        inputTokens      = 10L,
        outputTokens     = 30L,
        totalCost        = 0.05,
        lastActivity     = "2026-05-03",
        modelsUsed       = list("claude-haiku-4-6"),
        modelBreakdowns  = list(),
        projectPath      = "Unknown Project"
      )
    )
  )
}

make_daily_fixture <- function() {
  list(
    projects = list(
      "-Users-johngavin-docs-gh-llm" = list(
        list(date = "2026-04-01", inputTokens = 10L, outputTokens = 5L,
             cacheCreationTokens = 0L, cacheReadTokens = 0L,
             totalTokens = 15L, totalCost = 0.05,
             modelsUsed = list("claude-opus-4-6"), modelBreakdowns = list())
      ),
      "subagents" = list(
        list(date = "2026-04-01", inputTokens = 1L, outputTokens = 2L,
             cacheCreationTokens = 0L, cacheReadTokens = 0L,
             totalTokens = 3L, totalCost = 0.01,
             modelsUsed = list("claude-sonnet-4-6"), modelBreakdowns = list())
      ),
      "-Users-johngavin-docs-gh-llmtelemetry" = list(
        list(date = "2026-04-01", inputTokens = 20L, outputTokens = 10L,
             cacheCreationTokens = 0L, cacheReadTokens = 0L,
             totalTokens = 30L, totalCost = 0.1,
             modelsUsed = list("claude-opus-4-6"), modelBreakdowns = list())
      )
    )
  )
}

FORBIDDEN_PATTERNS <- c(
  "-Users-johngavin",
  "sessionId.*Users",
  "projectPath.*Users",
  "antigravity",
  "pers-NHS"
)

contains_leak <- function(text) {
  any(vapply(FORBIDDEN_PATTERNS, function(p) grepl(p, text, perl = TRUE), logical(1)))
}

# ---------------------------------------------------------------------------
# sanitize_session_all() tests
# ---------------------------------------------------------------------------

test_that("sanitize_session_all removes path-style sessionId values", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)

  session_ids <- vapply(result$sanitized_data$sessions, `[[`, character(1), "sessionId")
  # No sessionId should start with -Users-
  expect_false(any(grepl("^-Users-", session_ids)))
})

test_that("sanitize_session_all removes path-style projectPath values", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)

  project_paths <- vapply(result$sanitized_data$sessions, `[[`, character(1), "projectPath")
  expect_false(any(grepl("^-Users-", project_paths)))
})

test_that("sanitize_session_all reports correct count of sanitized fields", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)
  # Session 1: sessionId is a path (1 field sanitized); projectPath is "Unknown Project" (0)
  # Session 2: sessionId is "subagents" (0); projectPath is a path (1)
  # Session 3: sessionId is a path (1); projectPath is "Unknown Project" (0)
  # Total = 3
  expect_equal(result$n_sanitized, 3L)
})

test_that("sanitize_session_all preserves non-path sessionId values unchanged", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)
  # "subagents" is not a path — must survive unchanged
  session_ids <- vapply(result$sanitized_data$sessions, `[[`, character(1), "sessionId")
  expect_true("subagents" %in% session_ids)
})

test_that("sanitize_session_all output contains no forbidden leak patterns", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)
  json_text <- jsonlite::toJSON(result$sanitized_data, auto_unbox = TRUE)
  expect_false(contains_leak(json_text))
})

test_that("sanitize_session_all replaces NHS path patterns", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)
  json_text <- jsonlite::toJSON(result$sanitized_data, auto_unbox = TRUE)
  expect_false(grepl("antigravity", json_text))
  expect_false(grepl("pers-NHS", json_text))
})

test_that("sanitize_session_all preserves token and cost data", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)
  # First session should still have its cost
  expect_equal(result$sanitized_data$sessions[[1]]$totalCost, 0.5)
  expect_equal(result$sanitized_data$sessions[[1]]$inputTokens, 100L)
})

test_that("sanitize_session_all stable hash: same input always same output", {
  fixture <- make_session_fixture()
  r1 <- sanitize_session_all(fixture)
  r2 <- sanitize_session_all(fixture)
  ids1 <- vapply(r1$sanitized_data$sessions, `[[`, character(1), "sessionId")
  ids2 <- vapply(r2$sanitized_data$sessions, `[[`, character(1), "sessionId")
  expect_identical(ids1, ids2)
})

# ---------------------------------------------------------------------------
# sanitize_daily_all() tests
# ---------------------------------------------------------------------------

test_that("sanitize_daily_all removes path-style project keys", {
  fixture <- make_daily_fixture()
  result <- sanitize_daily_all(fixture)
  project_names <- names(result$sanitized_data$projects)
  expect_false(any(grepl("^-Users-", project_names)))
})

test_that("sanitize_daily_all reports correct count of sanitized keys", {
  fixture <- make_daily_fixture()
  result <- sanitize_daily_all(fixture)
  # Two path-style keys: -Users-johngavin-docs-gh-llm, -Users-johngavin-docs-gh-llmtelemetry
  expect_equal(result$n_sanitized, 2L)
})

test_that("sanitize_daily_all preserves non-path keys unchanged", {
  fixture <- make_daily_fixture()
  result <- sanitize_daily_all(fixture)
  expect_true("subagents" %in% names(result$sanitized_data$projects))
})

test_that("sanitize_daily_all output contains no forbidden leak patterns", {
  fixture <- make_daily_fixture()
  result <- sanitize_daily_all(fixture)
  json_text <- jsonlite::toJSON(result$sanitized_data, auto_unbox = TRUE)
  expect_false(contains_leak(json_text))
})

test_that("sanitize_daily_all preserves entry data after key rename", {
  fixture <- make_daily_fixture()
  result <- sanitize_daily_all(fixture)
  # All canonical project entries must still have their date arrays
  for (proj_entries in result$sanitized_data$projects) {
    expect_true(is.list(proj_entries))
    expect_true(length(proj_entries) >= 1L)
  }
})

# ---------------------------------------------------------------------------
# Round-trip: write to tempfile, re-read, verify
# ---------------------------------------------------------------------------

test_that("session sanitization round-trips through JSON without leaks", {
  fixture <- make_session_fixture()
  result <- sanitize_session_all(fixture)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(result$sanitized_data, tmp, auto_unbox = TRUE, pretty = TRUE)
  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_false(contains_leak(txt))
})

test_that("daily sanitization round-trips through JSON without leaks", {
  fixture <- make_daily_fixture()
  result <- sanitize_daily_all(fixture)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(result$sanitized_data, tmp, auto_unbox = TRUE, pretty = TRUE)
  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_false(contains_leak(txt))
})

# ---------------------------------------------------------------------------
# Worktree / tmp / agent-path leak tests (issue #140)
# These tests use patterns the OLD regex '^-Users-|^/' MISSES entirely.
# They MUST be RED before the fix and GREEN after.
# ---------------------------------------------------------------------------

WORKTREE_FORBIDDEN_PATTERNS <- c(
  "-private-tmp-",      # macOS /private/tmp worktrees (e.g. -private-tmp-roborev-worktree-1234)
  "-tmp-",              # generic /tmp worktrees (e.g. -tmp-fixer-worktree-2651148717)
  "worktree-[0-9]",     # numeric-suffix worktrees
  "worktree-agent-",    # named agent worktrees (.claude/worktrees/agent-...)
  "johngavin"           # username leaked anywhere (belt-and-suspenders)
)

contains_worktree_leak <- function(text) {
  any(vapply(WORKTREE_FORBIDDEN_PATTERNS, function(p) grepl(p, text, perl = TRUE), logical(1)))
}

make_worktree_session_fixture <- function() {
  list(
    sessions = list(
      # roborev-style worktree: /private/tmp/roborev-worktree-1832056705
      list(
        sessionId   = "-private-tmp-roborev-worktree-1832056705",
        inputTokens = 10L, outputTokens = 5L, totalCost = 0.01,
        lastActivity = "2026-05-20",
        modelsUsed   = list("claude-sonnet-4-6"), modelBreakdowns = list(),
        projectPath  = "Unknown Project"
      ),
      # .claude/worktrees/agent-... style
      list(
        sessionId   = "-Users-johngavin-docs-gh-llmtelemetry-.claude-worktrees-agent-a9b480562a7b91e3e",
        inputTokens = 20L, outputTokens = 8L, totalCost = 0.02,
        lastActivity = "2026-05-20",
        modelsUsed   = list("claude-opus-4-6"), modelBreakdowns = list(),
        projectPath  = "/private/tmp/roborev-worktree-3388621982"
      ),
      # Generic /tmp worktree
      list(
        sessionId   = "-tmp-fixer-worktree-2651148717",
        inputTokens = 5L, outputTokens = 3L, totalCost = 0.005,
        lastActivity = "2026-05-21",
        modelsUsed   = list("claude-haiku-4-6"), modelBreakdowns = list(),
        projectPath  = "Unknown Project"
      ),
      # Legitimate public project — must NOT be mangled
      list(
        sessionId   = "llmtelemetry",
        inputTokens = 50L, outputTokens = 30L, totalCost = 0.1,
        lastActivity = "2026-05-22",
        modelsUsed   = list("claude-sonnet-4-6"), modelBreakdowns = list(),
        projectPath  = "Unknown Project"
      )
    )
  )
}

make_worktree_daily_fixture <- function() {
  list(
    projects = list(
      "-private-tmp-roborev-worktree-437813078" = list(
        list(date = "2026-05-19", inputTokens = 5L, outputTokens = 3L,
             cacheCreationTokens = 0L, cacheReadTokens = 0L,
             totalTokens = 8L, totalCost = 0.01,
             modelsUsed = list("claude-sonnet-4-6"), modelBreakdowns = list())
      ),
      "-private-tmp-roborev-worktree-3047898692" = list(
        list(date = "2026-05-19", inputTokens = 8L, outputTokens = 4L,
             cacheCreationTokens = 0L, cacheReadTokens = 0L,
             totalTokens = 12L, totalCost = 0.02,
             modelsUsed = list("claude-opus-4-6"), modelBreakdowns = list())
      ),
      # Legitimate public project — key must survive unchanged
      "irishbuoys" = list(
        list(date = "2026-05-19", inputTokens = 2L, outputTokens = 1L,
             cacheCreationTokens = 0L, cacheReadTokens = 0L,
             totalTokens = 3L, totalCost = 0.005,
             modelsUsed = list("claude-haiku-4-6"), modelBreakdowns = list())
      )
    )
  )
}

test_that("sanitize_session_all redacts /private/tmp worktree sessionId (issue #140)", {
  fixture <- make_worktree_session_fixture()
  result  <- sanitize_session_all(fixture)
  session_ids <- vapply(result$sanitized_data$sessions, `[[`, character(1), "sessionId")
  # No sessionId should contain the username or worktree numeric suffix
  for (pat in WORKTREE_FORBIDDEN_PATTERNS) {
    expect_false(
      any(grepl(pat, session_ids, perl = TRUE)),
      label = sprintf("sessionId contains worktree pattern '%s'", pat)
    )
  }
})

test_that("sanitize_session_all redacts /private/tmp worktree projectPath (issue #140)", {
  fixture <- make_worktree_session_fixture()
  result  <- sanitize_session_all(fixture)
  project_paths <- vapply(result$sanitized_data$sessions, `[[`, character(1), "projectPath")
  for (pat in WORKTREE_FORBIDDEN_PATTERNS) {
    expect_false(
      any(grepl(pat, project_paths, perl = TRUE)),
      label = sprintf("projectPath contains worktree pattern '%s'", pat)
    )
  }
})

test_that("sanitize_session_all preserves legitimate public project sessionId (issue #140)", {
  fixture <- make_worktree_session_fixture()
  result  <- sanitize_session_all(fixture)
  session_ids <- vapply(result$sanitized_data$sessions, `[[`, character(1), "sessionId")
  # "llmtelemetry" is a legitimate public name and must not be mangled
  expect_true("llmtelemetry" %in% session_ids)
})

test_that("sanitize_session_all worktree fixture: full JSON contains no worktree leaks (issue #140)", {
  fixture <- make_worktree_session_fixture()
  result  <- sanitize_session_all(fixture)
  json_text <- jsonlite::toJSON(result$sanitized_data, auto_unbox = TRUE)
  expect_false(contains_worktree_leak(json_text))
})

test_that("sanitize_daily_all redacts /private/tmp worktree project keys (issue #140)", {
  fixture <- make_worktree_daily_fixture()
  result  <- sanitize_daily_all(fixture)
  project_names <- names(result$sanitized_data$projects)
  for (pat in WORKTREE_FORBIDDEN_PATTERNS) {
    expect_false(
      any(grepl(pat, project_names, perl = TRUE)),
      label = sprintf("project key contains worktree pattern '%s'", pat)
    )
  }
})

test_that("sanitize_daily_all preserves legitimate public project key (issue #140)", {
  fixture <- make_worktree_daily_fixture()
  result  <- sanitize_daily_all(fixture)
  # "irishbuoys" must not be mangled
  expect_true("irishbuoys" %in% names(result$sanitized_data$projects))
})

test_that("sanitize_daily_all worktree fixture: full JSON contains no worktree leaks (issue #140)", {
  fixture <- make_worktree_daily_fixture()
  result  <- sanitize_daily_all(fixture)
  json_text <- jsonlite::toJSON(result$sanitized_data, auto_unbox = TRUE)
  expect_false(contains_worktree_leak(json_text))
})

test_that("derive_canonical_id redacts mid-string username (issue #140)", {
  # A path where the username appears mid-string after a non-^ position
  raw_id <- "-tmp-roborev-worktree-1234567890"
  result <- derive_canonical_id(raw_id)
  # Must not survive with worktree numeric suffix or tmp prefix
  expect_false(grepl("roborev", result, fixed = TRUE))
  expect_false(grepl("worktree-[0-9]", result, perl = TRUE))
  expect_false(grepl("-tmp-", result, fixed = TRUE))
  # Must be marked as sanitized
  expect_true(grepl("^sanitized@", result))
})

test_that("derive_canonical_id redacts .claude/worktrees/agent-... style path (issue #140)", {
  raw_id <- "-Users-johngavin-docs-gh-llmtelemetry-.claude-worktrees-agent-a9b480562a7b91e3e"
  result <- derive_canonical_id(raw_id)
  expect_false(grepl("johngavin", result, fixed = TRUE))
  expect_true(grepl("^sanitized@", result))
})

# ---------------------------------------------------------------------------
# Finding 1 — verification gate must cover all patterns that SENSITIVE_ID_PATTERN
# covers, so it cannot report "0 leaks" while leaks remain (#140 round-2).
# These tests prove the new WORKTREE_FORBIDDEN_PATTERNS catches strings that the
# OLD VERIFY_PATTERNS (only "Users-johngavin", "-private-tmp-", "worktree-[0-9]+",
# "worktree-agent-") would MISS.
# ---------------------------------------------------------------------------

test_that("contains_worktree_leak detects worktree-agent- pattern that old VERIFY_PATTERNS missed (Finding 1)", {
  # The OLD script VERIFY_PATTERNS had "worktree-[0-9]+" but NOT "worktree-agent-"
  # as a standalone entry (it was only added to SENSITIVE_ID_PATTERN, not VERIFY_PATTERNS).
  # This test proves the WORKTREE_FORBIDDEN_PATTERNS used in tests also catches it.
  sensitive_string <- "worktree-agent-a9b480562a7b91e3e"
  expect_true(contains_worktree_leak(sensitive_string))
})

test_that("contains_worktree_leak detects bare johngavin that old gate missed (Finding 1)", {
  # A string containing the username not preceded by -Users- is missed by
  # "-Users-johngavin" but caught by the new "johngavin" pattern.
  sensitive_string <- "sanitized@johngavin@h123abc"
  expect_true(contains_worktree_leak(sensitive_string))
})

test_that("sanitize_session_all output contains no -tmp- or johngavin leaks (Finding 1)", {
  # Verify that the sanitizer actually removes patterns the verification gate now checks.
  fixture <- make_worktree_session_fixture()
  result  <- sanitize_session_all(fixture)
  json_text <- jsonlite::toJSON(result$sanitized_data, auto_unbox = TRUE)
  # These were missed by the old verification gate; they must be absent after sanitization.
  expect_false(grepl("-tmp-fixer-worktree", json_text, fixed = TRUE),
               label = "generic -tmp- worktree must be sanitized")
  expect_false(grepl("johngavin", json_text, fixed = TRUE),
               label = "bare username must be sanitized")
})

# ---------------------------------------------------------------------------
# Finding 2 — merged modelsUsed must serialize as a JSON array, not a scalar,
# even when only one model is present after the merge (#140 round-2).
# ---------------------------------------------------------------------------

make_merge_daily_fixture <- function() {
  # Two raw project keys that both canonicalize to "unknown" (worktree / tmp
  # identifiers that have no recognisable project name), each with one date
  # entry using the same single model — forces the merge path to run.
  list(
    projects = list(
      "-private-tmp-roborev-worktree-1111111111" = list(
        list(date = "2026-05-01",
             inputTokens = 10L, outputTokens = 5L,
             cacheCreationTokens = 0L, cacheReadTokens = 0L,
             totalTokens = 15L, totalCost = 0.05,
             modelsUsed = list("claude-opus-4-7"), modelBreakdowns = list())
      ),
      "-private-tmp-roborev-worktree-2222222222" = list(
        list(date = "2026-05-01",
             inputTokens = 3L, outputTokens = 2L,
             cacheCreationTokens = 0L, cacheReadTokens = 0L,
             totalTokens = 5L, totalCost = 0.02,
             modelsUsed = list("claude-opus-4-7"), modelBreakdowns = list())
      )
    )
  )
}

test_that("sanitize_daily_all merged single-model entry has modelsUsed as list (Finding 2)", {
  fixture <- make_merge_daily_fixture()
  result  <- sanitize_daily_all(fixture)

  # Both raw keys are /private/tmp/ worktrees that canonicalize to "unknown";
  # the merge path runs and the two date entries for 2026-05-01 are combined.
  # After merge, modelsUsed must be a list (not a character vector) so that
  # write_json(auto_unbox=TRUE) emits an array, not a scalar string.
  projects <- result$sanitized_data$projects
  expect_equal(length(projects), 1L,
               label = "two same-date tmp worktree keys must merge to one canonical project")
  # The merged entry is a list with one date; check that date entry.
  merged_entry <- projects[[1]][[1]]
  expect_true(is.list(merged_entry$modelsUsed),
              label = "merged modelsUsed must be a list so JSON serializes as array")
})

test_that("sanitize_daily_all merged modelsUsed serializes as JSON array (Finding 2)", {
  fixture <- make_merge_daily_fixture()
  result  <- sanitize_daily_all(fixture)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(result$sanitized_data, tmp, auto_unbox = TRUE, pretty = TRUE)
  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")

  # With auto_unbox=TRUE a character vector of length 1 serializes as "model",
  # but a list of length 1 serializes as ["model"].  Check for the array form.
  expect_true(grepl('"modelsUsed":\\s*\\[', txt, perl = TRUE),
              label = "merged modelsUsed must serialize as JSON array, not scalar string")
})
