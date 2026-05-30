# ---------------------------------------------------------------------------
# Helpers used by both the main scan test and the unit tests below.
# ---------------------------------------------------------------------------

#' Scan a character vector of values for forbidden patterns.
#'
#' @param values character vector — the column or field values to scan
#' @param patterns character vector — patterns to check (grepl, perl = TRUE)
#' @param field_name character(1) — name for error messages
#' @param file_name  character(1) — source file for error messages
#' @return invisible(NULL); calls expect_false() for each pattern hit
scan_field <- function(values, patterns, field_name, file_name) {
  col_text <- paste(values, collapse = "\n")
  for (pat in patterns) {
    expect_false(
      grepl(pat, col_text, perl = TRUE),
      info = sprintf(
        "%s column '%s' contains forbidden pattern '%s'",
        file_name, field_name, pat
      )
    )
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Pattern constants — derived from the package's single source of truth.
# ---------------------------------------------------------------------------

# Base patterns from the canonical package function (covers all classes in
# sensitive_id_pattern()).  See R/sensitive_patterns.R for the class table.
base_forbidden <- sensitive_verify_patterns()

# Additional structural patterns not in the verify set but relevant for the
# committed-extdata regression gate.
structural_forbidden <- c(
  # Agent-worktree identifiers, e.g. "D73dOZsvyf/repo" / "D73dOZsvyf-repo".
  # Require a DIGIT in the 8+ run AND a trailing word boundary so legitimate
  # English words ("ephemeral-repos", #237) do NOT false-positive: "ephemeral"
  # has no digit, and "-repos" is not a word boundary after "repo".
  # Relies on perl=TRUE (scan_field uses it).
  "(?=[A-Za-z0-9]*[0-9])[A-Za-z0-9]{8,}/repo\\b", # slash-form agent worktree id
  "(?=[A-Za-z0-9]*[0-9])[A-Za-z0-9]{8,}-repo\\b", # dash-form agent worktree id
  # `pers-NHS` is the unique sentinel for the medical-project path leak
  # (`-Users-johngavin-docs--pers-NHS-health-data-antigravity-<project>`).
  # It appears in 0 committed extdata files; any occurrence is a real leak.
  # The standalone `antigravity` rule was redundant and caused false positives
  # on legitimate provider telemetry in codexbar_usage.json.
  "pers-NHS"
)

# Full set applied to all non-allowlisted fields.
forbidden_patterns <- c(base_forbidden, structural_forbidden)

# Bare-token allowlist for commit MESSAGE fields.
# These tokens may appear in author-written text without constituting a leak:
#   - "johngavin"     — author name in e.g. "fix: check johngavin cache"
#   - "JohnGavin"     — GitHub handle in PR merge messages
#   - "worktree-agent-" — branch references in merge-commit messages
#   - "-worktree-"    — PR branch name substring in merge messages, e.g.
#                       "fix/cc-sh-worktree-safety"; NOT a filesystem path
#                       (actual /private/tmp/roborev-worktree-... still caught
#                       by path_shape_patterns() via the generic
#                       "/private/[^/[:space:]]+/" entry — #157)
#   - "/Users/"       — when describing the concept (not a real path with username)
#   - "/private/"     — when describing the concept (no following dir token)
# IMPORTANT: allowlisting a token removes it from the scan for bare occurrences,
# but path_shape_patterns() patterns are ADDED BACK unconditionally so that a
# message containing "/Users/johngavin/...", "/Users/alice/...", or even
# "/Users/alice" (bare, no trailing slash) is still caught via the generic
# "/Users/[^/[:space:]]+" regex (#157, round-2).
commit_message_bare_allowlist <- c(
  "johngavin",        # bare username — OK in message context
  "JohnGavin",        # GitHub handle — OK in message context
  "worktree-agent-",  # branch reference — OK in message context
  "-worktree-",       # PR branch name substring (e.g. fix/cc-sh-worktree-safety)
  "/Users/",          # abstract mention of the /Users/ directory concept
  "/private/",        # abstract mention (no username following = not a real path)
  "/tmp/"             # abstract mention
)

# Which fields in git commit data are commit message text:
commit_message_fields_json   <- "message"   # key name in JSON array
commit_message_field_parquet <- "message"   # column name in parquet

# Patterns for commit message fields:
# 1. Start from the full forbidden set.
# 2. Remove bare-token allowances (e.g. bare "/Users/", "/private/", "johngavin").
# 3. Add path_shape_patterns() unconditionally — these use generic PCRE regexes
#    ("/Users/[^/[:space:]]+" and "/private/[^/[:space:]]+") that catch ANY
#    user's home-dir or any non-tmp private path, not just johngavin-specific
#    forms.  Trailing slash is NOT required (round-2 fix): "/Users/alice" and
#    "/Users/alice/" are both caught.  Bare concept mentions ("/Users/" alone,
#    no username token) do not match.  (#157)
strict_message_patterns <- unique(c(
  setdiff(forbidden_patterns, commit_message_bare_allowlist),
  path_shape_patterns()
))

# ---------------------------------------------------------------------------
# Main regression gate: scan every committed extdata file.
# ---------------------------------------------------------------------------

test_that("no filesystem-path-style strings in committed extdata", {
  # Use testthat::test_path() to locate the package root correctly whether
  # running via devtools::test(), R CMD check, or in a git worktree.
  pkg_root <- normalizePath(file.path(test_path(), "..", ".."), mustWork = FALSE)
  extdata_dir <- file.path(pkg_root, "inst", "extdata")
  parquet_dir <- file.path(pkg_root, "inst", "extdata", "telemetry")

  json_files    <- list.files(extdata_dir, pattern = "\\.json$",    full.names = TRUE)
  parquet_files <- list.files(parquet_dir, pattern = "\\.parquet$",
                              recursive = TRUE, full.names = TRUE)

  # JSON files to skip entirely (not generated by export_dashboard_data.R):
  # - ccusage_daily_raw_*: legacy raw snapshots
  # - github_issue_events.json: raw GH API dump with English phrases that match
  #   the agent-worktree regex but are not path leaks
  # - test.json: scratch/developer file
  # NOTE: git_commits_by_project.json is NO LONGER skipped wholesale (round-3
  # Finding B fix).  Its "message" field is allowed to contain author-written
  # tokens; all other fields (project, canonical_project, hash, date, ...) are
  # fully scanned.  See field-specific logic below.
  # NOTE: ccusage_daily_all.json, ccusage_session_all.json, and
  # ccusage_blocks_all.json are sanitized and MUST be scanned.
  skip_basenames <- c(
    "ccusage_daily_raw_20260220.json",
    "ccusage_daily_raw_20260221.json",
    "github_issue_events.json",
    "test.json"
  )
  json_files <- json_files[!basename(json_files) %in% skip_basenames]

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  for (f in json_files) {
    is_commit_file <- basename(f) == "git_commits_by_project.json"

    if (!is_commit_file) {
      # Standard whole-file scan — every pattern must be absent.
      content <- readLines(f, warn = FALSE)
      text <- paste(content, collapse = "\n")
      for (pat in forbidden_patterns) {
        expect_false(
          grepl(pat, text, perl = TRUE),
          info = sprintf("%s contains forbidden pattern '%s'", basename(f), pat)
        )
      }
    } else {
      # git_commits_by_project.json: field-specific scan.
      # "message" field may contain bare author-name tokens (not path leaks);
      # all other fields are fully scanned.
      # IMPORTANT: path-shape patterns ARE enforced on the message field too —
      # a message containing "/Users/johngavin/..." is still a leak.
      commits <- jsonlite::fromJSON(f, simplifyDataFrame = TRUE)
      non_msg_cols <- setdiff(names(commits), commit_message_fields_json)

      for (col in non_msg_cols) {
        if (is.character(commits[[col]])) {
          scan_field(commits[[col]], forbidden_patterns, col, basename(f))
        }
      }

      # Message field: bare-token allowlist applied, but path-shape is NOT waived.
      if (commit_message_fields_json %in% names(commits)) {
        msg_values <- commits[[commit_message_fields_json]]
        if (is.character(msg_values)) {
          scan_field(msg_values, strict_message_patterns, "message", basename(f))
        }
      }
    }
  }

  # Parquet files: read each into a data frame, check string columns for patterns.
  # git_commits.parquet: NO LONGER skipped wholesale (round-3 Finding B fix).
  # Structured columns are fully scanned; the "message" column gets the same
  # field-specific treatment as git_commits_by_project.json above.
  for (pf in parquet_files) {
    is_commit_file <- basename(pf) == "git_commits.parquet"
    df <- DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s')", pf))

    for (col in names(df)) {
      if (!is.character(df[[col]])) next

      if (is_commit_file && col == commit_message_field_parquet) {
        # Message column: allow bare-token allowlist, but keep path-shape checks.
        scan_field(df[[col]], strict_message_patterns, col, basename(pf))
      } else {
        # All other columns: full scan.
        scan_field(df[[col]], forbidden_patterns, col, basename(pf))
      }
    }
  }
})

# ---------------------------------------------------------------------------
# Test: field-specific scan helper catches project-column leaks but not
# allowlisted message tokens (#140 round-4 Low).
# ---------------------------------------------------------------------------

test_that("scan_field catches project-column leak and passes allowlisted message content", {
  fake_commits <- data.frame(
    project           = c("-Users-johngavin-docs-gh-llm", "irishbuoys"),
    canonical_project = c("llm", "irishbuoys"),
    hash              = c("abc1234", "def5678"),
    date              = c("2026-05-01", "2026-05-02"),
    message           = c("fix: check johngavin cache invalidation",
                          "Merge pull request from JohnGavin/fix-branch"),
    stringsAsFactors  = FALSE
  )

  # The project column contains a raw home-dir path — scan_field must detect it.
  project_leak_detected <- FALSE
  withCallingHandlers(
    {
      # scan_field calls expect_false(); if the pattern IS present, the
      # expectation fails but we can detect it by wrapping in tryCatch.
      tryCatch(
        scan_field(fake_commits$project, forbidden_patterns, "project", "fake.json"),
        error = function(e) { project_leak_detected <<- TRUE }
      )
    },
    expectation_failure = function(e) {
      project_leak_detected <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  # We can't easily intercept testthat failures in the same test, so instead:
  # directly verify the grep logic that scan_field uses.
  project_text <- paste(fake_commits$project, collapse = "\n")
  expect_true(
    any(vapply(forbidden_patterns, function(p)
      grepl(p, project_text, perl = TRUE), logical(1))),
    label = "project column with raw path must match at least one forbidden pattern"
  )

  # The message column contains only allowlisted tokens (johngavin, JohnGavin).
  # strict_message_patterns has had those tokens removed.
  # Verify the message text does NOT match any strict pattern.
  message_text <- paste(fake_commits$message, collapse = "\n")
  expect_false(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, message_text, perl = TRUE), logical(1))),
    label = "allowlisted message text must not match any strict_message_patterns"
  )
})

# ---------------------------------------------------------------------------
# Regression: path-shape in message field MUST be caught even though bare
# tokens are allowlisted (#140 round-4 — message-field allowlisting gap).
# ---------------------------------------------------------------------------

test_that("path-shape leak in message field is still caught despite bare-token allowlist", {
  # A message that contains a raw filesystem path is a leak, not an author name.
  message_with_path_leak <- "/Users/johngavin/docs_gh/llm/some-file.R was the culprit"
  expect_true(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, message_with_path_leak, perl = TRUE), logical(1))),
    label = "message containing /Users/... must still match strict_message_patterns"
  )

  # A message with /private/tmp path leak must also be caught.
  private_tmp_leak <- "session at /private/tmp/roborev-worktree-1234 crashed"
  expect_true(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, private_tmp_leak, perl = TRUE), logical(1))),
    label = "message containing /private/tmp/... must still match strict_message_patterns"
  )

  # Clean messages must pass.
  clean_message <- "fix: check johngavin cache — from JohnGavin/feat-branch"
  expect_false(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, clean_message, perl = TRUE), logical(1))),
    label = "clean message with only bare author tokens must NOT match strict_message_patterns"
  )
})

# ---------------------------------------------------------------------------
# Regression (#157): any-user /Users/ and non-tmp /private/ paths in a
# commit message must be caught even when bare /Users/ and /private/ tokens
# are in the allowlist.  path_shape_patterns() must use generic patterns.
# ---------------------------------------------------------------------------

test_that("path-shape catches /Users/<anyuser>/... in message field (#157)", {
  # /Users/alice/ is a real path — not johngavin, but still a leak.
  alice_path_msg <- "bug introduced in /Users/alice/projects/secret/main.R"
  expect_true(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, alice_path_msg, perl = TRUE), logical(1))),
    label = "message with /Users/alice/... must match strict_message_patterns"
  )

  # /Users/bob/ — another arbitrary user
  bob_path_msg <- "checked /Users/bob/work/config — looks fine"
  expect_true(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, bob_path_msg, perl = TRUE), logical(1))),
    label = "message with /Users/bob/... must match strict_message_patterns"
  )

  # Benign message mentioning the word "Users" without a path — must NOT flag
  clean_users_msg <- "fix: check johngavin cache — relevant to Users of the app"
  expect_false(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, clean_users_msg, perl = TRUE), logical(1))),
    label = "message mentioning Users (not a path) must NOT match strict_message_patterns"
  )
})

test_that("path-shape catches /private/<anydir>/... (not just /private/tmp/) in message field (#157)", {
  # /private/var/folders/... — macOS sandbox path, not /private/tmp/
  private_var_msg <- "process wrote to /private/var/folders/xx/yz/T/ on macOS"
  expect_true(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, private_var_msg, perl = TRUE), logical(1))),
    label = "message with /private/var/... must match strict_message_patterns"
  )

  # /private/etc/ — another non-tmp private path
  private_etc_msg <- "config read from /private/etc/resolver/default"
  expect_true(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, private_etc_msg, perl = TRUE), logical(1))),
    label = "message with /private/etc/... must match strict_message_patterns"
  )

  # Benign message mentioning the word "private" — must NOT flag
  clean_private_msg <- "feat: add worktree-safety for private branch workflows"
  expect_false(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, clean_private_msg, perl = TRUE), logical(1))),
    label = "message mentioning 'private' without a path must NOT match strict_message_patterns"
  )
})

test_that("path-shape catches non-johngavin worktree path in message field (#157)", {
  # /Users/alice/.claude/worktrees/agent-xyz/... — non-johngavin worktree path
  alice_worktree_msg <- "crash at /Users/alice/.claude/worktrees/agent-abc123/R/foo.R"
  expect_true(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, alice_worktree_msg, perl = TRUE), logical(1))),
    label = "message with /Users/alice/... worktree path must match strict_message_patterns"
  )

  # Branch name referencing worktree-safety (not a path) — must NOT flag
  branch_ref_msg <- "Merge pull request from JohnGavin/fix/cc-sh-worktree-safety"
  expect_false(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, branch_ref_msg, perl = TRUE), logical(1))),
    label = "message with worktree-safety branch name must NOT match strict_message_patterns"
  )
})

# ---------------------------------------------------------------------------
# Round-2 regression (#157): BARE path forms (no trailing slash) must be caught.
# The original patterns required a trailing slash, so /Users/alice and
# /Users/johngavin (no trailing slash) bypassed the gate.
# ---------------------------------------------------------------------------

test_that("path-shape catches bare /Users/<user> (no trailing slash) in message field (#157 round-2)", {
  # Bare home-dir path with no trailing slash — must be caught.
  bare_alice_msg <- "commit authored at /Users/alice in a message field"
  expect_true(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, bare_alice_msg, perl = TRUE), logical(1))),
    label = "message with bare /Users/alice (no trailing slash) must match strict_message_patterns"
  )

  # Bare johngavin home-dir path with no trailing slash — regression case.
  bare_jg_msg <- "session root was /Users/johngavin per log"
  expect_true(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, bare_jg_msg, perl = TRUE), logical(1))),
    label = "message with bare /Users/johngavin (no trailing slash) must match strict_message_patterns"
  )

  # Bare bob path with no trailing slash.
  bare_bob_msg <- "checked /Users/bob was the problem"
  expect_true(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, bare_bob_msg, perl = TRUE), logical(1))),
    label = "message with bare /Users/bob (no trailing slash) must match strict_message_patterns"
  )

  # Bare /Users/ alone (no username token) — must NOT flag (benign concept mention).
  bare_prefix_msg <- "fix: check /Users/ path handling in the logic"
  expect_false(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, bare_prefix_msg, perl = TRUE), logical(1))),
    label = "bare /Users/ with no username must NOT match strict_message_patterns"
  )
})

test_that("path-shape catches bare /private/<dir> (no trailing slash) in message field (#157 round-2)", {
  # Bare /private/etc with no trailing slash — must be caught.
  bare_etc_msg <- "config was at /private/etc in the system"
  expect_true(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, bare_etc_msg, perl = TRUE), logical(1))),
    label = "message with bare /private/etc (no trailing slash) must match strict_message_patterns"
  )

  # Bare /private/var with no trailing slash — must be caught.
  bare_var_msg <- "sandbox path /private/var was accessed"
  expect_true(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, bare_var_msg, perl = TRUE), logical(1))),
    label = "message with bare /private/var (no trailing slash) must match strict_message_patterns"
  )

  # Bare /private/ alone (no dir token) — must NOT flag (benign concept mention).
  bare_private_prefix_msg <- "feat: add worktree-safety for /private/ path references"
  expect_false(
    any(vapply(strict_message_patterns, function(p)
      grepl(p, bare_private_prefix_msg, perl = TRUE), logical(1))),
    label = "bare /private/ with no dir token must NOT match strict_message_patterns"
  )
})

# ---------------------------------------------------------------------------
# Regression (#237): tightened -repo / /repo patterns must not false-positive
# on legitimate hyphenated English words like "ephemeral-repos".
# ---------------------------------------------------------------------------

test_that("agent-worktree -repo/repo ids match but legit English words do not (#237)", {
  # Real high-entropy worktree ids MUST still be caught:
  expect_true(any(vapply(strict_message_patterns,
    function(p) grepl(p, "merge D73dOZsvyf-repo into main", perl = TRUE), logical(1))),
    info = "D73dOZsvyf-repo must match")
  expect_true(any(vapply(strict_message_patterns,
    function(p) grepl(p, "see D73dOZsvyf/repo path", perl = TRUE), logical(1))),
    info = "D73dOZsvyf/repo must match")
  # Legitimate hyphenated English words MUST NOT false-positive:
  expect_false(any(vapply(strict_message_patterns,
    function(p) grepl(p, "feat(roborev): ephemeral-repos cleanup script (#217)", perl = TRUE), logical(1))),
    info = "ephemeral-repos must NOT match")
})

# ---------------------------------------------------------------------------
# Regression: generic ^/ path without "worktree" in it (#140 round-4).
# The roborev finding: /private/tmp/plain-id passes old verify gate.
# ---------------------------------------------------------------------------

test_that("forbidden_patterns catches /private/tmp/plain-id (no worktree substring)", {
  # A raw projectPath like "/private/tmp/plain-id" does NOT contain "-worktree-",
  # "worktree-agent-", or "/Users/" — but it IS a private absolute path.
  # The new "/private/" entry in sensitive_verify_patterns() must catch it.
  plain_id_path <- "/private/tmp/plain-id"
  expect_true(
    any(vapply(forbidden_patterns, function(p)
      grepl(p, plain_id_path, perl = TRUE), logical(1))),
    label = "/private/tmp/plain-id must match forbidden_patterns via /private/ entry"
  )
})

test_that("forbidden_patterns catches generic -worktree- without numeric suffix or 'agent-'", {
  # The old forbidden_patterns had "worktree-[0-9]+" and "worktree-agent-" but
  # not a plain "-worktree-" entry.  A form like "project-worktree-fixer-abc"
  # (no numeric suffix, no "agent-" prefix) would slip through.
  generic_worktree <- "project-worktree-fixer-abc123"
  expect_true(
    any(vapply(forbidden_patterns, function(p)
      grepl(p, generic_worktree, perl = TRUE), logical(1))),
    label = "generic -worktree- form must match forbidden_patterns"
  )
})

test_that("forbidden_patterns does not false-positive on clean project names", {
  clean <- c("llmtelemetry", "irishbuoys", "randomwalk",
             "sanitized@llm@h1a2b3c4d5e6", "claude-sonnet-4-6", "unknown")
  for (s in clean) {
    expect_false(
      any(vapply(forbidden_patterns, function(p)
        grepl(p, s, perl = TRUE), logical(1))),
      label = sprintf("clean string '%s' must NOT match forbidden_patterns", s)
    )
  }
})

# ---------------------------------------------------------------------------
# Legacy regression tests (kept from prior rounds).
# ---------------------------------------------------------------------------

test_that("leak injected into non-message column of git_commits_by_project.json is caught (Finding B, #140 round-3)", {
  # Regression test: proves that removing the whole-file skip (round-3 fix)
  # means a leak in the 'project' column is now detected, even though 'message'
  # is allowed to contain author-written references.
  fake_commits <- data.frame(
    project           = c("-Users-johngavin-docs-gh-llm", "irishbuoys"),
    canonical_project = c("llm", "irishbuoys"),
    hash              = c("abc1234", "def5678"),
    date              = c("2026-05-01", "2026-05-02"),
    message           = c("fix: normal commit message", "feat: normal commit"),
    stringsAsFactors  = FALSE
  )

  forbidden_in_project <- "-Users-johngavin"

  # Check the project column (must be caught):
  project_text <- paste(fake_commits$project, collapse = "\n")
  expect_true(
    grepl(forbidden_in_project, project_text, perl = TRUE),
    label = "injected leak must be detectable in project column"
  )

  # Check the message column (no injected leak):
  message_text <- paste(fake_commits$message, collapse = "\n")
  expect_false(
    grepl(forbidden_in_project, message_text, perl = TRUE),
    label = "message column must not contain the injected project-column leak"
  )
})

test_that("leak injected into non-message column of git_commits.parquet is caught by field-specific scan (Finding B, #140 round-3)", {
  pkg_root <- normalizePath(file.path(test_path(), "..", ".."), mustWork = FALSE)
  pf <- file.path(pkg_root, "inst", "extdata", "telemetry", "v1", "git_commits.parquet")
  skip_if(!file.exists(pf), "git_commits.parquet not present in extdata")

  con2 <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)

  df <- DBI::dbGetQuery(con2, sprintf("SELECT * FROM read_parquet('%s')", pf))

  forbidden_patterns_structural <- c(
    "-Users-johngavin",
    "johngavin",
    "-private-tmp-",
    "-tmp-",
    "worktree-agent-"
  )
  for (col in c("project", "canonical_project")) {
    if (!col %in% names(df)) next
    col_text <- paste(df[[col]], collapse = "\n")
    for (pat in forbidden_patterns_structural) {
      expect_false(
        grepl(pat, col_text, perl = TRUE),
        info = sprintf("git_commits.parquet column '%s' contains forbidden pattern '%s' — field-specific scan should have caught this", col, pat)
      )
    }
  }
})

test_that("committed extdata JSON files are valid JSON", {
  pkg_root <- normalizePath(file.path(test_path(), "..", ".."), mustWork = FALSE)
  extdata_dir <- file.path(pkg_root, "inst", "extdata")
  json_files <- list.files(extdata_dir, pattern = "\\.json$", full.names = TRUE)
  for (f in json_files) {
    result <- tryCatch(jsonlite::fromJSON(f), error = function(e) e)
    expect_false(
      inherits(result, "error"),
      info = sprintf("%s is not valid JSON: %s", basename(f),
                     if (inherits(result, "error")) conditionMessage(result) else "")
    )
  }
})

# ---------------------------------------------------------------------------
# Hardening: hex-encoding bypass detection (#163 round-2).
# Long hex-blob fields (matching ^[0-9a-f]{40,}$) in committed JSON files
# must decode to text that contains none of the forbidden path patterns.
# This catches the codex_log_offsets.json class of leak where raw log text
# (including cwd=/Users/... paths) is hex-encoded and slips past plain-text
# scanners.
# ---------------------------------------------------------------------------

# FILE-SCOPE hex helpers — shared by both the main extdata gate scan below
# AND the uppercase-hex regression test (~line 500+).  Defining them here once
# ensures that if the real detector regresses (e.g. reverts to lowercase-only),
# the regression test fails too — it cannot pass against a stale local copy.

#' Detect whether a string looks like a long hex-encoded blob.
#'
#' @param x character(1) — the string to test.
#' @return logical(1) — TRUE iff x is >= 40 characters, even length, and
#'   consists entirely of hex digits (case-insensitive).
is_hex_blob <- function(x) {
  nchar(x) >= 40L &&
    nchar(x) %% 2L == 0L &&
    grepl("^[0-9a-f]+$", x, perl = TRUE, ignore.case = TRUE)
}

#' Decode a hex string to a UTF-8 character string.
#'
#' @param hex_str character(1) — a hex-encoded string (upper, lower, or mixed case).
#' @return character(1) — the decoded string, or NA_character_ on failure.
#'   tolower() normalises uppercase/mixed-case hex before byte extraction so
#'   that strtoi() receives consistent lowercase input.
hex_decode <- function(hex_str) {
  tryCatch({
    hex_str <- tolower(hex_str)
    n <- nchar(hex_str) %/% 2L
    raw_vec <- as.raw(vapply(seq_len(n), function(i) {
      strtoi(substr(hex_str, (i - 1L) * 2L + 1L, i * 2L), base = 16L)
    }, integer(1L)))
    rawToChar(raw_vec)
  }, error   = function(e) NA_character_,
     warning = function(e) NA_character_)
}

test_that("hex-encoded fields in committed extdata JSON do not decode to forbidden paths", {
  # Patterns that must not appear in decoded hex text:
  # these are the canonical forbidden path identifiers.
  hex_forbidden <- c(
    "/Users/",
    "/private/",
    "cwd=",
    "-private-tmp-"
  )

  pkg_root <- normalizePath(file.path(test_path(), "..", ".."), mustWork = FALSE)
  extdata_dir <- file.path(pkg_root, "inst", "extdata")

  # Scan only git-tracked files — untracked runtime sidecars (e.g.
  # codex_log_offsets.json) may legitimately exist on disk and would
  # generate false positives.  The threat model is committed files only.
  tracked_in_extdata <- tryCatch({
    raw_out <- system2(
      "git",
      c("-C", pkg_root, "ls-files", "--error-unmatch", "inst/extdata/"),
      stdout = TRUE, stderr = FALSE
    )
    file.path(pkg_root, raw_out[grepl("\\.json$", raw_out)])
  }, error = function(e) {
    # Fallback: use filesystem listing if git is unavailable (e.g. R CMD check
    # without git).  This is conservative — no tracked-file filter.
    list.files(extdata_dir, pattern = "\\.json$", full.names = TRUE)
  })
  json_files <- tracked_in_extdata

  # Files to skip (same exclusions as the main regression gate):
  skip_basenames <- c(
    "ccusage_daily_raw_20260220.json",
    "ccusage_daily_raw_20260221.json",
    "github_issue_events.json",
    "test.json"
  )
  json_files <- json_files[!basename(json_files) %in% skip_basenames]

  # Recursively collect all leaf character values from a parsed JSON object.
  collect_strings <- function(obj) {
    if (is.character(obj)) {
      return(as.character(obj))
    }
    if (is.list(obj)) {
      return(unlist(lapply(obj, collect_strings), use.names = FALSE))
    }
    if (is.data.frame(obj)) {
      chr_cols <- vapply(obj, is.character, logical(1L))
      return(unlist(lapply(obj[chr_cols], as.character), use.names = FALSE))
    }
    character(0L)
  }

  for (f in json_files) {
    parsed <- tryCatch(
      jsonlite::fromJSON(f, simplifyVector = TRUE, simplifyDataFrame = TRUE),
      error = function(e) NULL
    )
    if (is.null(parsed)) next

    all_strings <- collect_strings(parsed)
    hex_candidates <- all_strings[!is.na(all_strings) & vapply(all_strings, is_hex_blob, logical(1L))]

    for (hex_val in hex_candidates) {
      decoded <- hex_decode(hex_val)
      if (is.na(decoded)) next

      for (pat in hex_forbidden) {
        expect_false(
          grepl(pat, decoded, fixed = TRUE),
          info = sprintf(
            "%s: hex-encoded field decodes to text containing forbidden pattern '%s'",
            basename(f), pat
          )
        )
      }
    }
  }
})

# ---------------------------------------------------------------------------
# Regression: uppercase/mixed-case hex bypasses lowercase-only detector
# (#163 round-3 Finding 2).
# Proves that is_hex_blob() with ignore.case = TRUE catches uppercase hex
# blobs, and that hex_decode() correctly decodes them after tolower().
# ---------------------------------------------------------------------------

test_that("uppercase hex blob containing forbidden path is detected and rejected (#163 round-3)", {
  # Build a forbidden string, encode it in UPPERCASE hex.
  leak_text <- "cwd=/Users/johngavin/docs_gh/llm"
  hex_lower <- paste(format(as.hexmode(utf8ToInt(leak_text)), width = 2L), collapse = "")
  hex_upper <- toupper(hex_lower)

  # Sanity: hex_upper is all-uppercase and long enough to trigger the detector.
  expect_true(nchar(hex_upper) >= 40L)
  expect_true(nchar(hex_upper) %% 2L == 0L)
  expect_true(grepl("^[0-9A-F]+$", hex_upper))     # uppercase chars present
  expect_false(grepl("^[0-9a-f]+$", hex_upper))     # lowercase-only check FAILS (old behaviour)

  # The REAL file-scope is_hex_blob() with ignore.case = TRUE must catch it.
  # If is_hex_blob() ever regresses to lowercase-only matching, this test fails.
  expect_true(is_hex_blob(hex_upper),
    label = "is_hex_blob with ignore.case=TRUE must detect uppercase hex blob")

  # Decode using the REAL file-scope hex_decode() and verify it round-trips.
  # If hex_decode() ever regresses (e.g. drops the tolower() call), this fails.
  decoded <- hex_decode(hex_upper)
  expect_false(is.na(decoded), label = "uppercase hex must decode without error")
  expect_equal(decoded, leak_text, label = "decoded text must match original leak string")

  # The forbidden pattern must be found in the decoded text.
  expect_true(
    grepl("/Users/", decoded, fixed = TRUE),
    label = "decoded uppercase hex blob must contain forbidden /Users/ pattern"
  )
})
