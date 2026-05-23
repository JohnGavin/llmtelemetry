# test-email-nan-guard.R
# Tests for issue #137: NaN guard in email formatting helpers
#
# Coverage:
#   - email_safe_num() renders NaN, NA, Inf, NULL, 0, and valid numbers correctly
#   - dollar(), comma(), millions() guard non-finite inputs
#   - R-side QA check is case-sensitive (no false positives from substrings like
#     "channel" or "nanum" containing "nan")
#   - The qa_email_output.sh bash script uses case-sensitive NaN matching
#     (grep without -i): "nan"-substring inputs do NOT trip the gate but a real
#     "NaN" does
#
# Strategy: source the production helpers from R/email_helpers.R so tests
# exercise the REAL implementation.  A regression in email_helpers.R will now
# cause these tests to fail (previously they would pass even with a broken
# production file because helpers were redeclared locally).

# ── Source production helpers ─────────────────────────────────────────────────
# Locate R/email_helpers.R relative to the package root.
# testthat runs with cwd = package root (via devtools::test()) or
# tests/testthat/ (via Rscript).  We try both.
.helpers_candidates <- c(
  file.path(getwd(), "R", "email_helpers.R"),             # cwd = pkg root
  file.path(getwd(), "..", "..", "R", "email_helpers.R")  # cwd = tests/testthat
)
.found <- Filter(file.exists, .helpers_candidates)
if (length(.found) == 0L) {
  stop("Could not locate R/email_helpers.R — tests cannot exercise production code")
}
source(.found[[1L]])

# ── Tests: dollar() ───────────────────────────────────────────────────────────
test_that("dollar() returns '-' for NaN", {
  expect_equal(dollar(NaN), "-")
})

test_that("dollar() returns '-' for NA", {
  expect_equal(dollar(NA), "-")
})

test_that("dollar() returns '-' for NULL", {
  expect_equal(dollar(NULL), "-")
})

test_that("dollar() returns '-' for Inf", {
  expect_equal(dollar(Inf), "-")
})

test_that("dollar() formats zero correctly", {
  expect_equal(dollar(0), "$0.00")
})

test_that("dollar() formats positive value correctly", {
  expect_equal(dollar(1.5), "$1.50")
})

# ── Tests: comma() ────────────────────────────────────────────────────────────
test_that("comma() returns '-' for NaN", {
  expect_equal(comma(NaN), "-")
})

test_that("comma() returns '-' for NA", {
  expect_equal(comma(NA), "-")
})

test_that("comma() returns '-' for NULL", {
  expect_equal(comma(NULL), "-")
})

test_that("comma() formats zero", {
  expect_equal(comma(0), "0")
})

# ── Tests: millions() ─────────────────────────────────────────────────────────
test_that("millions() returns '-' for NaN", {
  expect_equal(millions(NaN), "-")
})

test_that("millions() returns '-' for NA", {
  expect_equal(millions(NA), "-")
})

test_that("millions() returns '-' for NULL", {
  expect_equal(millions(NULL), "-")
})

# ── Tests: email_safe_num() — the guard for raw integer insertions ─────────
test_that("email_safe_num() returns '-' for NaN (regression #137)", {
  expect_equal(email_safe_num(NaN), "-")
})

test_that("email_safe_num() returns '-' for NA", {
  expect_equal(email_safe_num(NA), "-")
})

test_that("email_safe_num() returns '-' for Inf", {
  expect_equal(email_safe_num(Inf), "-")
})

test_that("email_safe_num() returns '-' for NULL", {
  expect_equal(email_safe_num(NULL), "-")
})

test_that("email_safe_num() returns '-' for empty vector", {
  expect_equal(email_safe_num(numeric(0)), "-")
})

test_that("email_safe_num() returns '0' for zero", {
  expect_equal(email_safe_num(0), "0")
})

test_that("email_safe_num() returns '5' for 5", {
  expect_equal(email_safe_num(5), "5")
})

test_that("email_safe_num() returns '5' for 5.9 (truncates to int)", {
  expect_equal(email_safe_num(5.9), "5")
})

test_that("email_safe_num() uses custom fallback", {
  expect_equal(email_safe_num(NaN, fallback = "n/a"), "n/a")
  expect_equal(email_safe_num(NA, fallback = "0"), "0")
})

# ── Tests: NaN-in-HTML regression ────────────────────────────────────────────
# Simulate the sprintf call for the gm_days field
# BEFORE the fix: gm_days = NaN would produce "NaN" in HTML.
# AFTER the fix:  email_safe_num(gm_days) produces "-".

test_that("gm_days = NaN produces 'NaN' without guard (demonstrates the bug)", {
  gm_days <- NaN
  # Simulate what the old code does: passes gm_days directly to sprintf
  html_fragment <- sprintf("<td>%s</td>", gm_days)
  expect_match(html_fragment, "NaN")
})

test_that("gm_days = NaN produces '-' with email_safe_num guard (demonstrates the fix)", {
  gm_days <- NaN
  html_fragment <- sprintf("<td>%s</td>", email_safe_num(gm_days))
  expect_false(grepl("NaN", html_fragment, fixed = TRUE))
  expect_match(html_fragment, "-")
})

test_that("gm_days = NA produces '-' with email_safe_num guard", {
  gm_days <- NA
  html_fragment <- sprintf("<td>%s</td>", email_safe_num(gm_days))
  expect_false(grepl("NaN", html_fragment, fixed = TRUE))
  expect_match(html_fragment, "-")
})

# ── Tests: QA script false-positive (case-sensitivity) ───────────────────────
# The bash QA script previously used grep -ci "NaN" (case-insensitive), which
# falsely matched "nan" inside substrings like session IDs containing "nan"
# (e.g. "nanum-123", or any project/session name with "nan").
# Verify the R-side QA check is case-sensitive (fixed = TRUE).

test_that("R QA check does not flag 'nanum' session ID as NaN (case-sensitive)", {
  html <- "<td>nanum-session-abc123</td>"
  # R-side check with fixed = TRUE (correct): "nan" != "NaN"
  expect_false(grepl("NaN", html, fixed = TRUE))
})

test_that("R QA check does not flag 'finance' project as NaN (case-sensitive)", {
  html <- "<td>finance-tracker</td>"
  expect_false(grepl("NaN", html, fixed = TRUE))
})

test_that("R QA check catches actual 'NaN' correctly", {
  html <- sprintf("<td>%s</td>", NaN)  # produces "<td>NaN</td>"
  expect_true(grepl("NaN", html, fixed = TRUE))
})

test_that("bash -ci flag false-positives on 'nanum' session ID (documents the bug)", {
  # This test documents WHY grep -ci is wrong.
  # We simulate grep -ci behaviour with ignore.case = TRUE in R.
  html <- "<td>nanum-session-abc123</td>"
  bash_match <- grepl("NaN", html, ignore.case = TRUE)  # simulates grep -ci
  r_match    <- grepl("NaN", html, fixed = TRUE)          # correct R check
  # Bash false-positives on "nan" in "nanum"; R does not
  expect_true(bash_match)   # demonstrates the bug: bash -ci fires on "nanum"
  expect_false(r_match)     # R fixed=TRUE correctly ignores "nanum"
})

# ── Integration fixture: qa_email_output.sh uses case-sensitive NaN check ─────
# The script (inst/scripts/qa_email_output.sh) must NOT fire on lowercase "nan"
# substrings but MUST fire on literal "NaN".
# We verify the key grep lines from the script behave correctly without
# executing bash (to keep tests portable and fast).

test_that("qa_email_output.sh NaN grep: 'nanum' does not match (case-sensitive)", {
  # The script uses: grep -c "NaN"  (no -i flag)
  # Simulate in R: grepl without ignore.case
  html_line <- "<td>nanum-session-abc123</td>"
  # Case-sensitive grep -c "NaN" on this line returns 0 — no match
  expect_false(grepl("NaN", html_line))
})

test_that("qa_email_output.sh NaN grep: literal 'NaN' matches (case-sensitive)", {
  html_line <- "<td>NaN</td>"
  # Case-sensitive grep -c "NaN" on this line returns 1 — match
  expect_true(grepl("NaN", html_line))
})

test_that("qa_email_output.sh NaN grep: 'nan' (lowercase) does not match", {
  html_line <- "<td>nan</td>"
  # Case-sensitive grep -c "NaN" — "nan" != "NaN"
  expect_false(grepl("NaN", html_line))
})

test_that("qa_email_output.sh skips HTML comment lines for NaN check", {
  # The script pipes through: grep -v '<!--' | grep -c "NaN"
  # So a line like <!-- QA:models_found=NaN --> is excluded
  comment_line <- "<!-- QA:marker=NaN -->"
  non_comment  <- "<td>NaN</td>"
  # Comment lines are excluded by grep -v '<!--':
  expect_true(grepl("<!--", comment_line))   # would be filtered out
  expect_false(grepl("<!--", non_comment))   # this line IS checked for NaN
  expect_true(grepl("NaN", non_comment))
})

# ── Source-assertion: qa_email_output.sh NaN grep must NOT use -i flag ────────
# A regression back to `grep -ci "NaN"` (case-insensitive) would cause
# false positives on session IDs or project names containing "nan".
# This test reads the actual script and asserts the forbidden flag is absent
# from the NaN check section.  A real regression in the script will fail this
# test, even if all the R-simulation tests above still pass.
#
# Implementation note: the script passes patterns via a shell variable
# (`grep -c "$pat"`) rather than quoting NaN inline.  We therefore inspect the
# NaN-pattern loop header that declares `"NaN"` as one of the patterns, and the
# grep command(s) within that loop, and assert that neither contains the -i flag.

test_that("qa_email_output.sh NaN grep section does not use -i flag (source assertion)", {
  # Locate the script: system.file() is primary (works in installed/CHECK contexts);
  # getwd()-relative paths are fallbacks for devtools::test() (#155).
  script_candidates <- c(
    system.file("scripts", "qa_email_output.sh", package = "llmtelemetry"), # primary: installed
    file.path(getwd(), "inst", "scripts", "qa_email_output.sh"),             # cwd = pkg root
    file.path(getwd(), "..", "..", "inst", "scripts", "qa_email_output.sh")  # cwd = tests/testthat
  )
  script_path <- Filter(nzchar, Filter(file.exists, script_candidates))
  skip_if(length(script_path) == 0L, "qa_email_output.sh not found — skipping source assertion")
  script_path <- script_path[[1L]]

  script_lines <- readLines(script_path)

  # Find the loop header line that declares "NaN" as one of the patterns.
  # The script has: for pat in "NaN" ">NULL<" "NA_real_"; do
  nan_loop_idx <- grep('"NaN"', script_lines)
  expect_gt(length(nan_loop_idx), 0L,
            label = 'At least one line containing "NaN" must exist in the script')

  # The "NaN" declaration is in a for-loop header.  The grep call(s) that
  # perform the actual NaN matching follow within the same loop body (within
  # ~6 lines).  Look at the 8 lines after the first "NaN" occurrence and
  # collect any lines that contain `grep` (the command, not the word in a
  # comment).
  window_start <- nan_loop_idx[[1L]]
  window_end   <- min(window_start + 8L, length(script_lines))
  window_lines <- script_lines[window_start:window_end]
  # Match lines that contain a grep invocation (not just the word "grep" in a
  # comment): look for lines where "grep" appears outside a leading "#"
  non_comment <- window_lines[!grepl("^\\s*#", window_lines)]
  nan_grep_lines <- grep("\\bgrep\\b", non_comment, value = TRUE)

  expect_gt(length(nan_grep_lines), 0L,
            label = "At least one grep call must appear in the NaN pattern loop body")

  # None of the grep calls in this section should carry the -i flag.
  # Flag variants that indicate case-insensitive matching:
  #   -i, -ci, -ic, -Ei, -iE, -cia, grep -i, grep -ci "NaN", etc.
  # We check: does any grep invocation on the line have a flag cluster that
  # includes the letter "i"?
  # Pattern: `grep` followed by optional content then `-` then a flag string
  # that contains "i" before any space or quote.
  has_case_insensitive <- grepl("\\bgrep\\b[^|\\n]*\\s-[a-zA-Z]*i[a-zA-Z]*", nan_grep_lines)
  expect_false(any(has_case_insensitive),
               label = paste(
                 "grep call(s) in the NaN check section of qa_email_output.sh",
                 "must NOT use the -i (case-insensitive) flag.",
                 "Offending line(s):", paste(nan_grep_lines[has_case_insensitive], collapse = "; ")
               ))
})

# ── Functional: run actual qa_email_output.sh against tiny fixture files ──────
# These tests run the real bash script so a script regression (e.g. reverting
# to grep -ci) is caught by an actual non-zero exit code, not just source text.
# The fixtures include the required structural features the script checks for.

test_that("qa_email_output.sh exits 0 for valid HTML with 'nanum' (no false positive)", {
  skip_if(.Platform$OS.type != "unix", "bash tests require a Unix shell")
  script_candidates <- c(
    system.file("scripts", "qa_email_output.sh", package = "llmtelemetry"), # primary: installed
    file.path(getwd(), "inst", "scripts", "qa_email_output.sh"),
    file.path(getwd(), "..", "..", "inst", "scripts", "qa_email_output.sh")
  )
  script_path <- Filter(nzchar, Filter(file.exists, script_candidates))
  skip_if(length(script_path) == 0L, "qa_email_output.sh not found")
  script_path <- script_path[[1L]]

  # Minimal valid HTML that satisfies ALL checks in the script:
  #   Required features: "Summary" (as >Summary<), "font-weight: bold",
  #                      dashboard URL, at least one $N.NN cost
  #   Ordering: "Time Block Activity" before ">Summary<"
  #   QA markers: blocks_grouped_by_day, model_breakdown_days, models_found
  # "nanum-session-abc123" must NOT trigger the NaN gate (false-positive check).
  # NOTE: the ordering check uses `grep -n ">Summary<"` (angle brackets), so we
  # must render as ">Summary<" not just "Summary".
  fixture <- tempfile(fileext = ".html")
  on.exit(unlink(fixture), add = TRUE)
  # Helper: build a minimal but structurally complete HTML fixture.
  # The script uses `grep -n ">Summary<"` (angle brackets) for the ordering
  # check, so the fixture must contain that literal string.  It also uses
  # `grep -qi` for optional features ("Time Block Activity", "Daily Cost by
  # Model", "MTok", "blocks)") and `grep -qE '\$[0-9]+\.[0-9]{2}'` for costs.
  # Including all of them avoids false WARNs and prevents set -euo pipefail
  # crashes from grep returning 1 on missing patterns.
  writeLines(c(
    "<html><body>",
    "<style>.hdr { font-weight: bold; }</style>",
    "<h2>Time Block Activity (3 blocks)</h2>",
    "<h2>Daily Cost by Model</h2>",
    "<td>MTok used</td>",
    "<td>>Summary< section</td>",   # provides the literal ">Summary<" token
    "Summary",                       # satisfies grep -qi Summary
    "<a href='https://johngavin.github.io/llmtelemetry'>Dashboard</a>",
    "<td>$1.23</td>",
    "<td>nanum-session-abc123</td>",
    "<!-- QA:blocks_grouped_by_day=3 -->",
    "<!-- QA:model_breakdown_days=3 -->",
    "<!-- QA:models_found=claude-sonnet-4-6 -->"
  ), fixture)

  result <- system2("bash", args = c(script_path, fixture),
                    stdout = TRUE, stderr = TRUE)
  exit_code <- attr(result, "status")
  # system2 returns NULL status on success (exit 0)
  expect_true(is.null(exit_code) || exit_code == 0L,
              label = paste(
                "Script should exit 0 for HTML containing 'nanum' (not NaN).",
                "Output:", paste(result, collapse = "\n")
              ))
})

test_that("qa_email_output.sh exits 1 for HTML containing literal 'NaN'", {
  skip_if(.Platform$OS.type != "unix", "bash tests require a Unix shell")
  script_candidates <- c(
    system.file("scripts", "qa_email_output.sh", package = "llmtelemetry"), # primary: installed
    file.path(getwd(), "inst", "scripts", "qa_email_output.sh"),
    file.path(getwd(), "..", "..", "inst", "scripts", "qa_email_output.sh")
  )
  script_path <- Filter(nzchar, Filter(file.exists, script_candidates))
  skip_if(length(script_path) == 0L, "qa_email_output.sh not found")
  script_path <- script_path[[1L]]

  # Same structural HTML but with a literal "NaN" in a visible table cell.
  # The NaN gate must detect this and exit 1.
  fixture <- tempfile(fileext = ".html")
  on.exit(unlink(fixture), add = TRUE)
  writeLines(c(
    "<html><body>",
    "<style>.hdr { font-weight: bold; }</style>",
    "<h2>Time Block Activity (3 blocks)</h2>",
    "<h2>Daily Cost by Model</h2>",
    "<td>MTok used</td>",
    "<td>>Summary< section</td>",
    "Summary",
    "<a href='https://johngavin.github.io/llmtelemetry'>Dashboard</a>",
    "<td>$1.23</td>",
    "<td>NaN</td>",   # literal NaN — must trip the NaN gate
    "<!-- QA:blocks_grouped_by_day=3 -->",
    "<!-- QA:model_breakdown_days=3 -->",
    "<!-- QA:models_found=claude-sonnet-4-6 -->"
  ), fixture)

  # suppressWarnings: system2() emits a warning when the process exits non-zero
  # while stdout/stderr are captured; we expect exit 1, so silence the warning.
  result <- suppressWarnings(
    system2("bash", args = c(script_path, fixture),
            stdout = TRUE, stderr = TRUE)
  )
  exit_code <- attr(result, "status")
  expect_equal(exit_code, 1L,
               label = paste(
                 "Script should exit 1 when HTML contains literal 'NaN'.",
                 "Output:", paste(result, collapse = "\n")
               ))
})
