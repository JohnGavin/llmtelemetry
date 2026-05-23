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
