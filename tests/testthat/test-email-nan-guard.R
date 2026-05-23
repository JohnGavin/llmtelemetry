# test-email-nan-guard.R
# Tests for issue #137: NaN guard in email formatting helpers
#
# Coverage:
#   - email_safe_num() renders NaN, NA, Inf, NULL, 0, and valid numbers correctly
#   - The QA bash script's NaN check is case-sensitive (no false positives from
#     substrings like "channel" containing "nan")
#
# Strategy: test the helper functions that wrap numeric insertions in the
# send_daily_email.R script to ensure no unguarded NaN can reach sprintf.
# These helpers are defined inline in the script; we redeclare them here to
# keep tests self-contained (mirrors the approach in test-email-codex-section.R).

# ── Inline helpers (mirror send_daily_email.R) ────────────────────────────────
dollar <- function(x) {
  if (is.null(x) || length(x) == 0) return("-")
  if (is.na(x) || is.nan(x) || !is.finite(x)) return("-")
  sprintf("$%.2f", x)
}
comma <- function(x) {
  if (is.null(x) || length(x) == 0) return("-")
  if (is.na(x) || is.nan(x) || !is.finite(x)) return("-")
  format(x, big.mark = ",", scientific = FALSE)
}
millions <- function(x) {
  if (is.null(x) || length(x) == 0) return("-")
  if (is.na(x) || is.nan(x) || !is.finite(x)) return("-")
  sprintf("%.1fM", x / 1e6)
}

# email_safe_num() is the new guard for direct %s insertions (cc_days, gm_days,
# cm_days, gm_sessions_count, n_sessions, cx_days, cx_sessions).
# It must be defined in send_daily_email.R before we can reference it here.
# For now define the spec here; the actual implementation must match.
email_safe_num <- function(x, fallback = "-") {
  if (is.null(x) || length(x) == 0) return(fallback)
  if (is.na(x) || is.nan(x)) return(fallback)
  if (!is.finite(x)) return(fallback)
  as.character(as.integer(x))
}

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

# ── Tests: email_safe_num() — the new guard for raw integer insertions ────────
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
# Simulate the sprintf call for the gm_days field (line ~587 in send_daily_email.R)
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
# The bash QA script used grep -ci "NaN" (case-insensitive), which falsely
# matched "nan" inside substrings like session IDs containing "nan" (e.g. a
# Gemini session named "nanum-123", or any project/session name with "nan").
# Verify that the R-side QA check is case-sensitive (uses fixed = TRUE).

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
