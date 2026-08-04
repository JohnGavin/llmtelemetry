# Property test (#340): sensitive_verify_patterns() (verify) must be a
# TRUE SUBSET of sensitive_id_pattern() (sanitize) -- containment, not just
# "both functions mention the same class name".  See the class table and
# INVARIANT note in R/sensitive_patterns.R for the full rationale.
#
# #340 root cause: sensitive_id_pattern() anchored the tmp classes to
# string-start (^-tmp-, ^-private-tmp-) while sensitive_verify_patterns()
# used the identical substrings unanchored (-tmp-, -private-tmp-).  A
# dashed macOS per-user temp id has its "-tmp-" token mid-string (after
# "-private-var-folders-...-T-"), so it matched verify but never matched
# sanitize -- the fail-closed verification gate (#131) refused to publish
# on every run, forever.  A mid-string probe is exactly what would have
# caught this before it shipped; this file makes that probe permanent.

# ---------------------------------------------------------------------------
# Patterns exempted from mid-string containment testing.
# ---------------------------------------------------------------------------
# Two verify-side entries correspond to sanitize classes that are
# DELIBERATELY whole-value / string-start anchored, by design (see the
# roxygen for sensitive_id_pattern()):
#
#   - "Users-johngavin" -> sanitize's `^-Users-` (home-dir prefix class):
#     a dashed ID derived from an absolute path can only legitimately have
#     "-Users-" as its OWN prefix (macOS home directories are rooted at
#     /Users/, so a home-dir-derived id always starts there).
#   - "/Users/", "/private/", "/tmp/", "/var/" -> sanitize's `^/` (absolute
#     path class): this class means "the entire ID value IS a raw absolute
#     path", not an embeddable substring.  Unanchoring `^/` itself is not
#     viable -- it would match any string containing a literal "/" anywhere
#     (URLs, "owner/repo" identifiers, hierarchical project names), which
#     is a completely different, much noisier detection job already handled
#     separately by path_shape_patterns() for free-text fields (commit
#     messages etc.), not by sensitive_id_pattern().
#
# Every OTHER verify substring represents a dashed-ID *segment* that can
# legitimately appear anywhere within an already-dashed identifier -- this
# is exactly the shape of the #340 bug -- and MUST satisfy containment at
# every position.
anchored_by_design <- c(
  "Users-johngavin",
  "/Users/",
  "/private/",
  "/tmp/",
  "/var/"
)

unanchored_verify_patterns <- setdiff(sensitive_verify_patterns(), anchored_by_design)

test_that("every unanchored-by-design verify pattern is matched by sensitive_id_pattern() regardless of position (#340)", {
  sanitize_re <- sensitive_id_pattern()

  # Confirm the exemption list itself has not silently grown stale (e.g. if
  # sensitive_verify_patterns() drops one of the anchored-by-design entries,
  # setdiff() above would just quietly not test it -- catch that here).
  expect_true(
    all(anchored_by_design %in% sensitive_verify_patterns()),
    label = "anchored_by_design entries must still exist in sensitive_verify_patterns()"
  )
  expect_true(
    length(unanchored_verify_patterns) > 0L,
    label = "at least one verify pattern must remain subject to the containment probe"
  )

  for (pat in unanchored_verify_patterns) {
    candidates <- c(
      start  = paste0(pat, "abc123"),
      middle = paste0("prefix-segment", pat, "suffix-segment"),
      end    = paste0("prefix-segment-", pat)
    )

    for (position in names(candidates)) {
      s <- candidates[[position]]

      # Sanity: the candidate must actually exercise the verify pattern at
      # the intended position (a broken test fixture would otherwise pass
      # vacuously).
      expect_true(
        grepl(pat, s, fixed = TRUE),
        label = sprintf(
          "test fixture bug: candidate '%s' does not contain verify pattern '%s' at position '%s'",
          s, pat, position
        )
      )

      expect_true(
        grepl(sanitize_re, s, perl = TRUE),
        info = sprintf(
          paste(
            "containment invariant violated (#340): verify pattern '%s' matches",
            "'%s' (%s of string) but sensitive_id_pattern() does not -- this is",
            "the exact shape of bug #340 (sanitize anchored, verify not)."
          ),
          pat, s, position
        )
      )
    }
  }
})

test_that("sensitive_id_pattern() matches the historical #340 offending value directly", {
  # Belt-and-suspenders: the exact real-world value from the incident, not a
  # synthetic construction.  See test-no-path-leak.R and
  # test-sanitize-ccusage-all.R for the forbidden_patterns / end-to-end
  # sanitization companions to this check.
  offending_id <- "-private-var-folders-hn-3nfdjww12237gp6y33bpnr7w0000gn-T-tmp-msqYsMoZTd"
  expect_true(
    grepl(sensitive_id_pattern(), offending_id, perl = TRUE),
    label = "sensitive_id_pattern() must match the real #340 offending id"
  )
})
