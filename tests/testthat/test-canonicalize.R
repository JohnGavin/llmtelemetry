# Tests for R/canonicalize.R internal helpers.
#
# Covers:
#   .shorten_project_local()           — path-prefix normalisation
#   .canonicalize_project_local()      — vectorised scalar wrapper
#   .canonicalize_project_local_scalar()
#
# Finding (c) in roborev round V: underscore-form nested paths were not
# converted and polluted canonical_project with tokens like "docs_gh_llm".

# Load canonicalize helpers: try source from development tree first (devtools
# test workflow), fall back to package namespace (R CMD check workflow).
local({
  r_file <- tryCatch(
    normalizePath(file.path(test_path(), "..", "..", "R", "canonicalize.R"),
                  mustWork = TRUE),
    error = function(e) ""
  )
  # Names assigned to .GlobalEnv by either branch; cleaned up via teardown_env.
  assigned_names <- character(0L)
  if (nzchar(r_file) && file.exists(r_file)) {
    source(r_file, local = FALSE)
    # source() assigns all top-level names; schedule removal of the known ones.
    assigned_names <- c(".shorten_project_local",
                        ".canonicalize_project_local",
                        ".canonicalize_project_local_scalar")
  } else {
    # In R CMD check, functions are available via the package namespace.
    assign(".shorten_project_local",
           llmtelemetry:::.shorten_project_local, envir = .GlobalEnv)
    assign(".canonicalize_project_local",
           llmtelemetry:::.canonicalize_project_local, envir = .GlobalEnv)
    assign(".canonicalize_project_local_scalar",
           llmtelemetry:::.canonicalize_project_local_scalar, envir = .GlobalEnv)
    assigned_names <- c(".shorten_project_local",
                        ".canonicalize_project_local",
                        ".canonicalize_project_local_scalar")
  }
  # Restore .GlobalEnv when the test file finishes.
  withr::defer(
    rm(list = intersect(assigned_names, ls(envir = .GlobalEnv, all.names = TRUE)),
       envir = .GlobalEnv),
    envir = testthat::teardown_env()
  )
})

# ── Dash-form (existing behaviour, must not regress) ──────────────────────────

test_that("dash-form hook path is normalised to project name", {
  expect_equal(.canonicalize_project_local("docs-gh-llmtelemetry"), "llmtelemetry")
  expect_equal(.canonicalize_project_local("docs-gh-llm"),          "llm")
  expect_equal(.canonicalize_project_local("docs-gh-mycare"),        "mycare")
})

test_that("full dash-form -Users- prefix is stripped", {
  expect_equal(
    .canonicalize_project_local("-Users-johngavin-docs-gh-llmtelemetry"),
    "llmtelemetry"
  )
})

# ── Slash-form (pre-converted input, must not regress) ────────────────────────

test_that("slash-form path returns first real segment", {
  # "docs/gh/llmtelemetry": "docs" is not a container prefix so first segment
  # "docs" is returned.  This is the existing behaviour; use known forms for
  # regression tests.
  expect_equal(.canonicalize_project_local("llmtelemetry/inst"),         "llmtelemetry")
  expect_equal(.canonicalize_project_local("simulations/randomwalk"),    "randomwalk")
})

# ── Underscore-form nested paths (finding c: new behaviour) ──────────────────

test_that("underscore-form docs_gh_ prefix is stripped to project name", {
  expect_equal(.canonicalize_project_local("docs_gh_llmtelemetry"), "llmtelemetry")
  expect_equal(.canonicalize_project_local("docs_gh_llm"),          "llm")
  expect_equal(.canonicalize_project_local("docs_gh_mycare"),        "mycare")
})

test_that("underscore-form -Users_ full prefix is stripped", {
  expect_equal(
    .canonicalize_project_local("-Users_johngavin_docs_gh_llmtelemetry"),
    "llmtelemetry"
  )
})

# ── Mixed dash-and-underscore separator forms (critic m9) ────────────────────

test_that("mixed dash-underscore forms docs-gh_<project> and docs_gh-<project> normalise correctly", {
  # "docs-gh_llmtelemetry": docs-gh_ prefix (dash then underscore)
  expect_equal(.canonicalize_project_local("docs-gh_llmtelemetry"), "llmtelemetry")
  # "docs_gh-llmtelemetry": docs_gh- prefix (underscore then dash)
  expect_equal(.canonicalize_project_local("docs_gh-llmtelemetry"), "llmtelemetry")
})

test_that("mixed dash-underscore snapshot", {
  inputs <- c("docs-gh_llmtelemetry", "docs_gh-llmtelemetry")
  expect_snapshot(.canonicalize_project_local(inputs))
})

# ── Bare underscore names must pass through UNCHANGED (meta_only guard) ───────

test_that("bare 'urban_planning' is KEPT as a real project", {
  # 2026-05-25: urban_planning is a real project — must NOT be dropped.
  # Underscore-to-slash conversion must not fire for bare single-token names
  # that are not path-like.
  expect_equal(.canonicalize_project_local("urban_planning"), "urban_planning")
})

test_that("bare 'irish_buoy_network' is not converted by underscore logic", {
  # This name is NOT in meta_only — it maps via the overrides list once it is
  # in slash form.  Bare underscore form must not reach the override checks
  # after incorrect conversion.  Since "irish_buoy_network" does NOT start with
  # a path-like prefix, .shorten_project_local() leaves it intact; the scalar
  # helper then returns the first segment "irish_buoy_network" which passes
  # the meta_only check and is returned as-is.
  result <- .canonicalize_project_local("irish_buoy_network")
  expect_false(is.na(result))
  expect_equal(result, "irish_buoy_network")
})

# ── Path-prefix + underscore meta_only names (critic m10) ────────────────────

test_that("path-prefixed docs_gh_urban_planning resolves to urban_planning (real project)", {
  # docs_gh_urban_planning: the ^docs[-_]gh[-_] prefix is stripped first,
  # yielding "urban_planning" which is a real project -> "urban_planning".
  expect_equal(.canonicalize_project_local("docs_gh_urban_planning"), "urban_planning")
})

test_that("path-prefixed docs_gh_telemetry resolves to llmtelemetry (remap)", {
  # docs_gh_telemetry: strips to "telemetry" which remaps -> "llmtelemetry".
  expect_equal(.canonicalize_project_local("docs_gh_telemetry"), "llmtelemetry")
})

# ── Snapshot tests ────────────────────────────────────────────────────────────

test_that("dash-form, slash-form, underscore-path, and bare-underscore snapshot", {
  inputs <- c(
    "docs-gh-llmtelemetry",                      # dash-form
    "llmtelemetry/inst",                         # slash-form (already normalised)
    "docs_gh_llmtelemetry",                      # underscore-path
    "-Users_johngavin_docs_gh_llm",              # underscore full prefix
    "urban_planning",                            # real project (not meta_only)
    "irish_buoy_network"                         # bare underscore name (not meta_only)
  )
  result <- .canonicalize_project_local(inputs)
  expect_snapshot(result)
})

test_that(".shorten_project_local snapshot — output for known forms is stable", {
  inputs <- c(
    "docs-gh-llmtelemetry",          # dash-form -> "llmtelemetry"
    "docs_gh_llmtelemetry",          # underscore-path -> "llmtelemetry"
    "-Users-johngavin-docs-gh-llm",  # dash full prefix -> "llm"
    "-Users_johngavin_docs_gh_llm",  # underscore full prefix -> "llm"
    "urban_planning"                 # bare underscore name -> unchanged
  )
  result <- vapply(inputs, .shorten_project_local, character(1L), USE.NAMES = FALSE)
  expect_snapshot(result)
})

# ── Branch/worktree-suffix stripping (fix: project-name-canonicalization) ─────
#
# Before this fix, "llm-feat-cc-20260521-092847" -> "feat" (wrong) because
# .shorten_project_local() stripped "llm-" first, leaving "feat/cc/...".
# After the fix, branch suffixes are stripped BEFORE the "llm-" strip.

test_that("branch-suffixed llm paths strip to llm", {
  # "llm-feat-cc-20260521-092847" must give "llm", not "feat"
  expect_equal(.canonicalize_project_local("llm-feat-cc-20260521-092847"), "llm")
  expect_equal(.canonicalize_project_local("llm-sonnet"),                  "llm")
  expect_equal(.canonicalize_project_local("llm-wt-193"),                  "llm")
  expect_equal(.canonicalize_project_local("llm-wt-198"),                  "llm")
})

test_that("branch-suffixed llmtelemetry paths strip to llmtelemetry", {
  expect_equal(.canonicalize_project_local("llmtelemetry-feat-cc-20260524-102501"),
               "llmtelemetry")
  expect_equal(
    .canonicalize_project_local("docs-gh-llmtelemetry-feat-cc-20260524-102501"),
    "llmtelemetry"
  )
  expect_equal(
    .canonicalize_project_local("-Users-johngavin-docs-gh-llmtelemetry-feat-cc-20260524-102501"),
    "llmtelemetry"
  )
})

# ── .claude/worktrees/agent-<hex> paths → NA ─────────────────────────────────

test_that(".claude/worktrees/agent-<hex> path returns NA", {
  expect_equal(.canonicalize_project_local(".claude/worktrees/agent-ab6f701adcaed79e9"),
               NA_character_)
})

# ── Bare hex-hash strings → NA ───────────────────────────────────────────────

test_that("bare hex agent hash of 12+ chars returns NA", {
  expect_equal(.canonicalize_project_local("ab6f701adcaed79e9"), NA_character_)
  expect_equal(.canonicalize_project_local("af82a624c38fab7a"),  NA_character_)
})

# ── Fragment-noise tokens → NA ────────────────────────────────────────────────
# These tokens appeared in telemetry as canonicalized project names despite
# being branch fragments or ephemeral process names, not real projects.

test_that("branch fragment tokens (feat, wt, scope, etc.) return NA", {
  noise <- c("feat", "fix", "chore", "wt", "scope",
             "repo", "docs", "project")
  for (tok in noise) {
    expect_equal(.canonicalize_project_local(tok), NA_character_,
                 info = sprintf("'%s' should be NA", tok))
  }
})

test_that("'agent' residual token returns NA (2026-05-26: agent-<x> path residual)", {
  # User confirmed 2026-05-26: "agent" is NOT a real project — it is a noise
  # token produced when an agent-<hex> worktree path collapses to its first segment.
  expect_equal(.canonicalize_project_local("agent"), NA_character_)
})

test_that("former agent-tooling tokens (roborev, sonnet, cc, eval, subagents, worker, ClaudeProbe) now return NA", {
  # 2026-05-26: these tokens are noise — drop to NA (reverses 2026-05-25 bucketing).
  # They have no recoverable parent project and pollute by-project plots.
  agent_tooling <- c("roborev", "ClaudeProbe", "sonnet", "cc", "eval", "subagents", "worker")
  for (tok in agent_tooling) {
    expect_equal(.canonicalize_project_local(tok), NA_character_,
                 info = sprintf("'%s' should be NA", tok))
  }
})

test_that("ClaudeProject and literal agent-tooling return NA", {
  # 2026-05-26: ClaudeProject is the Claude Code default project name — noise.
  # The literal "agent-tooling" is added defensively: any pre-computed stale
  # value re-canonicalizes to NA rather than passing through.
  expect_equal(.canonicalize_project_local("ClaudeProject"), NA_character_)
  expect_equal(.canonicalize_project_local("agent-tooling"), NA_character_)
})

test_that("network remaps to irish_buoy_network", {
  expect_equal(.canonicalize_project_local("network"), "irish_buoy_network")
})

test_that("telemetry remaps to llmtelemetry", {
  expect_equal(.canonicalize_project_local("telemetry"), "llmtelemetry")
})

test_that("urban_planning, football, knowledge are preserved as real projects", {
  expect_equal(.canonicalize_project_local("urban_planning"), "urban_planning")
  expect_equal(.canonicalize_project_local("football"),       "football")
  expect_equal(.canonicalize_project_local("knowledge"),      "knowledge")
})

test_that("user-confirmed noise tokens (demos, wiki) return NA", {
  # Confirmed as noise by user 2026-05-24: demos = demo content, wiki = internal wiki.
  expect_equal(.canonicalize_project_local("demos"), NA_character_)
  expect_equal(.canonicalize_project_local("wiki"),  NA_character_)
})

# ── Regression: real projects must not be affected ───────────────────────────

test_that("real project names are unaffected by the fix", {
  expect_equal(.canonicalize_project_local("llm"),                     "llm")
  expect_equal(.canonicalize_project_local("llmtelemetry"),            "llmtelemetry")
  expect_equal(.canonicalize_project_local("footbet"),                 "footbet")
  expect_equal(.canonicalize_project_local("randomwalk"),              "randomwalk")
  expect_equal(.canonicalize_project_local("acd_area_climate_design"), "acd_area_climate_design")
  expect_equal(.canonicalize_project_local("irish_buoy_network"),      "irish_buoy_network")
})

test_that("user-confirmed real projects (hartree, maps, tlang) are PRESERVED", {
  # Regression guard: user confirmed 2026-05-24 that these are real projects.
  # A future change that accidentally adds them to meta_only must fail here.
  expect_equal(.canonicalize_project_local("hartree"), "hartree")
  expect_equal(.canonicalize_project_local("maps"),    "maps")
  expect_equal(.canonicalize_project_local("tlang"),   "tlang")
})

test_that("vectorised branch-suffix stripping works across a mixed column", {
  input <- c(
    "llm-feat-cc-20260521-092847",          # should give llm
    "llm-wt-193",                           # should give llm
    "llmtelemetry-feat-cc-20260524-102501", # should give llmtelemetry
    "cc",                                   # 2026-05-26: now NA (noise, not agent-tooling)
    "ab6f701adcaed79e9",                    # hex hash -> NA
    "footbet"                               # real project unchanged
  )
  expected <- c("llm", "llm", "llmtelemetry", NA_character_, NA_character_, "footbet")
  expect_equal(.canonicalize_project_local(input), expected)
})
