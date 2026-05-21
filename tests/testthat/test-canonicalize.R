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
  if (nzchar(r_file) && file.exists(r_file)) {
    source(r_file, local = FALSE)
  } else {
    # In R CMD check, functions are available via the package namespace.
    assign(".shorten_project_local",
           llmtelemetry:::.shorten_project_local, envir = .GlobalEnv)
    assign(".canonicalize_project_local",
           llmtelemetry:::.canonicalize_project_local, envir = .GlobalEnv)
    assign(".canonicalize_project_local_scalar",
           llmtelemetry:::.canonicalize_project_local_scalar, envir = .GlobalEnv)
  }
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

test_that("bare 'urban_planning' remains NA (meta_only)", {
  # Regression guard: underscore-to-slash conversion must not fire for bare
  # single-token names that are not path-like.  'urban_planning' is in meta_only
  # and must resolve to NA_character_, not "urban/planning" -> "urban".
  expect_equal(.canonicalize_project_local("urban_planning"), NA_character_)
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

test_that("path-prefixed docs_gh_urban_planning resolves to NA (meta_only)", {
  # docs_gh_urban_planning: the ^docs[-_]gh[-_] prefix is stripped first,
  # yielding "urban_planning" which is in meta_only -> NA_character_.
  expect_equal(.canonicalize_project_local("docs_gh_urban_planning"), NA_character_)
})

test_that("path-prefixed docs_gh_telemetry resolves to NA (meta_only)", {
  # docs_gh_telemetry: strips to "telemetry" which is in meta_only -> NA_character_.
  expect_equal(.canonicalize_project_local("docs_gh_telemetry"), NA_character_)
})

# ── Snapshot tests ────────────────────────────────────────────────────────────

test_that("dash-form, slash-form, underscore-path, and bare-underscore snapshot", {
  inputs <- c(
    "docs-gh-llmtelemetry",                      # dash-form
    "llmtelemetry/inst",                         # slash-form (already normalised)
    "docs_gh_llmtelemetry",                      # underscore-path
    "-Users_johngavin_docs_gh_llm",              # underscore full prefix
    "urban_planning",                            # bare underscore name (meta_only -> NA)
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
