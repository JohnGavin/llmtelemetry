# Tests for canonicalize_project() helper
# The helper is defined in inst/scripts/export_dashboard_data.R.
# We source it here via a minimal wrapper to avoid running the full export.

# Source the helper: parse and eval only the canonicalize_project block
# by sourcing the script up to the end of the helper definition.
# Simpler: define a local copy of the function matching the spec.

# Helper extractor: source the script in a local environment and grab the fn.
get_canonicalize_fn <- function() {
  script <- system.file(
    "scripts", "export_dashboard_data.R",
    package = "llmtelemetry"
  )
  if (!nzchar(script)) {
    skip("export_dashboard_data.R not found via system.file()")
  }
  env <- new.env(parent = baseenv())
  # Suppress messages from library() and cat() calls in the script
  # by only parsing and eval-ing the canonicalize_project definition.
  lines <- readLines(script)
  # Find the block: from "canonicalize_project <- function" to Vectorize line
  start <- which(grepl("^canonicalize_project <- function", lines))[1]
  end   <- which(grepl("^canonicalize_project <- Vectorize", lines))[1]
  if (is.na(start) || is.na(end)) {
    skip("canonicalize_project definition not found in export_dashboard_data.R")
  }
  block <- paste(lines[start:(end + 1L)], collapse = "\n")
  eval(parse(text = block), envir = env)
  env$canonicalize_project
}

canonicalize_project <- get_canonicalize_fn()

# ── scalar / vectorised identity ──────────────────────────────────────────────
test_that("simple project names pass through unchanged", {
  expect_equal(canonicalize_project("llm"),           "llm")
  expect_equal(canonicalize_project("llmtelemetry"),  "llmtelemetry")
  expect_equal(canonicalize_project("footbet"),       "footbet")
  expect_equal(canonicalize_project("mycare"),        "mycare")
  expect_equal(canonicalize_project("randomwalk"),    "randomwalk")
})

# ── worktree/ prefix stripping ─────────────────────────────────────────────────
test_that("worktree/ prefix is stripped", {
  expect_equal(canonicalize_project("worktree/llm"),          "llm")
  expect_equal(canonicalize_project("worktree/llmtelemetry"), "llmtelemetry")
})

test_that("nested worktree/project/subdir gives first segment", {
  expect_equal(canonicalize_project("worktree/llmtelemetry/sonnet"), "llmtelemetry")
})

# ── agent worktree ID stripping ────────────────────────────────────────────────
test_that("bare agent-worktree ID with no project returns NA", {
  expect_equal(canonicalize_project("D73dOZsvyf/repo"),  NA_character_)
  expect_equal(canonicalize_project("kSBNJFuu6G/repo"),  NA_character_)
})

test_that("agent-worktree ID prefix is stripped to reveal project", {
  expect_equal(canonicalize_project("D73dOZsvyf/repo/llm"),                    "llm")
  expect_equal(canonicalize_project("kSBNJFuu6G/repo/llmtelemetry/vignettes"), "llmtelemetry")
})

# ── former agent-tooling tokens → NA ──────────────────────────────────────────
test_that("former agent-tooling tokens now return NA (2026-05-26 decision reverses 2026-05-25)", {
  expect_equal(canonicalize_project("roborev"),     NA_character_)
  expect_equal(canonicalize_project("sonnet"),      NA_character_)
  expect_equal(canonicalize_project("cc"),          NA_character_)
  expect_equal(canonicalize_project("eval"),        NA_character_)
  expect_equal(canonicalize_project("subagents"),   NA_character_)
  expect_equal(canonicalize_project("worker"),      NA_character_)
  expect_equal(canonicalize_project("ClaudeProbe"), NA_character_)
})

test_that("ClaudeProject and literal agent-tooling return NA", {
  # ClaudeProject is the Claude Code default project name — treated as noise.
  # The literal "agent-tooling" re-canonicalizes to NA defensively.
  expect_equal(canonicalize_project("ClaudeProject"), NA_character_)
  expect_equal(canonicalize_project("agent-tooling"), NA_character_)
})

# ── meta-only names → NA ───────────────────────────────────────────────────────
test_that("meta-only names canonicalise to NA", {
  expect_equal(canonicalize_project("worktree"), NA_character_)
})

test_that("expanded meta-only top-level names canonicalise to NA", {
  expect_equal(canonicalize_project("antigravity"), NA_character_)
  expect_equal(canonicalize_project("crypto"),      NA_character_)
  expect_equal(canonicalize_project("data"),        NA_character_)
  expect_equal(canonicalize_project("github"),      NA_character_)
  expect_equal(canonicalize_project("hello"),       NA_character_)
  expect_equal(canonicalize_project("simulations"), NA_character_)
  expect_equal(canonicalize_project("sport"),       NA_character_)
})

test_that("new AA1 meta-only names canonicalise to NA", {
  expect_equal(canonicalize_project("t"),           NA_character_)
  expect_equal(canonicalize_project("io"),          NA_character_)
  expect_equal(canonicalize_project("notmineraft"), NA_character_)
})

test_that("urban_planning, football, knowledge are real projects (KEPT)", {
  # 2026-05-25: confirmed as real projects — must NOT be dropped.
  expect_equal(canonicalize_project("urban_planning"), "urban_planning")
  expect_equal(canonicalize_project("football"),       "football")
  expect_equal(canonicalize_project("knowledge"),      "knowledge")
})

test_that("network remaps to irish_buoy_network", {
  expect_equal(canonicalize_project("network"), "irish_buoy_network")
})

test_that("telemetry remaps to llmtelemetry", {
  expect_equal(canonicalize_project("telemetry"), "llmtelemetry")
})

# ── container-prefix stripping ─────────────────────────────────────────────────
test_that("container-directory sub-paths strip prefix to reveal project", {
  expect_equal(canonicalize_project("simulations/randomwalk"),  "randomwalk")
  expect_equal(canonicalize_project("sport/footbet"),           "footbet")
  expect_equal(canonicalize_project("subagents/foo"),           "foo")
})

test_that("notmineraft in simulations/ is itself meta-only so returns NA", {
  expect_equal(canonicalize_project("simulations/notmineraft"), NA_character_)
})

# ── explicit overrides ─────────────────────────────────────────────────────────
test_that("buoy/network maps to irish_buoy_network", {
  expect_equal(canonicalize_project("buoy/network"),      "irish_buoy_network")
  expect_equal(canonicalize_project("buoy/network/data"), "irish_buoy_network")
})

test_that("data/historical maps to historical", {
  expect_equal(canonicalize_project("data/historical"),       "historical")
  expect_equal(canonicalize_project("data/historical/2026"),  "historical")
})

test_that("data/micromort maps to micromort", {
  expect_equal(canonicalize_project("data/micromort"), "micromort")
})

test_that("crypto/swarms maps to swarms", {
  expect_equal(canonicalize_project("crypto/swarms"),     "swarms")
  expect_equal(canonicalize_project("crypto/swarms/sub"), "swarms")
})

# ── sub-path default: first segment ───────────────────────────────────────────
test_that("first path segment is returned for unrecognised sub-paths", {
  expect_equal(canonicalize_project("footbet/R"),    "footbet")
  expect_equal(canonicalize_project("llm/vignettes"), "llm")
})

# ── numeric-only segments → NA ────────────────────────────────────────────────
test_that("pure numeric strings (worktree IDs) return NA", {
  expect_equal(canonicalize_project("1020043174"),  NA_character_)
  expect_equal(canonicalize_project("94513747"),    NA_character_)
})

# ── null / NA / empty ─────────────────────────────────────────────────────────
test_that("NA input returns NA_character_", {
  expect_equal(canonicalize_project(NA_character_), NA_character_)
})

test_that("empty string returns NA_character_", {
  expect_equal(canonicalize_project(""), NA_character_)
})

# ── vectorised behaviour ───────────────────────────────────────────────────────
test_that("vectorised call over a column works correctly", {
  input    <- c("llm", "worktree/llm", "D73dOZsvyf/repo", "sonnet",
                "buoy/network", "data/historical", NA_character_, "footbet/R",
                "1020043174")
  # 2026-05-26: "sonnet" now returns NA (reverses 2026-05-25 agent-tooling bucket)
  expected <- c("llm", "llm",       NA_character_, NA_character_,
                "irish_buoy_network", "historical", NA_character_, "footbet",
                NA_character_)
  result   <- canonicalize_project(input)
  expect_equal(result, expected)
})

# ── Fragment-noise and hex-hash suppression (fix: project-name-canonicalization)

test_that("bare hex agent worktree hashes return NA", {
  expect_equal(canonicalize_project("ab6f701adcaed79e9"), NA_character_)
  expect_equal(canonicalize_project("af82a624c38fab7a"),  NA_character_)
})

test_that("fragment-noise tokens (non-agent-tooling) return NA", {
  noise <- c("feat", "fix", "chore", "wt", "scope",
             "repo", "docs", "project")
  for (tok in noise) {
    expect_equal(canonicalize_project(tok), NA_character_,
                 info = sprintf("'%s' should be NA", tok))
  }
})
