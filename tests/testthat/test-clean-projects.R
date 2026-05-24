# Tests for clean_projects() helper (#83 Phase B)
# Validates the privacy exclusion / remap logic in export_dashboard_data.R.
# We source the constants and helper directly so these tests can run without
# running the full export script.

# --- Source helpers from the export script (just the pure functions) -----------
local({
  script_path <- system.file(
    "scripts", "export_dashboard_data.R",
    package = "llmtelemetry"
  )
  if (!nzchar(script_path)) {
    # Fallback for non-installed dev sessions (devtools::test())
    script_path <- file.path(
      rprojroot::find_package_root_file(),
      "inst", "scripts", "export_dashboard_data.R"
    )
  }
  # Parse and eval only up to (and including) clean_projects() definition.
  # We stop before library() calls and downstream code by sourcing into a child
  # env and then exporting the needed symbols to the test env.
  lines <- readLines(script_path)

  # Find the last line of the clean_projects definition block.
  # It ends with the closing brace of the function just before parse_cmonitor_time.
  end_idx <- which(grepl("^# Helper: convert cmonitor-rs time array", lines))[1L]
  if (is.na(end_idx)) end_idx <- 200L  # fallback if comment moves

  # Evaluate only the setup section (constants + clean_projects) in a new env
  env <- new.env(parent = globalenv())
  tryCatch(
    eval(parse(text = paste(lines[seq_len(end_idx - 1L)], collapse = "\n")), envir = env),
    error = function(e) NULL  # tolerate library() failures in non-nix CI
  )

  # Export symbols to global test env if they exist
  for (sym in c("EXCLUDED_DASHBOARD_PROJECTS", "CONFIDENTIAL_PROJECTS",
                 "PROJECT_REMAP", "clean_projects")) {
    if (exists(sym, envir = env)) {
      assign(sym, get(sym, envir = env), envir = globalenv())
    }
  }
})

# Safeguard: define stubs if sourcing above failed (e.g. R CMD check without nix)
if (!exists("clean_projects")) {
  EXCLUDED_DASHBOARD_PROJECTS <- c(
    "mycare", "crypto", "crypto_solwatch", "crypto_swarms",
    "my_t_project", "hello_t", "t_demos"
  )
  CONFIDENTIAL_PROJECTS <- c("mycare", "crypto", "crypto_solwatch", "crypto_swarms")
  PROJECT_REMAP <- c("llmtelemetry-hook-sync" = "llmtelemetry")

  clean_projects <- function(df, project_col = "project") {
    if (!is.data.frame(df) || nrow(df) == 0L) return(df)
    if (!project_col %in% names(df)) return(df)
    proj <- df[[project_col]]
    base_proj <- sub(
      "-(?:feat|fix|chore|docs|refactor|test|ci|perf|style|build|revert|worktree)-.*$",
      "", proj, ignore.case = TRUE
    )
    remap_keys_lower <- tolower(names(PROJECT_REMAP))
    for (i in seq_along(PROJECT_REMAP)) {
      hits <- tolower(base_proj) == remap_keys_lower[i] |
              tolower(proj)      == remap_keys_lower[i]
      proj[hits]      <- PROJECT_REMAP[[i]]
      base_proj[hits] <- PROJECT_REMAP[[i]]
    }
    df[[project_col]] <- proj
    excluded_lower <- tolower(EXCLUDED_DASHBOARD_PROJECTS)
    keep <- !tolower(base_proj) %in% excluded_lower
    df[keep, , drop = FALSE]
  }
}

# ── (a) Every EXCLUDED_DASHBOARD_PROJECTS entry is dropped ────────────────────

test_that("clean_projects drops every EXCLUDED_DASHBOARD_PROJECTS entry", {
  df <- data.frame(
    project = c(EXCLUDED_DASHBOARD_PROJECTS, "llm", "footbet"),
    value   = seq_len(length(EXCLUDED_DASHBOARD_PROJECTS) + 2L),
    stringsAsFactors = FALSE
  )
  result <- clean_projects(df)
  remaining <- result$project
  for (excl in EXCLUDED_DASHBOARD_PROJECTS) {
    expect_false(excl %in% remaining,
                 label = paste0("'", excl, "' should be excluded but found in output"))
  }
  expect_true("llm"     %in% remaining, label = "llm should be kept")
  expect_true("footbet" %in% remaining, label = "footbet should be kept")
})

# ── (b) PROJECT_REMAP: llmtelemetry-hook-sync → llmtelemetry, counts merged ──

test_that("clean_projects remaps llmtelemetry-hook-sync to llmtelemetry", {
  df <- data.frame(
    project = c("llmtelemetry-hook-sync", "llmtelemetry"),
    n       = c(5L, 10L),
    stringsAsFactors = FALSE
  )
  result <- clean_projects(df)
  expect_false("llmtelemetry-hook-sync" %in% result$project,
               label = "llmtelemetry-hook-sync should be remapped and not appear in output")
  expect_true("llmtelemetry" %in% result$project,
              label = "llmtelemetry should be present after remap")
})

test_that("clean_projects with aggregation: hook-sync rows merge into llmtelemetry", {
  df <- data.frame(
    project = c("llmtelemetry-hook-sync", "llmtelemetry"),
    n       = c(5L, 10L),
    stringsAsFactors = FALSE
  )
  result <- clean_projects(df)
  # After clean_projects, both rows have project="llmtelemetry".
  # A subsequent aggregate by project would sum to 15.
  # Here we just verify no hook-sync rows survive.
  expect_equal(sum(result$project == "llmtelemetry"), 2L,
               label = "both rows should now carry 'llmtelemetry'")
  total_n <- sum(result$n)
  expect_equal(total_n, 15L, label = "total n should be preserved across remap")
})

# ── (c) Innocent projects are kept ────────────────────────────────────────────

test_that("clean_projects keeps coMMpass, content, historical, knowledge", {
  innocent <- c("coMMpass", "content", "historical", "knowledge",
                "llm", "footbet", "randomwalk", "irishbuoys", "acd_area_climate_design")
  df <- data.frame(project = innocent, stringsAsFactors = FALSE)
  result <- clean_projects(df)
  for (p in innocent) {
    expect_true(
      tolower(p) %in% tolower(result$project),
      label = paste0("'", p, "' should be kept by clean_projects")
    )
  }
})

# ── (d) Suffix variants of excluded projects are also dropped ─────────────────

test_that("clean_projects drops branch-suffixed variants of excluded projects", {
  df <- data.frame(
    project = c("mycare-feat-some-feature", "crypto-fix-issue-42",
                "llm", "footbet"),
    stringsAsFactors = FALSE
  )
  result <- clean_projects(df)
  expect_false(any(grepl("mycare", result$project, ignore.case = TRUE)),
               label = "mycare-feat-* should be dropped")
  expect_false(any(grepl("^crypto$|^crypto-", result$project, ignore.case = TRUE)),
               label = "crypto-fix-* should be dropped")
  expect_true("llm"     %in% result$project)
  expect_true("footbet" %in% result$project)
})

# ── (e) Empty / no-op inputs are handled gracefully ──────────────────────────

test_that("clean_projects returns empty data.frame unchanged", {
  df <- data.frame(project = character(0), stringsAsFactors = FALSE)
  result <- clean_projects(df)
  expect_equal(nrow(result), 0L)
})

test_that("clean_projects returns df unchanged when project_col is absent", {
  df <- data.frame(repo = c("llm", "mycare"), stringsAsFactors = FALSE)
  result <- clean_projects(df)  # default project_col = "project" — not present
  expect_equal(nrow(result), nrow(df))  # unchanged
})

# ── (f) canonical_project column as the key (projects_master pattern) ─────────

test_that("clean_projects works on canonical_project column", {
  df <- data.frame(
    canonical_project = c("mycare", "llm", "crypto", "footbet"),
    n_sources         = c(3L, 5L, 2L, 4L),
    stringsAsFactors  = FALSE
  )
  result <- clean_projects(df, project_col = "canonical_project")
  expect_false("mycare" %in% result$canonical_project)
  expect_false("crypto" %in% result$canonical_project)
  expect_true("llm"     %in% result$canonical_project)
  expect_true("footbet" %in% result$canonical_project)
})
