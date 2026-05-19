# Tests for dashboard filter sentinel logic (Issues 3, 4, 5, 6)
# resolve_sel() is defined inline in vignettes/dashboard_shinylive.qmd.
# We replicate it here to verify correct sentinel behaviour without
# requiring the full Shiny runtime.

# Replicated from vignettes/dashboard_shinylive.qmd ~line 1222
make_resolve_sel <- function(all_projects) {
  function(sel) {
    if (is.null(sel) || "__ALL__" %in% sel) return(NULL)
    if ("__NONE__" %in% sel) return(character(0))
    sel
  }
}

all_projects <- c("llm", "llmtelemetry", "randomwalk")
resolve_sel <- make_resolve_sel(all_projects)

# --- Issue 5 (restored): __ALL__ returns NULL so cc_d_proj falls back to cc_d() ---
test_that("resolve_sel returns NULL for __ALL__ (restores cc_d() fallback)", {
  expect_null(resolve_sel("__ALL__"))
  expect_null(resolve_sel(NULL))
})

# --- Issue 4: __NONE__ returns character(0) so charts show zero rows ---
test_that("resolve_sel returns character(0) for __NONE__", {
  result <- resolve_sel("__NONE__")
  expect_identical(result, character(0))
  expect_length(result, 0L)
})

# --- Issue 6: specific selection returns the vector as-is (not filtered to master) ---
test_that("resolve_sel passes through specific project vectors unchanged", {
  sel <- c("llm", "randomwalk")
  expect_identical(resolve_sel(sel), sel)
})

test_that("resolve_sel passes through a single project name", {
  expect_identical(resolve_sel("llmtelemetry"), "llmtelemetry")
})

# --- Issue 6: a project NOT in all_projects still passes through (live projects) ---
test_that("resolve_sel does not filter out projects absent from master list", {
  live_only <- "new-project-not-yet-in-master"
  expect_identical(resolve_sel(live_only), live_only)
})

# --- Caller contract: NULL from resolve_sel means skip IN filter ---
test_that("NULL from resolve_sel correctly causes is.null() callers to skip filter", {
  sel <- resolve_sel("__ALL__")
  # Callers use: if (!is.null(sel)) d <- d[d$col %in% sel, ]
  # With NULL, the filter is skipped -- all rows are kept.
  d <- data.frame(project = c("llm", "llmtelemetry", "live-only"), x = 1:3)
  if (!is.null(sel)) d <- d[d$project %in% sel, ]
  expect_equal(nrow(d), 3L)  # All rows kept -- including "live-only"
})

# --- Caller contract: character(0) from resolve_sel => zero rows ---
test_that("character(0) from resolve_sel filters data frame to zero rows", {
  sel <- resolve_sel("__NONE__")
  d <- data.frame(project = c("llm", "llmtelemetry"), x = 1:2)
  if (!is.null(sel)) d <- d[d$project %in% sel, ]
  expect_equal(nrow(d), 0L)
})

# --- Issue 3 verification: apostrophe escaping for DuckDB IN clause ---
# The JS quoting logic lives in vignettes/dashboard_shinylive.qmd line ~593:
#   projects.map(p => "'" + p.replace(/'/g, "''") + "'")
# This R test verifies the equivalent escaping rule:
#   a single quote is doubled to produce a valid SQL single-quoted literal.
test_that("apostrophe escaping doubles single quotes for SQL IN clause (Issue 3)", {
  sql_quote <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  expect_equal(sql_quote("llm"),            "'llm'")
  expect_equal(sql_quote("it's-a-project"), "'it''s-a-project'")
  expect_equal(sql_quote("has'two'quotes"), "'has''two''quotes'")
  expect_equal(sql_quote("no-apostrophes"), "'no-apostrophes'")
})
