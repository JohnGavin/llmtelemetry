# Regression tests for Issue #11:
# canonicalize helpers in R/canonicalize.R and inst/scripts/export_dashboard_data.R
# must produce identical canonical names for historically split project names.

# ── Load both implementations ─────────────────────────────────────────────────
# Local R package helper: try development tree first, fall back to package
# namespace for R CMD check compatibility.
local({
  r_file <- tryCatch(
    normalizePath(file.path(test_path(), "..", "..", "R", "canonicalize.R"),
                  mustWork = TRUE),
    error = function(e) ""
  )
  assigned_names <- character(0L)
  if (nzchar(r_file) && file.exists(r_file)) {
    source(r_file, local = FALSE)
    assigned_names <- c(".shorten_project_local",
                        ".canonicalize_project_local",
                        ".canonicalize_project_local_scalar")
  } else {
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

# Export script helper (extract scalar function + shorten_project)
get_export_canonicalize_fn <- function() {
  script <- system.file(
    "scripts", "export_dashboard_data.R",
    package = "llmtelemetry"
  )
  if (!nzchar(script)) skip("export_dashboard_data.R not found")
  lines <- readLines(script)
  # shorten_project through Vectorize call (inclusive)
  start <- which(grepl("^# Helper: shorten project", lines))[1L]
  end   <- which(grepl("^canonicalize_project <- Vectorize", lines))[1L]
  if (is.na(start) || is.na(end)) skip("Helper block not found in export script")
  block <- paste(lines[start:end], collapse = "\n")
  env <- new.env(parent = baseenv())
  eval(parse(text = block), envir = env)
  list(
    canon = env$canonicalize_project,
    canon_session = env$canonicalize_session_project
  )
}

fns <- get_export_canonicalize_fn()
export_canonicalize          <- fns$canon
export_canonicalize_session  <- fns$canon_session

# ── Historically split names: both implementations must agree ─────────────────
test_that("export script and R/canonicalize.R produce identical results for irishbuoys forms", {
  # Slash-form input (as seen after shorten_project / .shorten_project_local)
  slash_inputs <- c(
    "irishbuoys",
    "buoy/network",
    "buoy/network/data",
    "data/weather/irish/buoy/network",
    "weather/irish/buoy/network"
  )
  for (inp in slash_inputs) {
    expect_equal(
      export_canonicalize(inp),
      .canonicalize_project_local(inp),
      info = sprintf("input '%s': export vs R/canonicalize.R differ", inp)
    )
  }
})

test_that("both helpers canonicalize raw dash-form paths to irish_buoy_network", {
  raw_path <- "-Users-johngavin-docs-gh-proj-data-weather-irish-buoy-network"
  expect_equal(export_canonicalize_session(raw_path), "irish_buoy_network")
})

test_that("both helpers canonicalize irishbuoys slash-form to irish_buoy_network", {
  expect_equal(export_canonicalize("irishbuoys"),   "irish_buoy_network")
  expect_equal(.canonicalize_project_local("irishbuoys"), "irish_buoy_network")
})

test_that("export script and R/canonicalize.R agree on stats/simulations sub-paths", {
  inputs <- c("stats/simulations/randomwalk", "simulations/randomwalk")
  for (inp in inputs) {
    expect_equal(
      export_canonicalize(inp),
      .canonicalize_project_local(inp),
      info = sprintf("input '%s': implementations differ", inp)
    )
    expect_equal(export_canonicalize(inp), "randomwalk",
                 info = sprintf("input '%s': expected randomwalk", inp))
  }
})

test_that("export script and R/canonicalize.R agree on stats/sport sub-paths", {
  inputs <- c("stats/sport/footbet", "sport/footbet")
  for (inp in inputs) {
    expect_equal(
      export_canonicalize(inp),
      .canonicalize_project_local(inp),
      info = sprintf("input '%s': implementations differ", inp)
    )
    expect_equal(export_canonicalize(inp), "footbet",
                 info = sprintf("input '%s': expected footbet", inp))
  }
})

test_that("NHS path form canonicalizes to mycare via export canonicalize_session_project", {
  nhs_path <- "-Users-johngavin-docs--pers-NHS-health-data-antigravity-mycare"
  expect_equal(export_canonicalize_session(nhs_path), "mycare")
})
