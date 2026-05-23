# Tests for the {shinylive-r} parse guard (issue #159)
#
# These tests verify that check_shinylive_parse.R:
#   (a) exits 0 when the current dashboard_shinylive.qmd chunk parses cleanly
#   (b) exits 1 when given a deliberately broken {shinylive-r} chunk

# Helper: path to the guard script (works from source tree and installed pkg)
guard_script_path <- function() {
  # Try installed package first
  p <- system.file("scripts/check_shinylive_parse.R", package = "llmtelemetry")
  if (nchar(p) > 0 && file.exists(p)) return(p)
  # Fall back to source tree (running via devtools::test())
  src <- tryCatch(here::here(), error = function(e) NULL)
  if (!is.null(src)) {
    candidate <- file.path(src, "inst", "scripts", "check_shinylive_parse.R")
    if (file.exists(candidate)) return(candidate)
  }
  NULL
}

# Helper: path to dashboard_shinylive.qmd
dashboard_qmd_path <- function() {
  p <- system.file("../vignettes/dashboard_shinylive.qmd", package = "llmtelemetry")
  if (nchar(p) > 0 && file.exists(p)) return(p)
  src <- tryCatch(here::here(), error = function(e) NULL)
  if (!is.null(src)) {
    candidate <- file.path(src, "vignettes", "dashboard_shinylive.qmd")
    if (file.exists(candidate)) return(candidate)
  }
  NULL
}

# Helper: write a minimal .qmd with a {shinylive-r} chunk containing the given body
write_shinylive_qmd <- function(body_lines) {
  tmp <- tempfile(fileext = ".qmd")
  header <- c(
    "---",
    "title: 'test fixture'",
    "---",
    "",
    "```{shinylive-r}",
    "#| standalone: true"
  )
  footer <- c("```")
  writeLines(c(header, body_lines, footer), tmp)
  tmp
}

# ---------------------------------------------------------------------------

test_that("guard script exists and is readable", {
  skip_if_not_installed("here")
  guard <- guard_script_path()
  expect_true(!is.null(guard), info = "check_shinylive_parse.R not found")
  expect_true(file.exists(guard), info = paste("Script not found at:", guard))
})

test_that("dashboard_shinylive.qmd {shinylive-r} chunk parses cleanly", {
  skip_if_not_installed("here")

  guard <- guard_script_path()
  qmd   <- dashboard_qmd_path()

  skip_if(is.null(guard), "check_shinylive_parse.R not found — skip")
  skip_if(is.null(qmd),   "dashboard_shinylive.qmd not found — skip")

  result <- system2(
    "Rscript",
    args   = c(shQuote(guard), shQuote(qmd)),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_code <- attr(result, "status")
  # system2 returns NULL status on success (exit 0)
  expect_true(
    is.null(exit_code) || exit_code == 0L,
    info = paste(
      "Parse guard failed on dashboard_shinylive.qmd (expected exit 0).\n",
      "Output:\n",
      paste(result, collapse = "\n")
    )
  )
})

test_that("guard correctly rejects a broken {shinylive-r} chunk", {
  skip_if_not_installed("here")

  guard <- guard_script_path()
  skip_if(is.null(guard), "check_shinylive_parse.R not found — skip")

  # Broken chunk: unclosed parenthesis — same class of error as #156
  broken_body <- c(
    "library(shiny)",
    "",
    "# Deliberately broken: missing closing parenthesis",
    "ui <- fluidPage(",
    "  titlePanel('test'",   # missing closing )
    "",
    "server <- function(input, output) {}",
    "",
    "shinyApp(ui, server)"
  )

  fixture_qmd <- write_shinylive_qmd(broken_body)
  on.exit(unlink(fixture_qmd), add = TRUE)

  # suppressWarnings: system2() emits a warning when the child exits non-zero;
  # that is the expected behaviour here — we are testing for failure.
  result <- suppressWarnings(system2(
    "Rscript",
    args   = c(shQuote(guard), shQuote(fixture_qmd)),
    stdout = TRUE,
    stderr = TRUE
  ))
  exit_code <- attr(result, "status")

  # Guard MUST exit non-zero on a broken chunk
  expect_true(
    !is.null(exit_code) && exit_code != 0L,
    info = paste(
      "Parse guard did NOT reject a broken {shinylive-r} chunk (expected exit 1).\n",
      "Exit code:", if (is.null(exit_code)) "NULL (= 0)" else exit_code, "\n",
      "Output:\n",
      paste(result, collapse = "\n")
    )
  )

  # Error output should mention PARSE ERROR
  all_output <- paste(result, collapse = "\n")
  expect_true(
    grepl("PARSE ERROR", all_output, ignore.case = TRUE),
    info = paste("Expected 'PARSE ERROR' in output:\n", all_output)
  )
})

test_that("guard rejects a .qmd with no {shinylive-r} chunk", {
  skip_if_not_installed("here")

  guard <- guard_script_path()
  skip_if(is.null(guard), "check_shinylive_parse.R not found — skip")

  # A .qmd with a regular R chunk (not shinylive-r)
  tmp <- tempfile(fileext = ".qmd")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(
    "---",
    "title: 'no shinylive'",
    "---",
    "",
    "```{r}",
    "1 + 1",
    "```"
  ), tmp)

  result <- suppressWarnings(system2(
    "Rscript",
    args   = c(shQuote(guard), shQuote(tmp)),
    stdout = TRUE,
    stderr = TRUE
  ))
  exit_code <- attr(result, "status")

  expect_true(
    !is.null(exit_code) && exit_code != 0L,
    info = "Guard should exit non-zero when no {shinylive-r} chunk is found"
  )
})

test_that("guard exits non-zero when given a non-existent file", {
  skip_if_not_installed("here")

  guard <- guard_script_path()
  skip_if(is.null(guard), "check_shinylive_parse.R not found — skip")

  result <- suppressWarnings(system2(
    "Rscript",
    args   = c(shQuote(guard), shQuote("/tmp/does_not_exist_abc123.qmd")),
    stdout = TRUE,
    stderr = TRUE
  ))
  exit_code <- attr(result, "status")

  expect_true(
    !is.null(exit_code) && exit_code != 0L,
    info = "Guard should exit non-zero for missing input file"
  )
})
