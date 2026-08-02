# Tests for R/runiverse.R — R-Universe API helpers (llm#761)
#
# The primary tests below use a recorded fixture
# (tests/testthat/fixtures/runiverse_packages_fixture.json — a trimmed
# capture of https://johngavin.r-universe.dev/api/packages taken 2026-07-10)
# mocked via httr2::local_mocked_responses(), so the test suite performs NO
# live network calls. One additional smoke test hits the live API and is
# skipped on CI and when offline.

fixture_path <- function(name) {
  testthat::test_path("fixtures", name)
}

mock_runiverse_response <- function() {
  raw_json <- readLines(fixture_path("runiverse_packages_fixture.json"), warn = FALSE)
  httr2::response(
    status_code = 200L,
    headers = list("content-type" = "application/json"),
    body = charToRaw(paste(raw_json, collapse = "\n"))
  )
}

# ---------------------------------------------------------------------------
# runiverse_packages()
# ---------------------------------------------------------------------------

test_that("runiverse_packages() validates its arguments", {
  expect_error(runiverse_packages(123))
  expect_error(runiverse_packages(""))
  expect_error(runiverse_packages("johngavin", package = 123))
})

test_that("runiverse_packages() returns one row per package with the documented schema", {
  httr2::local_mocked_responses(list(mock_runiverse_response()))

  result <- runiverse_packages("johngavin")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 6L)
  expect_named(
    result,
    c(
      "package", "version", "title", "maintainer", "maintainer_email",
      "owner", "status", "registered", "published", "stars", "downloads",
      "usedby", "devurl", "pkgdown", "upstream", "jobs"
    )
  )
  expect_setequal(
    result$package,
    c("irishbuoys", "micromort", "randomwalk", "coMMpass", "millsratio", "etfdata")
  )
})

test_that("runiverse_packages() types registered as logical and published as POSIXct", {
  httr2::local_mocked_responses(list(mock_runiverse_response()))

  result <- runiverse_packages("johngavin")

  expect_type(result$registered, "logical")
  expect_true(all(result$registered))
  expect_s3_class(result$published, "POSIXct")
  expect_false(anyNA(result$published))
})

test_that("runiverse_packages() jobs list-column holds the raw per-config build matrix", {
  httr2::local_mocked_responses(list(mock_runiverse_response()))

  result <- runiverse_packages("johngavin")
  irish <- result[result$package == "irishbuoys", ]

  expect_type(irish$jobs, "list")
  expect_length(irish$jobs, 1L)
  expect_gt(length(irish$jobs[[1]]), 0L)
})

test_that("runiverse_packages() returns the correct empty schema when the API returns zero packages", {
  httr2::local_mocked_responses(list(
    httr2::response(
      status_code = 200L,
      headers = list("content-type" = "application/json"),
      body = charToRaw("[]")
    )
  ))

  result <- runiverse_packages("an-empty-universe")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_named(
    result,
    c(
      "package", "version", "title", "maintainer", "maintainer_email",
      "owner", "status", "registered", "published", "stars", "downloads",
      "usedby", "devurl", "pkgdown", "upstream", "jobs"
    )
  )
})

# ---------------------------------------------------------------------------
# runiverse_checks()
# ---------------------------------------------------------------------------

test_that("runiverse_checks() validates its arguments", {
  expect_error(runiverse_checks(123))
  expect_error(runiverse_checks("johngavin", package = 123))
})

test_that("runiverse_checks() unnests one row per package x build config", {
  httr2::local_mocked_responses(list(mock_runiverse_response()))

  result <- runiverse_checks("johngavin")

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("package", "version", "config", "os", "r_version", "check", "build_time", "artifact", "status")
  )
  expect_gt(nrow(result), 6L) # more than one config per package
  expect_true(all(c("irishbuoys", "micromort") %in% result$package))
})

test_that("runiverse_checks() check column is an ordered factor and ERROR sorts above OK", {
  httr2::local_mocked_responses(list(mock_runiverse_response()))

  result <- runiverse_checks("johngavin")

  expect_s3_class(result$check, "ordered")
  expect_equal(levels(result$check), c("OK", "NOTE", "WARNING", "ERROR", "FAIL"))
  expect_true(factor("ERROR", levels(result$check), ordered = TRUE) >
    factor("OK", levels(result$check), ordered = TRUE))
})

test_that("runiverse_checks() derives os from config and reports irishbuoys linux-devel as ERROR", {
  httr2::local_mocked_responses(list(mock_runiverse_response()))

  result <- runiverse_checks("johngavin")
  row <- result[result$package == "irishbuoys" & result$config == "linux-devel-x86_64", ]

  expect_equal(nrow(row), 1L)
  expect_equal(as.character(row$check), "ERROR")
  expect_equal(row$os, "linux")
})

test_that("runiverse_checks() returns the correct empty schema when there are no packages", {
  httr2::local_mocked_responses(list(
    httr2::response(
      status_code = 200L,
      headers = list("content-type" = "application/json"),
      body = charToRaw("[]")
    )
  ))

  result <- runiverse_checks("an-empty-universe")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_s3_class(result$check, "factor")
})

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

test_that(".runiverse_config_os() maps known config prefixes", {
  configs <- c(
    "linux-devel-x86_64", "macos-release-arm64", "windows-oldrel",
    "source", "wasm-release", "pkgdown", "unknown-config"
  )
  expect_equal(
    llmtelemetry:::.runiverse_config_os(configs),
    c("linux", "macos", "windows", "source", "wasm", "pkgdown", NA_character_)
  )
})

test_that(".runiverse_as_time() parses ISO-8601 timestamps and passes through NA", {
  expect_s3_class(llmtelemetry:::.runiverse_as_time("2026-07-10T13:04:37.291Z"), "POSIXct")
  expect_true(is.na(llmtelemetry:::.runiverse_as_time(NA)))
})

# ---------------------------------------------------------------------------
# Live smoke test — opt-in only, never runs in a default `devtools::test()`
# or in CI. Set LLMTELEMETRY_RUN_LIVE_TESTS=true to exercise it manually.
# ---------------------------------------------------------------------------

test_that("runiverse_packages() works against the live API (manual opt-in only)", {
  testthat::skip_if_not(
    identical(Sys.getenv("LLMTELEMETRY_RUN_LIVE_TESTS"), "true"),
    "set LLMTELEMETRY_RUN_LIVE_TESTS=true to run this live-network test"
  )
  testthat::skip_on_ci()
  testthat::skip_if_offline()

  result <- runiverse_packages("johngavin", package = "irishbuoys")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_equal(result$package, "irishbuoys")
})
