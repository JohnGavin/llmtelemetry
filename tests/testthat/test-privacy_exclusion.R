## Tests for R/privacy_exclusion.R (#265)
## Regression fixture: the pre-refactor hardcoded EXCLUDED_DASHBOARD_PROJECTS
## from inst/scripts/export_dashboard_data.R was:
##   c("mycare", "crypto", "crypto_solwatch", "crypto_swarms",
##     "solwatch", "swarms", "my_t_project", "hello_t", "t_demos")

PRE_REFACTOR_LIST <- c(
  "mycare", "crypto", "crypto_solwatch", "crypto_swarms",
  "solwatch", "swarms",
  "my_t_project", "hello_t", "t_demos"
)

test_that("excluded_dashboard_projects() is non-empty (smoke test)", {
  result <- excluded_dashboard_projects()
  expect_true(is.character(result))
  expect_gt(length(result), 0L)
})

test_that("excluded_dashboard_projects() content matches pre-refactor hardcoded list", {
  result <- excluded_dashboard_projects()
  # Both directions: no dropped entries, no extra entries.
  missing <- setdiff(PRE_REFACTOR_LIST, result)
  extra   <- setdiff(result, PRE_REFACTOR_LIST)
  expect_equal(
    missing, character(0),
    label = "entries in pre-refactor list that are now MISSING from excluded_dashboard_projects()"
  )
  expect_equal(
    extra, character(0),
    label = "entries in excluded_dashboard_projects() not present in pre-refactor list"
  )
})
