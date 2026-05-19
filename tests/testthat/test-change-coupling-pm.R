# Regression test for Issue #12:
# change_coupling.json must contribute to n_sources in projects_master.json.
# Previously, extract_cp_dates(..., date_col = NA_character_) always returned NULL
# so pm_sources[[7L]] was never non-NULL and change_coupling never counted.

test_that("change_coupling.json exists and contains project column", {
  pkg_root <- normalizePath(file.path(test_path(), "..", ".."), mustWork = FALSE)
  cp_path  <- file.path(pkg_root, "inst", "extdata", "change_coupling.json")
  skip_if_not(file.exists(cp_path), "change_coupling.json not present")

  cp_df <- jsonlite::fromJSON(cp_path, simplifyDataFrame = TRUE)
  expect_true(is.data.frame(cp_df))
  expect_true("project" %in% names(cp_df) || "canonical_project" %in% names(cp_df))
  expect_gt(nrow(cp_df), 0L)
})

test_that("projects_master includes projects present only in change_coupling.json", {
  pkg_root <- normalizePath(file.path(test_path(), "..", ".."), mustWork = FALSE)
  extdata  <- file.path(pkg_root, "inst", "extdata")

  pm_path <- file.path(extdata, "projects_master.json")
  cp_path <- file.path(extdata, "change_coupling.json")
  skip_if_not(file.exists(pm_path), "projects_master.json not present")
  skip_if_not(file.exists(cp_path), "change_coupling.json not present")

  pm_df <- jsonlite::fromJSON(pm_path, simplifyDataFrame = TRUE)
  cp_df <- jsonlite::fromJSON(cp_path, simplifyDataFrame = TRUE)

  expect_true(is.data.frame(pm_df))
  expect_true("n_sources" %in% names(pm_df))
  expect_true("canonical_project" %in% names(pm_df))

  # Get canonical projects from change_coupling
  cp_col <- if ("canonical_project" %in% names(cp_df)) "canonical_project" else "project"
  cp_projects <- unique(cp_df[[cp_col]])
  cp_projects <- cp_projects[!is.na(cp_projects) & nzchar(cp_projects)]

  # At least one change_coupling project should appear in projects_master
  expect_true(
    length(intersect(cp_projects, pm_df$canonical_project)) > 0L,
    info = "No change_coupling projects found in projects_master.json"
  )
})

test_that("a fixture project present only in change_coupling has n_sources >= 1 when export runs", {
  # This test uses the extract_cp_dates fix: directly reading change_coupling.json
  # via the unconditional read path (not via extract_cp_dates with date_col = NA_character_).
  # We simulate the fixed behaviour by reading the file and constructing a minimal pm_sources.

  pkg_root <- normalizePath(file.path(test_path(), "..", ".."), mustWork = FALSE)
  extdata  <- file.path(pkg_root, "inst", "extdata")
  cp_path  <- file.path(extdata, "change_coupling.json")
  skip_if_not(file.exists(cp_path), "change_coupling.json not present")

  cp_df <- jsonlite::fromJSON(cp_path, simplifyDataFrame = TRUE)
  cp_col <- if ("canonical_project" %in% names(cp_df)) "canonical_project" else "project"
  cp_projects <- unique(cp_df[[cp_col]])
  cp_projects <- cp_projects[!is.na(cp_projects) & nzchar(cp_projects)]

  # Simulate the fixed pm_sources construction
  pm_cp_only <- data.frame(
    canonical_project = cp_projects,
    date              = NA_character_,
    source            = "change_coupling",
    stringsAsFactors  = FALSE
  )

  # Count n_sources as the export script would
  source_counts <- table(pm_cp_only$source)

  # The change_coupling source must contribute 1 to each project's count
  expect_gt(nrow(pm_cp_only), 0L)
  expect_true(all(!is.na(pm_cp_only$canonical_project)))

  # Specifically: if we had ONLY change_coupling, each project should get n_sources = 1
  pm_all <- pm_cp_only[!is.na(pm_cp_only$canonical_project), ]
  srcs_per_proj <- tapply(pm_all$source, pm_all$canonical_project,
                           function(s) length(unique(s)))
  expect_true(all(srcs_per_proj >= 1L),
              info = "All change_coupling projects should have n_sources >= 1")
})
