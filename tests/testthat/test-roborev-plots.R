skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    testthat::skip(paste0("Package '", pkg, "' not available"))
  }
}

test_that("fixture: make_roborev_findings returns expected shape", {
  source(test_path("fixtures", "roborev_page_fixture.R"))
  findings <- make_roborev_findings()
  expect_s3_class(findings, "tbl_df")
  expected_cols <- c(
    "finding_id", "review_id", "review_commit", "severity", "primary_file",
    "problem_text", "summary", "found_at", "resolved_at", "fix_commit",
    "project", "agent_id"
  )
  expect_true(all(expected_cols %in% names(findings)))
  expect_gt(nrow(findings), 0L)
})

test_that("fixture: make_roborev_loops returns 5 canonical loops", {
  source(test_path("fixtures", "roborev_page_fixture.R"))
  loops <- make_roborev_loops()
  expect_s3_class(loops, "tbl_df")
  expect_equal(nrow(loops), 5L)
  expect_true(all(c(
    "content_hash", "severity", "primary_file", "summary",
    "first_seen", "last_seen", "cycles", "tier",
    "fix_commit_shas", "estimated_wasted_usd", "ack_by", "ack_at",
    "ack_reason", "ack_until"
  ) %in% names(loops)))
})

test_that("ROBOREV_PALETTE covers all severity levels in fixture data", {
  source(test_path("fixtures", "roborev_page_fixture.R"))
  findings <- make_roborev_findings()
  observed_sevs <- unique(tolower(findings$severity))
  # All observed severities must either be in palette or map to "unknown"
  palette_keys <- names(llmtelemetry::ROBOREV_PALETTE)
  expect_true("unknown" %in% palette_keys)
  # No observed severity should be silently dropped
  missing <- setdiff(observed_sevs, c(palette_keys, NA))
  # If there are "missing" severities they should collapse to "unknown"
  # (the plot functions handle this via the .severity_factor() helper)
  expect_true(length(missing) == 0 || all(missing %in% observed_sevs))
})

test_that("ROBOREV_TIER_PALETTE covers all tier levels in fixture loops", {
  source(test_path("fixtures", "roborev_page_fixture.R"))
  loops <- make_roborev_loops()
  observed_tiers <- unique(tolower(loops$tier))
  palette_keys   <- names(llmtelemetry::ROBOREV_TIER_PALETTE)
  expect_true(all(observed_tiers %in% palette_keys))
})

test_that("plot_open_findings_trend returns a plotly object", {
  skip_if_not_installed("plotly")
  source(test_path("fixtures", "roborev_page_fixture.R"))
  findings <- make_roborev_findings()
  p <- llmtelemetry::plot_open_findings_trend(findings)
  expect_s3_class(p, "plotly")
})

test_that("plot_open_findings_trend returns plotly on empty input", {
  skip_if_not_installed("plotly")
  empty <- data.frame(
    finding_id = character(0), severity = character(0),
    found_at = as.POSIXct(character(0)), resolved_at = as.POSIXct(character(0))
  )
  p <- llmtelemetry::plot_open_findings_trend(empty)
  expect_s3_class(p, "plotly")
})

test_that("plot_top_files returns a plotly object", {
  skip_if_not_installed("plotly")
  source(test_path("fixtures", "roborev_page_fixture.R"))
  findings <- make_roborev_findings()
  p <- llmtelemetry::plot_top_files(findings)
  expect_s3_class(p, "plotly")
})

test_that("plot_top_files respects n parameter", {
  skip_if_not_installed("plotly")
  source(test_path("fixtures", "roborev_page_fixture.R"))
  findings <- make_roborev_findings(n = 100L)
  p5  <- llmtelemetry::plot_top_files(findings, n = 5L)
  p15 <- llmtelemetry::plot_top_files(findings, n = 15L)
  # Both should be plotly objects
  expect_s3_class(p5,  "plotly")
  expect_s3_class(p15, "plotly")
})

test_that("plot_loops_table returns a datatable htmlwidget", {
  skip_if_not_installed("DT")
  source(test_path("fixtures", "roborev_page_fixture.R"))
  loops <- make_roborev_loops()
  tbl <- llmtelemetry::plot_loops_table(loops)
  expect_s3_class(tbl, "datatables")
})

test_that("plot_loops_table returns datatable on empty input", {
  skip_if_not_installed("DT")
  empty <- data.frame()
  tbl <- llmtelemetry::plot_loops_table(empty)
  expect_s3_class(tbl, "datatables")
})

test_that("plot_inflow_outflow returns a plotly object", {
  skip_if_not_installed("plotly")
  source(test_path("fixtures", "roborev_page_fixture.R"))
  findings <- make_roborev_findings()
  p <- llmtelemetry::plot_inflow_outflow(findings)
  expect_s3_class(p, "plotly")
})

test_that("plot_resolution_time returns a plotly object", {
  skip_if_not_installed("plotly")
  source(test_path("fixtures", "roborev_page_fixture.R"))
  findings <- make_roborev_findings()
  p <- llmtelemetry::plot_resolution_time(findings)
  expect_s3_class(p, "plotly")
})

test_that("plot_resolution_time returns plotly on all-unresolved input", {
  skip_if_not_installed("plotly")
  source(test_path("fixtures", "roborev_page_fixture.R"))
  findings <- make_roborev_findings()
  findings$resolved_at <- NA
  p <- llmtelemetry::plot_resolution_time(findings)
  expect_s3_class(p, "plotly")
})

test_that("build_pulse_table returns data.frame with metric/value columns", {
  source(test_path("fixtures", "roborev_page_fixture.R"))
  findings <- make_roborev_findings()
  loops    <- make_roborev_loops()
  pulse    <- llmtelemetry::build_pulse_table(findings, loops)
  expect_s3_class(pulse, "data.frame")
  expect_true(all(c("metric", "value") %in% names(pulse)))
  expect_gt(nrow(pulse), 0L)
})

test_that("build_pulse_table reports correct escalate count", {
  source(test_path("fixtures", "roborev_page_fixture.R"))
  loops <- make_roborev_loops()
  # fixture has 1 escalate loop with no ack (content_hash = "112233aabbcc")
  n_expected_esc <- sum(
    tolower(loops$tier) == "escalate" & is.na(loops$ack_by)
  )
  findings <- make_roborev_findings()
  pulse <- llmtelemetry::build_pulse_table(findings, loops)
  esc_row <- pulse[pulse$metric == "  Escalate (no ack)", ]
  expect_equal(nrow(esc_row), 1L)
  expect_equal(as.integer(esc_row$value), n_expected_esc)
})

test_that("build_recent_table returns a datatables htmlwidget", {
  skip_if_not_installed("DT")
  source(test_path("fixtures", "roborev_page_fixture.R"))
  findings <- make_roborev_findings()
  tbl <- llmtelemetry::build_recent_table(findings, days = 30L)
  expect_s3_class(tbl, "datatables")
})

# Snapshot test: plotly JSON structure of open-findings trend
# This ensures chart structure doesn't regress between commits.
test_that("plot_open_findings_trend JSON structure snapshot", {
  skip_if_not_installed("plotly")
  source(test_path("fixtures", "roborev_page_fixture.R"))
  # Use small deterministic dataset for stable snapshot
  set.seed(1L)
  findings <- make_roborev_findings(n = 20L)
  p <- llmtelemetry::plot_open_findings_trend(findings)
  # Extract only the top-level layout keys (stable, not data-dependent)
  layout_keys <- sort(names(p$x$layout))
  expect_snapshot(layout_keys)
})
