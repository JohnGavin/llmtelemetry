# plan_roborev_vignette.R
# targets plan for roborev_summary.qmd vignette.
# Upstream targets roborev_findings and roborev_loops are produced by
# Agent 1's plan_roborev.R and Agent 2's plan_roborev_loops.R respectively.
# In test environments these are provided by the fixture loader.

NULL

#' Roborev vignette targets plan
#'
#' Returns a list of tar_target() calls that pre-compute all objects read
#' by `vignettes/roborev_summary.qmd` via `safe_tar_read()`.
#'
#' Upstream contracts (from Agent 1 / Agent 2):
#'   - `roborev_findings`: tibble with columns finding_id, review_id,
#'     review_commit, severity, primary_file, problem_text, summary,
#'     found_at, resolved_at, fix_commit, project, agent_id
#'   - `roborev_loops`: tibble with columns content_hash, severity,
#'     primary_file, summary, first_seen, last_seen, cycles, tier,
#'     fix_commit_shas, estimated_wasted_usd, ack_by, ack_at,
#'     ack_reason, ack_until
#'
#' @return A named list of tar_target() objects.
#' @export
plan_roborev_vignette <- function() {
  if (!requireNamespace("targets", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg targets} is required for {.fn plan_roborev_vignette}.",
      "i" = 'Install it with: {.run install.packages("targets")}'
    ))
  }
  list(
    targets::tar_target(
      vig_roborev_pulse,
      build_pulse_table(roborev_findings, roborev_loops)
    ),
    targets::tar_target(
      vig_roborev_trend,
      plot_open_findings_trend(roborev_findings)
    ),
    targets::tar_target(
      vig_roborev_top,
      plot_top_files(roborev_findings)
    ),
    targets::tar_target(
      vig_roborev_loops,
      plot_loops_table(roborev_loops)
    ),
    targets::tar_target(
      vig_roborev_inflow,
      plot_inflow_outflow(roborev_findings)
    ),
    targets::tar_target(
      vig_roborev_resolution,
      plot_resolution_time(roborev_findings)
    ),
    targets::tar_target(
      vig_roborev_recent,
      build_recent_table(roborev_findings, days = 30L)
    )
  )
}
