#' Build the roborev targets plan
#'
#' Returns a list of targets that scrape roborev review findings and upsert
#' them into the unified DuckDB. Both data targets use
#' `tar_cue(mode = "always")` so they refresh on every `tar_make()` call.
#'
#' Downstream agents (loop detection, dashboard vignette) read from
#' `roborev_raw` and `roborev_upsert` respectively.
#'
#' @return A list of [targets::tar_target()] objects.
#' @export
plan_roborev <- function() {
  list(
    targets::tar_target(
      roborev_raw,
      scrape_roborev(repo_path = here::here()),
      cue = targets::tar_cue(mode = "always")
    ),
    targets::tar_target(
      roborev_db_path,
      Sys.getenv(
        "LLMTELEMETRY_UNIFIED_DB",
        path.expand("~/.claude/logs/unified.duckdb")
      )
    ),
    targets::tar_target(
      roborev_upsert,
      upsert_roborev_findings(roborev_raw, roborev_db_path),
      cue = targets::tar_cue(mode = "always")
    )
  )
}
