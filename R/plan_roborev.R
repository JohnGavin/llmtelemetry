#' Build the roborev targets plan
#'
#' Returns a list of targets that scrape roborev review findings and upsert
#' them into the unified DuckDB. Both data targets use
#' `tar_cue(mode = "always")` so they refresh on every `tar_make()` call.
#'
#' Downstream agents (loop detection, dashboard vignette) consume
#' `roborev_findings`, which conforms to the upstream contract expected by
#' [plan_roborev_vignette()]. `roborev_raw` and `roborev_upsert` are retained
#' as intermediate targets for diagnostics and persistence.
#'
#' The `roborev_findings` target degrades gracefully: when [scrape_roborev()]
#' returns zero rows (e.g. because the unified.duckdb roborev tables are absent
#' or `roborev` binary is unavailable), it returns an empty tibble with the
#' correct schema rather than hard-failing.
#'
#' @return A list of `tar_target()` objects (from the `targets` package).
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
    ),
    targets::tar_target(
      roborev_findings,
      llmtelemetry:::.roborev_raw_to_findings(roborev_raw)
    )
  )
}
