#' Privacy exclusion list for dashboard outputs
#'
#' Returns a character vector of project names whose data MUST NOT appear in
#' any public dashboard output (JSON, parquet, or rendered HTML).
#'
#' This is the **single source of truth** for the dashboard exclusion list.
#' Both `inst/scripts/export_dashboard_data.R` and any future ETL surface
#' must use this function instead of a local hardcoded constant.
#'
#' The list covers:
#' - Medically sensitive projects (mycare and aliases).
#' - Crypto/financial projects whose names should not appear in public telemetry
#'   (crypto, crypto_solwatch, crypto_swarms, and their short-form aliases).
#' - Demo/test T-lang projects (my_t_project, hello_t, t_demos).
#'
#' To add a new excluded project, add its name here.  Changes here propagate
#' automatically to both `export_dashboard_data.R` (via `source()`) and to any
#' package consumer that calls `excluded_dashboard_projects()`.
#'
#' Origin: JohnGavin/llmtelemetry#83 Phase B (privacy sanitisation).
#' Centralised: JohnGavin/llmtelemetry#265.
#'
#' @return A character vector of project names to exclude from public outputs.
#' @export
excluded_dashboard_projects <- function() {
  c(
    # Medically sensitive
    "mycare",
    # Crypto / financial
    "crypto",
    "crypto_solwatch",
    "crypto_swarms",
    "solwatch",    # short-form alias of crypto_solwatch
    "swarms",      # short-form alias of crypto_swarms
    # Demo / test T-lang projects
    "my_t_project",
    "hello_t",
    "t_demos"
  )
}
