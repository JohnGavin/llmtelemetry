# Read git pulse Parquet files — daily project diagnostics
# All access via duckplyr::read_parquet_duckdb(), never raw SQL

#' Read git pulse data from Parquet files
#'
#' Reads daily Parquet snapshots written by git_project_pulse.sh.
#' Uses duckplyr for zero-SQL Parquet access.
#'
#' @param log_dir Directory containing Parquet files. Defaults to ~/.claude/logs/git/.
#' @param since Only include snapshots from this date onward (character YYYY-MM-DD or Date).
#' @return tibble with columns: snapshot_date, project, period, period_label, metric, value
#' @export
read_git_pulse <- function(log_dir = file.path(Sys.getenv("HOME"), ".claude", "logs", "git"),
                           since = NULL) {
  checkmate::assert_directory_exists(log_dir)

  parquet_files <- list.files(log_dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(parquet_files) == 0) {
    cli::cli_warn("No Parquet files found in {.path {log_dir}}")
    return(tibble::tibble(
      snapshot_date = character(), project = character(), period = character(),
      period_label = character(), metric = character(), value = character()
    ))
  }

  # Filter files by date if since is provided

if (!is.null(since)) {
    since <- as.Date(since)
    file_dates <- as.Date(gsub("\\.parquet$", "", basename(parquet_files)))
    parquet_files <- parquet_files[file_dates >= since]
  }

  if (length(parquet_files) == 0) {
    cli::cli_warn("No Parquet files found since {since}")
    return(tibble::tibble())
  }

  # Read all matching Parquet files via duckplyr (zero SQL)
  result <- purrr::map_dfr(parquet_files, function(f) {
    duckplyr::read_parquet_duckdb(f) |>
      dplyr::collect()
  })

  # Type coercion after collect (duckdplyr gotcha: do this outside DuckDB)
  result |>
    dplyr::mutate(
      snapshot_date = as.Date(snapshot_date),
      value = as.integer(value)
    )
}

#' Get weekly commit trends across projects
#'
#' @inheritParams read_git_pulse
#' @return tibble with project, week, commits
#' @export
git_weekly_commits <- function(log_dir = file.path(Sys.getenv("HOME"), ".claude", "logs", "git"),
                               since = NULL) {
  read_git_pulse(log_dir = log_dir, since = since) |>
    dplyr::filter(metric == "commits", period == "week") |>
    dplyr::select(snapshot_date, project, week = period_label, commits = value) |>
    # Take latest snapshot per project+week (in case of multiple days)
    dplyr::slice_max(snapshot_date, n = 1, by = c(project, week)) |>
    dplyr::select(-snapshot_date) |>
    dplyr::arrange(project, week)
}

#' Get file churn hotspots across projects
#'
#' @inheritParams read_git_pulse
#' @return tibble with project, file_path, n_changes
#' @export
git_churn_hotspots <- function(log_dir = file.path(Sys.getenv("HOME"), ".claude", "logs", "git"),
                                since = NULL) {
  read_git_pulse(log_dir = log_dir, since = since) |>
    dplyr::filter(metric == "file_churn") |>
    dplyr::select(snapshot_date, project, file_path = period_label, n_changes = value) |>
    dplyr::slice_max(snapshot_date, n = 1, by = c(project, file_path)) |>
    dplyr::select(-snapshot_date) |>
    dplyr::arrange(project, dplyr::desc(n_changes))
}

#' Get project health summary (latest snapshot)
#'
#' @inheritParams read_git_pulse
#' @return tibble with one row per project, key health metrics
#' @export
git_project_health <- function(log_dir = file.path(Sys.getenv("HOME"), ".claude", "logs", "git"),
                                since = NULL) {
  pulse <- read_git_pulse(log_dir = log_dir, since = since)
  if (nrow(pulse) == 0) return(tibble::tibble())

  latest <- max(pulse$snapshot_date, na.rm = TRUE)
  pulse_latest <- dplyr::filter(pulse, snapshot_date == latest)

  # Pivot scalar metrics to columns
  scalar_metrics <- c("total_commits", "lines_of_code", "files_changed",
                       "files_added", "files_deleted", "files_net_growth",
                       "lines_changed", "reverts_hotfixes", "commit_msg_length")

  pulse_latest |>
    dplyr::filter(metric %in% scalar_metrics) |>
    dplyr::select(project, metric, value) |>
    tidyr::pivot_wider(names_from = metric, values_from = value)
}
