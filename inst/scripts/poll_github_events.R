#!/usr/bin/env Rscript
# Poll GitHub issue events for scope change tracking (Layer 4)
# Run daily via cron or launchd
#
# Usage: Rscript inst/scripts/poll_github_events.R
# Output: inst/extdata/github_issue_events.json

library(jsonlite)
library(dplyr, warn.conflicts = FALSE)

# Projects to track
projects <- c(
  "JohnGavin/llm",
  "JohnGavin/llmtelemetry",
  "JohnGavin/irishbuoys",
  "JohnGavin/mycare",
  "JohnGavin/footbet"
)

load_previous <- function(owner_repo) {
  pkg_root <- here::here()
  out_path <- file.path(pkg_root, "inst", "extdata", "github_issue_events.json")
  if (!file.exists(out_path)) return(NULL)
  tryCatch({
    all_rows <- fromJSON(out_path, simplifyDataFrame = TRUE)
    if (is.null(all_rows) || nrow(all_rows) == 0) return(NULL)
    # Filter to only rows for this repo — avoids duplicating other projects' history
    repo_rows <- all_rows[all_rows$project == owner_repo, ]
    if (nrow(repo_rows) == 0) return(NULL)
    repo_rows
  }, error = function(e) NULL)
}

poll_issue_events <- function(owner_repo, since_days = 30) {
  cat(sprintf("Polling %s...\n", owner_repo))

  # Use gh CLI via system2 for GitHub API access
  parts <- strsplit(owner_repo, "/")[[1]]
  owner <- parts[1]
  repo <- parts[2]

  # Fetch issue events via gh api with multi-page iteration (#789)
  result <- tryCatch({
    page <- 1
    all_events_raw <- list()
    repeat {
      url <- sprintf("repos/%s/%s/issues/events?per_page=100&page=%d", owner, repo, page)
      page_json <- system2(
        "gh",
        args = c("api", url),
        stdout = TRUE,
        stderr = FALSE
      )
      status <- attr(page_json, "status")
      if (!is.null(status) && status != 0) break
      if (length(page_json) == 0 || identical(page_json[1], "")) break
      parsed <- tryCatch(
        fromJSON(paste(page_json, collapse = ""), simplifyDataFrame = FALSE),
        error = function(e) NULL
      )
      if (is.null(parsed) || length(parsed) == 0) break
      all_events_raw <- c(all_events_raw, parsed)
      if (length(parsed) < 100) break
      page <- page + 1
    }

    if (length(all_events_raw) == 0) {
      message(sprintf("  No events found for %s", owner_repo))
      return(NULL)
    }

    # Convert list-of-lists to data frame
    events <- fromJSON(
      jsonlite::toJSON(all_events_raw, auto_unbox = TRUE),
      simplifyDataFrame = TRUE
    )

    if (is.null(events) || nrow(events) == 0) {
      message(sprintf("  Empty events for %s", owner_repo))
      return(NULL)
    }

    # Extract relevant fields
    events_clean <- tibble(
      event_id = events$id,
      project = owner_repo,
      event_type = events$event,
      issue_number = events$issue$number,
      issue_title = events$issue$title,
      created_at = events$created_at,
      actor = events$actor$login
    )

    # Filter to recent events (last N days)
    cutoff <- Sys.time() - (since_days * 24 * 3600)
    events_clean$created_at <- as.POSIXct(events_clean$created_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    events_clean <- events_clean[events_clean$created_at >= cutoff, ]

    cat(sprintf("  Found %d events across %d page(s) (last %d days)\n", nrow(events_clean), page, since_days))
    events_clean
  }, error = function(e) {
    message(sprintf("  Error fetching %s: %s — using previous data for this repo", owner_repo, e$message))
    load_previous(owner_repo)
  })

  result
}

# Poll all projects
all_events <- lapply(projects, poll_issue_events) |>
  bind_rows()

if (nrow(all_events) == 0) {
  cat("No events found across all projects\n")
  all_events <- data.frame(
    event_id = integer(0),
    project = character(0),
    event_type = character(0),
    issue_number = integer(0),
    issue_title = character(0),
    created_at = character(0),
    actor = character(0)
  )
}

# Write to inst/extdata
pkg_root <- here::here()
out_path <- file.path(pkg_root, "inst", "extdata", "github_issue_events.json")
write_json(all_events, out_path, auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("\nWrote %d events to %s\n", nrow(all_events), out_path))
