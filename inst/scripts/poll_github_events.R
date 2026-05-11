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

poll_issue_events <- function(owner_repo, since_days = 30) {
  cat(sprintf("Polling %s...\n", owner_repo))

  # Use gh CLI via system2 for GitHub API access
  parts <- strsplit(owner_repo, "/")[[1]]
  owner <- parts[1]
  repo <- parts[2]

  # Fetch issue events via gh api (no pagination to avoid JSON concat issues)
  result <- tryCatch({
    events_json <- system2(
      "gh",
      args = c(
        "api",
        sprintf("/repos/%s/%s/issues/events?per_page=100", owner, repo)
      ),
      stdout = TRUE,
      stderr = FALSE
    )

    if (length(events_json) == 0 || events_json[1] == "") {
      message(sprintf("  No events found for %s", owner_repo))
      return(NULL)
    }

    events <- fromJSON(paste(events_json, collapse = ""), simplifyDataFrame = TRUE)

    if (nrow(events) == 0) {
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

    cat(sprintf("  Found %d events (last %d days)\n", nrow(events_clean), since_days))
    events_clean
  }, error = function(e) {
    message(sprintf("  Error fetching %s: %s", owner_repo, e$message))
    NULL
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
