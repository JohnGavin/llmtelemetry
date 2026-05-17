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

poll_issue_events <- function(owner_repo, since_days = 30,
                              output_path = NULL) {
  cat(sprintf("Polling %s...\n", owner_repo))

  # Use gh CLI via system2 for GitHub API access
  parts <- strsplit(owner_repo, "/")[[1]]
  owner <- parts[1]
  repo <- parts[2]

  # Helper: load previous output file contents so we can preserve them on
  # auth/rate-limit failure rather than overwriting with empty data (#774).
  load_previous <- function() {
    if (!is.null(output_path) && file.exists(output_path)) {
      tryCatch(
        fromJSON(output_path, simplifyDataFrame = TRUE),
        error = function(e) NULL
      )
    } else {
      NULL
    }
  }

  # Fetch issue events via gh api with pagination handled page-by-page to
  # avoid the multi-doc JSON problem caused by --paginate (#775).
  # Each page returns one valid JSON array; we collect all pages manually.
  result <- tryCatch({
    all_events_list <- list()
    page <- 1L
    repeat {
      events_json <- system2(
        "gh",
        args = c(
          "api",
          sprintf("/repos/%s/%s/issues/events?per_page=100&page=%d",
                  owner, repo, page)
        ),
        stdout = TRUE,
        stderr = FALSE
      )

      # Check exit status: non-zero means auth failure or rate-limit (#774).
      exit_status <- attr(events_json, "status")
      if (!is.null(exit_status) && exit_status != 0L) {
        warning(sprintf(
          "  gh api exited with status %d for %s (page %d) — preserving previous data",
          exit_status, owner_repo, page
        ))
        return(load_previous())
      }

      if (length(events_json) == 0 || identical(events_json, "")) {
        break
      }

      page_text <- paste(events_json, collapse = "")
      if (!nzchar(trimws(page_text)) || page_text == "[]") break

      page_events <- tryCatch(
        fromJSON(page_text, simplifyDataFrame = TRUE),
        error = function(e) {
          warning(sprintf("  JSON parse error for %s page %d: %s",
                          owner_repo, page, conditionMessage(e)))
          NULL
        }
      )
      if (is.null(page_events) || !is.data.frame(page_events) ||
          nrow(page_events) == 0L) break

      all_events_list[[page]] <- page_events

      # If this page was a full page, fetch the next; otherwise we're done.
      if (nrow(page_events) < 100L) break
      page <- page + 1L
    }

    if (length(all_events_list) == 0L) {
      message(sprintf("  No events found for %s", owner_repo))
      return(NULL)
    }

    events <- bind_rows(all_events_list)

    if (nrow(events) == 0L) {
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
    events_clean$created_at <- as.POSIXct(events_clean$created_at,
                                           format = "%Y-%m-%dT%H:%M:%SZ",
                                           tz = "UTC")
    events_clean <- events_clean[events_clean$created_at >= cutoff, ]

    cat(sprintf("  Found %d events (last %d days)\n", nrow(events_clean), since_days))
    events_clean
  }, error = function(e) {
    message(sprintf("  Error fetching %s: %s", owner_repo, e$message))
    NULL
  })

  result
}

# Write to inst/extdata
pkg_root <- here::here()
out_path <- file.path(pkg_root, "inst", "extdata", "github_issue_events.json")

# Poll all projects; pass out_path so each poller can preserve previous data
# on auth or rate-limit failure rather than returning NULL (#774).
all_events <- lapply(projects, poll_issue_events,
                     output_path = out_path) |>
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

write_json(all_events, out_path, auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("\nWrote %d events to %s\n", nrow(all_events), out_path))
