#!/usr/bin/env Rscript
# escalate_stuck_loops.R
# Standalone CLI: reads roborev_loops from unified.duckdb, finds Tier 3
# (escalate) loops with no acknowledgement, and files or updates GitHub issues.
#
# Idempotency: each loop gets a deterministic idempotency key
#   "STUCK_LOOP_<12-char prefix of content_hash>"
# which is embedded in the issue title. Existing open issues with that key
# are PATCH-updated; no duplicate issues are created.
#
# Authentication: requires GH_TOKEN environment variable with repo write access.
# Privacy: only `summary` (max 80 chars, no file system paths) appears in the
# email / issue body. Raw `problem_text` is NOT included (issue #140).
#
# Usage:
#   Rscript inst/scripts/escalate_stuck_loops.R [--dry-run] [--repo OWNER/REPO]

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(httr2)
  library(jsonlite)
  library(cli)
})

# ---- argument parsing --------------------------------------------------------
args     <- commandArgs(trailingOnly = TRUE)
dry_run  <- "--dry-run" %in% args
repo_arg <- {
  idx <- which(args == "--repo")
  if (length(idx) > 0 && length(args) >= idx + 1) args[idx + 1] else NULL
}

REPO <- repo_arg %||% Sys.getenv("GITHUB_REPOSITORY", unset = "JohnGavin/llmtelemetry")
DB_PATH <- Sys.getenv(
  "UNIFIED_DB_PATH",
  file.path(Sys.getenv("HOME"), ".claude", "logs", "unified.duckdb")
)

cli::cli_h1("escalate_stuck_loops")
cli::cli_alert_info("Repo: {REPO}")
cli::cli_alert_info("DB:   {DB_PATH}")
if (dry_run) cli::cli_alert_warning("DRY RUN — no GitHub API calls will be made")

# ---- helpers -----------------------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nchar(a) > 0) a else b

#' Sanitize a text string for inclusion in a GitHub issue body.
#' Strips file system paths and truncates to max_chars.
sanitize_for_issue <- function(text, max_chars = 80L) {
  if (is.null(text) || is.na(text)) return("")
  # Remove file system paths: anything that looks like /Users/.../ or ~/...
  text <- gsub("/Users/[^/]*/[^ \t\n\"']*", "<path>", text)
  text <- gsub("~[/\\\\][^ \t\n\"']*", "<path>", text)
  # Remove Windows-style paths
  text <- gsub("[A-Za-z]:[\\\\][^ \t\n\"']*", "<path>", text)
  substr(trimws(text), 1L, max_chars)
}

#' Build the idempotency key for a loop's content_hash.
loop_key <- function(content_hash) {
  paste0("STUCK_LOOP_", substr(content_hash, 1L, 12L))
}

#' Build the GitHub issue title for a stuck loop.
issue_title <- function(loop_row) {
  key     <- loop_key(loop_row$content_hash)
  summary <- sanitize_for_issue(loop_row$summary, max_chars = 60L)
  sprintf("[%s] %s", key, summary)
}

#' Build the GitHub issue body for a stuck loop.
issue_body <- function(loop_row, chain_df = NULL) {
  key          <- loop_key(loop_row$content_hash)
  summary      <- sanitize_for_issue(loop_row$summary, max_chars = 80L)
  cycles       <- as.integer(loop_row$cycles)
  tier         <- loop_row$tier
  severity     <- loop_row$severity
  first_seen   <- format(as.Date(loop_row$first_seen), "%Y-%m-%d")
  last_seen    <- format(as.Date(loop_row$last_seen),  "%Y-%m-%d")
  est_usd      <- sprintf("$%.2f", as.numeric(loop_row$estimated_wasted_usd))
  fix_shas     <- tryCatch(
    paste(unlist(loop_row$fix_commit_shas), collapse = ", "),
    error = function(e) ""
  )
  if (nchar(fix_shas) == 0) fix_shas <- "none"

  chain_table <- ""
  if (!is.null(chain_df) && nrow(chain_df) > 0) {
    rows <- vapply(seq_len(nrow(chain_df)), function(i) {
      r <- chain_df[i, ]
      sprintf(
        "| %s | %s | %s | %s | %s | %s |",
        r$review_id  %||% "—",
        r$review_commit %||% "—",
        r$fix_commit %||% "—",
        r$agent_id   %||% "—",
        sprintf("$%.4f", as.numeric(r$agent_cost %||% 0)),
        sanitize_for_issue(r$problem_text %||% "", max_chars = 60L)
      )
    }, character(1))
    chain_table <- paste0(
      "\n## Review chain\n\n",
      "| Review ID | Review commit | Fix commit | Agent | Cost | Problem |\n",
      "|---|---|---|---|---|---|\n",
      paste(rows, collapse = "\n"),
      "\n"
    )
  }

  sprintf(
    "## Stuck roborev loop — %s

**Idempotency key:** `%s`
**Summary:** %s
**Severity:** %s | **Tier:** %s
**Cycles:** %d | **First seen:** %s | **Last seen:** %s
**Fix commits attempted:** %s
**Estimated wasted USD:** %s
%s
## Acknowledge

To acknowledge this loop and prevent re-filing, run:

```bash
roborev ack %s --reason \"<your reason>\" --until <YYYY-MM-DD>
```

_Auto-filed by `escalate_stuck_loops.R`. Do NOT close manually — use the ack command._
",
    key,
    key,
    summary,
    severity, tier,
    cycles, first_seen, last_seen,
    fix_shas,
    est_usd,
    chain_table,
    loop_row$content_hash
  )
}

# ---- load data ---------------------------------------------------------------

cli::cli_h2("Loading roborev_loops from unified.duckdb")

if (!file.exists(DB_PATH)) {
  cli::cli_abort(c(
    "x" = "unified.duckdb not found at {.path {DB_PATH}}",
    "i" = "Set UNIFIED_DB_PATH env var or ensure the DB exists."
  ))
}

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = DB_PATH, read_only = TRUE)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

if (!"roborev_loops" %in% DBI::dbListTables(con)) {
  cli::cli_alert_info("Table 'roborev_loops' not yet present — nothing to escalate.")
  quit(status = 0)
}

loops <- DBI::dbReadTable(con, "roborev_loops") |> tibble::as_tibble()

# roborev_findings needed to build the review chain (best-effort)
findings <- if ("roborev_findings" %in% DBI::dbListTables(con)) {
  DBI::dbReadTable(con, "roborev_findings") |> tibble::as_tibble()
} else {
  NULL
}

stuck <- loops |>
  dplyr::filter(tier == "escalate", is.na(ack_by))

cli::cli_alert_info(
  "{nrow(stuck)} stuck loop(s) found (tier=escalate, ack_by IS NULL)"
)

if (nrow(stuck) == 0L) {
  cli::cli_alert_success("No stuck loops — nothing to escalate.")
  quit(status = 0)
}

# ---- GitHub API helpers -------------------------------------------------------

GH_TOKEN <- Sys.getenv("GH_TOKEN")
if (!dry_run && nchar(GH_TOKEN) == 0) {
  cli::cli_abort(c(
    "x" = "GH_TOKEN environment variable is not set.",
    "i" = "Export a GitHub PAT with repo write scope."
  ))
}

gh_req <- function(method, path, body = NULL) {
  url <- paste0("https://api.github.com", path)
  req <- httr2::request(url) |>
    httr2::req_method(method) |>
    httr2::req_headers(
      Accept        = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28"
    ) |>
    httr2::req_auth_bearer_token(GH_TOKEN)
  if (!is.null(body)) {
    req <- req |>
      httr2::req_body_json(body)
  }
  req
}

#' Find open GitHub issues for this repo whose title contains the idempotency key.
find_existing_issue <- function(key) {
  req <- gh_req("GET",
    sprintf("/search/issues?q=%s+in:title+repo:%s+is:issue+is:open",
            utils::URLencode(key, reserved = TRUE), REPO)
  )
  resp <- httr2::req_perform(req)
  data <- httr2::resp_body_json(resp)
  if (is.null(data$items) || length(data$items) == 0L) return(NULL)
  data$items[[1]]
}

#' Ensure required labels exist on the repo.
ensure_labels <- function(labels) {
  for (lbl in labels) {
    req_check <- gh_req("GET", sprintf("/repos/%s/labels/%s", REPO,
                                       utils::URLencode(lbl, reserved = TRUE)))
    resp_check <- tryCatch(
      httr2::req_perform(req_check),
      error = function(e) NULL
    )
    if (is.null(resp_check)) {
      cli::cli_alert_info("Creating missing label '{lbl}'")
      req_create <- gh_req("POST", sprintf("/repos/%s/labels", REPO),
                           body = list(name = lbl, color = "d93f0b"))
      tryCatch(httr2::req_perform(req_create),
               error = function(e) cli::cli_alert_warning("Could not create label '{lbl}': {e$message}"))
    }
  }
}

REQUIRED_LABELS <- c("bug", "roborev", "stuck-loop")

# ---- process each stuck loop -------------------------------------------------

results <- vector("list", nrow(stuck))

for (i in seq_len(nrow(stuck))) {
  loop_row <- stuck[i, ]
  key      <- loop_key(loop_row$content_hash)
  title    <- issue_title(loop_row)

  # Build chain table from roborev_findings if available
  chain_df <- if (!is.null(findings) && "review_id" %in% names(findings)) {
    # Match by primary_file and content_hash proximity (best-effort)
    findings[findings$primary_file == loop_row$primary_file, ]
  } else {
    NULL
  }

  body_text <- issue_body(loop_row, chain_df = chain_df)

  cli::cli_h3("Loop {i}/{nrow(stuck)}: {key}")
  cli::cli_text("Title: {title}")

  if (dry_run) {
    cli::cli_alert_info("[DRY RUN] Would file/update issue: {title}")
    results[[i]] <- list(key = key, action = "dry-run", issue_number = NA)
    next
  }

  # Check for existing open issue
  existing <- tryCatch(
    find_existing_issue(key),
    error = function(e) {
      cli::cli_alert_warning("Search failed for {key}: {e$message}")
      NULL
    }
  )

  if (!is.null(existing)) {
    # PATCH existing issue body
    issue_number <- existing$number
    req_patch <- gh_req(
      "PATCH",
      sprintf("/repos/%s/issues/%d", REPO, issue_number),
      body = list(body = body_text)
    )
    tryCatch({
      httr2::req_perform(req_patch)
      cli::cli_alert_success("Updated issue #{issue_number}: {title}")
      results[[i]] <- list(key = key, action = "updated", issue_number = issue_number)
    }, error = function(e) {
      cli::cli_alert_danger("Failed to update #{issue_number}: {e$message}")
      results[[i]] <<- list(key = key, action = "error", issue_number = issue_number)
    })
  } else {
    # Ensure labels exist before creating issue
    tryCatch(
      ensure_labels(REQUIRED_LABELS),
      error = function(e) cli::cli_alert_warning("Label check failed: {e$message}")
    )

    req_create <- gh_req(
      "POST",
      sprintf("/repos/%s/issues", REPO),
      body = list(
        title  = title,
        body   = body_text,
        labels = REQUIRED_LABELS
      )
    )
    tryCatch({
      resp <- httr2::req_perform(req_create)
      new_issue <- httr2::resp_body_json(resp)
      issue_number <- new_issue$number
      cli::cli_alert_success("Created issue #{issue_number}: {title}")
      results[[i]] <- list(key = key, action = "created", issue_number = issue_number)
    }, error = function(e) {
      cli::cli_alert_danger("Failed to create issue for {key}: {e$message}")
      results[[i]] <<- list(key = key, action = "error", issue_number = NA)
    })
  }
}

# ---- summary -----------------------------------------------------------------

cli::cli_h2("Summary")
n_created <- sum(vapply(results, function(r) identical(r$action, "created"), logical(1)))
n_updated <- sum(vapply(results, function(r) identical(r$action, "updated"), logical(1)))
n_errors  <- sum(vapply(results, function(r) identical(r$action, "error"),   logical(1)))

cli::cli_ul(c(
  "Created: {n_created}",
  "Updated: {n_updated}",
  "Errors:  {n_errors}",
  "Total:   {nrow(stuck)}"
))

if (n_errors > 0) {
  cli::cli_abort("Some issues failed to file — see warnings above.")
}
