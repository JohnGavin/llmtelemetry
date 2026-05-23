# roborev_page_fixture.R
# Synthetic fixture data for roborev vignette and plot tests.
# Emits roborev_findings and roborev_loops with the shapes defined in issue #144.
# The 5 canonical loops (rotated-log replay, incremental-merge replacement,
# sanitize-narrow, CI-fallback, irishbuoys-underscore) are seeded deterministically.

make_roborev_findings <- function(n = 50L) {
  set.seed(42L)
  severities <- c("critical", "high", "medium", "low", "info")
  files <- c(
    "R/canonicalize.R", "R/rollup_sessions.R", "R/read_unified.R",
    "R/rollup_costs.R", "R/ccusage.R", "inst/scripts/send_daily_email.R",
    "inst/scripts/export_dashboard_data.R", "vignettes/dashboard_shinylive.qmd",
    "tests/testthat/test-schema.R", "R/schema.R"
  )
  base_date <- as.POSIXct("2026-04-01 00:00:00", tz = "UTC")
  found_at <- base_date + sample(seq(0, 50 * 86400, by = 3600), n, replace = TRUE)
  resolved_at <- ifelse(
    runif(n) > 0.4,
    as.character(found_at + sample(seq(3600, 7 * 86400), n, replace = TRUE)),
    NA_character_
  )
  tibble::tibble(
    finding_id     = sprintf("F%04d", seq_len(n)),
    review_id      = sprintf("R%04d", sample(1:20, n, replace = TRUE)),
    review_commit  = paste0(
      "abc",
      sprintf("%05d", sample(1:99999, n, replace = TRUE))
    ),
    severity       = sample(severities, n, replace = TRUE,
                            prob = c(0.05, 0.15, 0.35, 0.30, 0.15)),
    primary_file   = sample(files, n, replace = TRUE),
    problem_text   = paste0("Issue in function at line ",
                            sample(1:500, n, replace = TRUE)),
    summary        = paste0("Fix needed: ", sample(
      c("missing NA check", "stop() usage", "T/F shorthand",
        "missing @export", "hardcoded path"), n, replace = TRUE
    )),
    found_at       = found_at,
    resolved_at    = as.POSIXct(resolved_at, tz = "UTC", origin = "1970-01-01"),
    fix_commit     = ifelse(
      !is.na(resolved_at),
      paste0("fix", sprintf("%05d", sample(1:99999, n, replace = TRUE))),
      NA_character_
    ),
    project        = sample(
      c("llmtelemetry", "llm", "irishbuoys"), n, replace = TRUE
    ),
    agent_id       = paste0("agent-", sprintf("%08x", sample.int(.Machine$integer.max, n)))
  )
}

make_roborev_loops <- function() {
  tibble::tibble(
    content_hash = c(
      "aabbcc112233",
      "ddeeff445566",
      "112233aabbcc",
      "778899ddeeff",
      "aabbccddeeff"
    ),
    severity = c("high", "medium", "critical", "low", "high"),
    primary_file = c(
      "inst/scripts/send_daily_email.R",
      "R/rollup_sessions.R",
      "R/canonicalize.R",
      ".github/workflows/daily-llm-report.yaml",
      "R/read_git_pulse.R"
    ),
    summary = c(
      "Rotated-log replay: duplicate session rows on log rotation",
      "Incremental-merge replacement: full table scan on merge",
      "Sanitize regex too narrow: leaks file system paths",
      "CI fallback triggers daily even when primary succeeds",
      "irishbuoys underscore: column name mismatch breaks join"
    ),
    first_seen = as.POSIXct(c(
      "2026-04-01", "2026-04-03", "2026-04-05", "2026-04-07", "2026-04-10"
    ), tz = "UTC"),
    last_seen = as.POSIXct(c(
      "2026-05-15", "2026-05-10", "2026-05-18", "2026-05-01", "2026-05-12"
    ), tz = "UTC"),
    cycles = c(8L, 5L, 12L, 3L, 6L),
    tier = c("block", "watch", "escalate", "watch", "block"),
    fix_commit_shas = list(
      c("fix00001", "fix00002"),
      c("fix00003"),
      character(0),
      c("fix00004"),
      c("fix00005", "fix00006")
    ),
    estimated_wasted_usd = c(1.24, 0.67, 3.10, 0.18, 0.95),
    ack_by = c("john", NA_character_, NA_character_, "john", NA_character_),
    ack_at = as.POSIXct(c(
      "2026-05-01", NA, NA, "2026-04-20", NA
    ), tz = "UTC"),
    ack_reason = c(
      "Known issue, scheduled for sprint 3",
      NA_character_,
      NA_character_,
      "Accepted as low priority",
      NA_character_
    ),
    ack_until = as.POSIXct(c(
      "2026-06-01", NA, NA, "2026-06-30", NA
    ), tz = "UTC")
  )
}
