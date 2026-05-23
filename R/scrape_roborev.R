# Roborev Scraper and Upsert Functions
# Scrapes roborev review findings and upserts into unified.duckdb

# Declare NSE variables used in dplyr pipelines
utils::globalVariables(c(
  "job_id", "content_hash", "scraped_at",
  "commit_sha", "reviewed_at", "agent"
))

# ---------------------------------------------------------------------------
# Internal helpers (not exported)
# ---------------------------------------------------------------------------

#' Resolve the roborev binary path
#'
#' Checks the `ROBOREV_BIN` environment variable first, then falls back to
#' `Sys.which("roborev")`. Aborts with a clear message if the binary cannot
#' be found.
#'
#' @return Character scalar — absolute path to the roborev binary.
#' @keywords internal
.roborev_bin <- function() {
  bin <- Sys.getenv("ROBOREV_BIN", unset = Sys.which("roborev"))
  if (!nzchar(bin) || !file.exists(bin)) {
    cli::cli_abort(
      c(
        "x" = "roborev binary not found.",
        "i" = "Install roborev or set {.envvar ROBOREV_BIN} to its path.",
        "i" = "See: {.url https://github.com/JohnGavin/roborev}"
      )
    )
  }
  bin
}

#' List roborev review jobs
#'
#' @param repo_path Path to the git repository
#' @param limit Maximum number of reviews to fetch
#' @return A data.frame with review job metadata from `roborev list --json`
#' @keywords internal
.roborev_list_jobs <- function(repo_path, limit = 500L) {
  roborev_bin <- .roborev_bin()

  result <- system2(
    roborev_bin,
    args = c("list", "--repo", repo_path, "--json", "--limit", as.character(limit)),
    stdout = TRUE,
    stderr = FALSE
  )

  if (length(result) == 0L || !nzchar(paste(result, collapse = ""))) {
    return(data.frame())
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(paste(result, collapse = "\n"), simplifyDataFrame = TRUE),
    error = function(e) {
      cli::cli_abort(
        c(
          "x" = "Failed to parse roborev list JSON output",
          "i" = "Error: {e$message}"
        )
      )
    }
  )

  if (is.null(parsed) || length(parsed) == 0L) {
    return(data.frame())
  }

  if (is.data.frame(parsed)) {
    return(parsed)
  }

  # Sometimes fromJSON returns a list of records
  if (is.list(parsed)) {
    return(dplyr::bind_rows(parsed))
  }

  data.frame()
}

#' Fetch and parse a single roborev review
#'
#' @param job_id Integer or character review job ID
#' @return A tibble of parsed findings, or NULL if the review failed to parse
#' @keywords internal
.roborev_show <- function(job_id) {
  roborev_bin <- .roborev_bin()

  result <- system2(
    roborev_bin,
    args = c("show", as.character(job_id)),
    stdout = TRUE,
    stderr = FALSE
  )

  if (length(result) == 0L) {
    cli::cli_warn(
      c(
        "!" = "roborev show {job_id} returned no output",
        "i" = "Skipping review job {job_id}"
      )
    )
    return(NULL)
  }

  markdown_text <- paste(result, collapse = "\n")
  .parse_roborev_show(markdown_text, job_id = as.integer(job_id))
}

#' Parse markdown output of `roborev show`
#'
#' Pure function, fully testable with fixture strings. Parses findings separated
#' by `---` horizontal rules and extracts severity, location, problem, and fix.
#'
#' @param markdown_text Character string — the full markdown output of `roborev show`
#' @param job_id Integer job ID (used for the returned tibble)
#' @return A tibble with columns matching the roborev findings schema, or an
#'   empty tibble if no valid findings are found
#' @keywords internal
.parse_roborev_show <- function(markdown_text, job_id = NA_integer_) {
  if (!nzchar(markdown_text)) {
    return(.empty_findings_tibble())
  }

  # Split on horizontal rule separators (--- or more dashes)
  blocks <- strsplit(markdown_text, "\n---+\n")[[1]]

  findings <- purrr::map(blocks, function(blk) {
    # Extract severity
    sev_match <- regmatches(blk, regexpr("\\*\\*Severity\\*\\*:\\s*([A-Za-z]+)", blk, perl = TRUE))
    if (length(sev_match) == 0L || !nzchar(sev_match)) {
      return(NULL)
    }
    severity_raw <- trimws(sub("\\*\\*Severity\\*\\*:\\s*", "", sev_match))
    severity <- tolower(severity_raw)

    # Extract location (optional — warn and skip field if missing, not the whole finding)
    loc_match <- regmatches(blk, regexpr("\\*\\*Location\\*\\*:\\s*(.+)", blk, perl = TRUE))
    if (length(loc_match) == 0L || !nzchar(loc_match)) {
      cli::cli_warn(
        c(
          "!" = "Finding in job {job_id} has no Location field",
          "i" = "Setting primary_file and full_location to NA"
        )
      )
      full_location <- NA_character_
      primary_file  <- NA_character_
    } else {
      full_location <- trimws(sub("\\*\\*Location\\*\\*:\\s*", "", loc_match))
      # Strip backticks from location
      full_location <- gsub("`", "", full_location)
      # Take first path segment (before any ; or , separator)
      first_path <- trimws(strsplit(full_location, "[;,]")[[1]][1])
      # Strip line-range suffix :NNN or :NNN-NNN
      primary_file <- sub(":\\d+(?:-\\d+)?$", "", first_path, perl = TRUE)
    }

    # Extract problem text — everything after **Problem**: up to next **Fix** or end
    prob_match <- regmatches(
      blk,
      regexpr("\\*\\*Problem\\*\\*:\\s*(.+?)(?=\\n\\s*-\\s*\\*\\*Fix\\*\\*|\\n\\s*##|$)",
              blk, perl = TRUE)
    )
    if (length(prob_match) == 0L || !nzchar(prob_match)) {
      return(NULL)
    }
    problem_text <- trimws(sub("\\*\\*Problem\\*\\*:\\s*", "", prob_match))

    # Extract fix text (optional)
    fix_match <- regmatches(
      blk,
      regexpr("\\*\\*Fix\\*\\*:\\s*(.+?)(?=\\n\\s*##|$)", blk, perl = TRUE)
    )
    fix_text <- if (length(fix_match) > 0L && nzchar(fix_match)) {
      trimws(sub("\\*\\*Fix\\*\\*:\\s*", "", fix_match))
    } else {
      NA_character_
    }

    tibble::tibble(
      severity      = severity,
      primary_file  = primary_file,
      full_location = full_location,
      problem_text  = problem_text,
      fix_text      = fix_text
    )
  })

  results <- purrr::keep(findings, Negate(is.null))

  if (length(results) == 0L) {
    return(.empty_findings_tibble())
  }

  dplyr::bind_rows(results)
}

#' Return an empty findings tibble with the correct schema
#' @keywords internal
.empty_findings_tibble <- function() {
  tibble::tibble(
    severity      = character(),
    primary_file  = character(),
    full_location = character(),
    problem_text  = character(),
    fix_text      = character()
  )
}

#' Compute a stable content hash for a finding
#'
#' Hash is derived from severity + primary_file + first 12 words of problem_text.
#' This allows deduplication across re-runs without an exact match requirement.
#'
#' @param severity Character severity string
#' @param primary_file Character file path
#' @param problem_text Character problem description
#' @return 32-character md5 hex string
#' @keywords internal
.finding_content_hash <- function(severity, primary_file, problem_text) {
  # Normalise: lowercase, NA -> ""
  sev  <- tolower(ifelse(is.na(severity), "", as.character(severity)))
  file <- ifelse(is.na(primary_file), "", as.character(primary_file))
  prob <- ifelse(is.na(problem_text), "", as.character(problem_text))

  # First 12 words of problem_text
  words <- strsplit(trimws(prob), "\\s+")[[1]]
  first_12 <- paste(head(words, 12L), collapse = " ")

  input_str <- paste(sev, file, first_12, sep = "\x1f")
  digest::digest(input_str, algo = "md5", serialize = FALSE)
}

# ---------------------------------------------------------------------------
# Exported functions
# ---------------------------------------------------------------------------

#' Scrape roborev review findings
#'
#' Calls `roborev list` to enumerate review jobs, then `roborev show` for each
#' job, and parses the markdown findings. Returns a tibble matching the
#' roborev findings interface contract consumed by downstream dashboard agents.
#'
#' @param repo_path Character path to the git repository root. Defaults to
#'   `here::here()`.
#' @param since_date Optional Date or character (ISO 8601). If provided, only
#'   reviews with `reviewed_at >= since_date` are included.
#' @return A tibble with columns:
#'   `job_id`, `project`, `branch`, `commit_sha`, `reviewed_at`,
#'   `severity`, `primary_file`, `full_location`, `problem_text`,
#'   `fix_text`, `agent`, `verdict`
#' @export
scrape_roborev <- function(repo_path = here::here(), since_date = NULL) {
  checkmate::assert_directory_exists(repo_path)
  if (!is.null(since_date)) {
    since_date <- as.Date(since_date)
    checkmate::assert_date(since_date, len = 1L)
  }

  # Binary resolution is handled inside .roborev_list_jobs()/.roborev_show()
  # via .roborev_bin(). Check early here so scrape_roborev() fails fast with a
  # clear message rather than midway through a long job list.
  .roborev_bin()

  # 1. List review jobs
  jobs_df <- .roborev_list_jobs(repo_path, limit = 500L)

  if (is.null(jobs_df) || nrow(jobs_df) == 0L) {
    cli::cli_inform("No roborev review jobs found for {.path {repo_path}}")
    return(.empty_scrape_tibble())
  }

  # Normalise column names to lower-case
  names(jobs_df) <- tolower(names(jobs_df))

  # Determine reviewed_at column (may be reviewed_at, created_at, timestamp, etc.)
  time_col <- intersect(c("reviewed_at", "created_at", "timestamp"), names(jobs_df))[1]
  if (is.na(time_col)) {
    time_col <- NULL
  }

  # Filter by since_date if requested
  if (!is.null(since_date) && !is.null(time_col)) {
    ts_vals <- as.POSIXct(jobs_df[[time_col]], tz = "UTC")
    jobs_df <- jobs_df[!is.na(ts_vals) & as.Date(ts_vals) >= since_date, , drop = FALSE]
  }

  if (nrow(jobs_df) == 0L) {
    cli::cli_inform("No reviews found after {since_date}")
    return(.empty_scrape_tibble())
  }

  # 2. For each job, call roborev show and parse findings
  all_findings <- purrr::map(seq_len(nrow(jobs_df)), function(i) {
    row <- jobs_df[i, , drop = FALSE]
    jid <- as.integer(row[["id"]])

    findings <- tryCatch(
      .roborev_show(jid),
      error = function(e) {
        cli::cli_warn(
          c(
            "!" = "Failed to parse review job {jid}: {e$message}",
            "i" = "Skipping this review"
          )
        )
        NULL
      }
    )

    if (is.null(findings) || nrow(findings) == 0L) {
      return(NULL)
    }

    # Attach job-level metadata
    project_val    <- if ("project" %in% names(row))    as.character(row[["project"]])    else NA_character_
    branch_val     <- if ("branch" %in% names(row))     as.character(row[["branch"]])     else NA_character_
    commit_sha_val <- if ("commit_sha" %in% names(row)) as.character(row[["commit_sha"]]) else NA_character_
    agent_val      <- if ("agent" %in% names(row))      as.character(row[["agent"]])      else NA_character_
    verdict_val    <- if ("verdict" %in% names(row))    as.character(row[["verdict"]])    else "unknown"
    # reviewed_at is NOT NULL in roborev_reviews; use Sys.time() when the job
    # list lacks a recognisable timestamp column so the upsert never sends NA.
    reviewed_at_val <- if (!is.null(time_col) && time_col %in% names(row)) {
      val <- as.POSIXct(row[[time_col]], tz = "UTC")
      if (is.na(val)) Sys.time() else val
    } else {
      Sys.time()
    }

    findings |>
      dplyr::mutate(
        job_id      = jid,
        project     = project_val,
        branch      = branch_val,
        commit_sha  = commit_sha_val,
        reviewed_at = reviewed_at_val,
        agent       = agent_val,
        verdict     = verdict_val
      )
  })

  results <- purrr::keep(all_findings, Negate(is.null))

  if (length(results) == 0L) {
    return(.empty_scrape_tibble())
  }

  out <- dplyr::bind_rows(results)

  # Enforce schema: severity and verdict as factors
  valid_severities <- c("critical", "high", "medium", "low", "info", "unknown")
  valid_verdicts   <- c("pass", "fail", "unknown")

  out <- out |>
    dplyr::mutate(
      severity = factor(
        dplyr::if_else(severity %in% valid_severities, severity, "unknown"),
        levels = valid_severities
      ),
      verdict = factor(
        dplyr::if_else(verdict %in% valid_verdicts, verdict, "unknown"),
        levels = valid_verdicts
      )
    ) |>
    dplyr::select(
      job_id, project, branch, commit_sha, reviewed_at,
      severity, primary_file, full_location, problem_text, fix_text,
      agent, verdict
    )

  out
}

#' Return an empty scrape tibble with the correct interface contract schema
#' @keywords internal
.empty_scrape_tibble <- function() {
  tibble::tibble(
    job_id        = integer(),
    project       = character(),
    branch        = character(),
    commit_sha    = character(),
    reviewed_at   = as.POSIXct(character(), tz = "UTC"),
    severity      = factor(levels = c("critical", "high", "medium", "low", "info", "unknown")),
    primary_file  = character(),
    full_location = character(),
    problem_text  = character(),
    fix_text      = character(),
    agent         = character(),
    verdict       = factor(levels = c("pass", "fail", "unknown"))
  )
}

#' Upsert roborev findings into unified.duckdb
#'
#' Opens the DuckDB at `db_path`, inserts new review records into
#' `roborev_reviews` and new finding records into `roborev_findings`
#' (idempotent — duplicates by `(job_id, content_hash)` are skipped),
#' and logs the run in `roborev_snapshot_log`.
#'
#' @param findings_df Tibble returned by [scrape_roborev()].
#' @param db_path Character path to the DuckDB database file.
#' @return A tibble with summary columns `reviews_seen`, `new_findings`,
#'   `new_reviews`.
#' @export
upsert_roborev_findings <- function(findings_df, db_path) {
  checkmate::assert_data_frame(findings_df)
  checkmate::assert_string(db_path)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Ensure schema exists
  .migrate_roborev_tables(con)

  started_at <- Sys.time()

  if (nrow(findings_df) == 0L) {
    run_summary <- tibble::tibble(
      reviews_seen  = 0L,
      new_findings  = 0L,
      new_reviews   = 0L
    )
    .log_snapshot_run(con, started_at = started_at, ended_at = Sys.time(),
                      reviews_seen = 0L, new_findings = 0L, loops_active = 0L,
                      status = "ok")
    return(run_summary)
  }

  # Compute content hashes for all findings
  findings_df <- findings_df |>
    dplyr::mutate(
      content_hash = purrr::pmap_chr(
        list(
          severity     = as.character(severity),
          primary_file = primary_file,
          problem_text = problem_text
        ),
        .finding_content_hash
      )
    )

  # --- Upsert reviews ---
  unique_reviews <- findings_df |>
    dplyr::distinct(job_id, project, branch, commit_sha, reviewed_at, agent, verdict)

  # Fetch existing job_ids
  existing_job_ids <- DBI::dbGetQuery(con, "SELECT job_id FROM roborev_reviews")$job_id

  new_reviews_df <- unique_reviews |>
    dplyr::filter(!(job_id %in% existing_job_ids))

  new_review_count <- nrow(new_reviews_df)

  if (new_review_count > 0L) {
    # Build raw_show stubs (we don't have the raw show here, just NA)
    reviews_to_insert <- new_reviews_df |>
      dplyr::mutate(
        raw_show   = NA_character_,
        scraped_at = Sys.time(),
        verdict    = as.character(verdict)
      )
    DBI::dbAppendTable(con, "roborev_reviews", reviews_to_insert)
  }

  # For existing reviews, update scraped_at
  if (length(existing_job_ids) > 0L) {
    existing_to_update <- unique_reviews |>
      dplyr::filter(job_id %in% existing_job_ids)
    if (nrow(existing_to_update) > 0L) {
      for (jid in existing_to_update$job_id) {
        DBI::dbExecute(
          con,
          "UPDATE roborev_reviews SET scraped_at = CURRENT_TIMESTAMP WHERE job_id = ?",
          list(jid)
        )
      }
    }
  }

  # --- Upsert findings ---
  existing_hashes_df <- DBI::dbGetQuery(
    con,
    "SELECT job_id, content_hash FROM roborev_findings"
  )

  new_findings_df <- findings_df |>
    dplyr::anti_join(
      existing_hashes_df,
      by = c("job_id", "content_hash")
    )

  new_finding_count <- nrow(new_findings_df)

  if (new_finding_count > 0L) {
    findings_to_insert <- new_findings_df |>
      dplyr::mutate(
        finding_id = purrr::map_int(
          seq_len(dplyr::n()),
          ~ as.integer(DBI::dbGetQuery(con, "SELECT nextval('roborev_finding_seq') AS id")$id)
        ),
        severity   = as.character(severity),
        scraped_at = Sys.time()
      ) |>
      dplyr::select(
        finding_id, job_id, severity, primary_file, full_location,
        problem_text, fix_text, content_hash, scraped_at
      )
    DBI::dbAppendTable(con, "roborev_findings", findings_to_insert)
  }

  # --- Log snapshot run ---
  .log_snapshot_run(
    con,
    started_at   = started_at,
    ended_at     = Sys.time(),
    reviews_seen = dplyr::n_distinct(findings_df$job_id),
    new_findings = new_finding_count,
    loops_active = 0L,
    status       = "ok"
  )

  tibble::tibble(
    reviews_seen  = dplyr::n_distinct(findings_df$job_id),
    new_findings  = new_finding_count,
    new_reviews   = new_review_count
  )
}

#' Migrate roborev tables into a DuckDB connection
#'
#' Creates all four roborev tables and sequences if they do not already exist.
#' Safe to call multiple times (idempotent).
#'
#' @param con An open DBI connection to DuckDB.
#' @return Invisibly NULL.
#' @keywords internal
.migrate_roborev_tables <- function(con) {
  statements <- c(
    "CREATE SEQUENCE IF NOT EXISTS roborev_finding_seq",
    "CREATE SEQUENCE IF NOT EXISTS roborev_snapshot_seq",

    "CREATE TABLE IF NOT EXISTS roborev_reviews (
      job_id       INTEGER PRIMARY KEY,
      project      VARCHAR NOT NULL DEFAULT '',
      branch       VARCHAR,
      commit_sha   VARCHAR NOT NULL DEFAULT '',
      reviewed_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      agent        VARCHAR,
      verdict      VARCHAR,
      raw_show     VARCHAR,
      scraped_at   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )",

    "CREATE TABLE IF NOT EXISTS roborev_findings (
      finding_id    INTEGER PRIMARY KEY,
      job_id        INTEGER NOT NULL,
      severity      VARCHAR NOT NULL,
      primary_file  VARCHAR,
      full_location VARCHAR,
      problem_text  VARCHAR,
      fix_text      VARCHAR,
      content_hash  VARCHAR NOT NULL,
      scraped_at    TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )",

    "CREATE TABLE IF NOT EXISTS roborev_loops (
      content_hash         VARCHAR PRIMARY KEY,
      severity             VARCHAR,
      primary_file         VARCHAR,
      summary              VARCHAR,
      first_seen           TIMESTAMP,
      last_seen            TIMESTAMP,
      cycles               INTEGER,
      tier                 VARCHAR,
      fix_commit_shas      VARCHAR,
      estimated_wasted_usd DOUBLE,
      ack_by               VARCHAR,
      ack_at               TIMESTAMP,
      ack_reason           VARCHAR,
      ack_until            TIMESTAMP,
      updated_at           TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )",

    "CREATE TABLE IF NOT EXISTS roborev_snapshot_log (
      run_id       INTEGER PRIMARY KEY,
      started_at   TIMESTAMP NOT NULL,
      ended_at     TIMESTAMP,
      reviews_seen INTEGER,
      new_findings INTEGER,
      loops_active INTEGER,
      status       VARCHAR
    )"
  )

  for (stmt in statements) {
    DBI::dbExecute(con, stmt)
  }

  invisible(NULL)
}

#' Log a snapshot run in roborev_snapshot_log
#' @keywords internal
.log_snapshot_run <- function(con, started_at, ended_at, reviews_seen,
                               new_findings, loops_active, status) {
  run_id <- as.integer(
    DBI::dbGetQuery(con, "SELECT nextval('roborev_snapshot_seq') AS id")$id
  )

  DBI::dbAppendTable(
    con,
    "roborev_snapshot_log",
    data.frame(
      run_id       = run_id,
      started_at   = started_at,
      ended_at     = ended_at,
      reviews_seen = as.integer(reviews_seen),
      new_findings = as.integer(new_findings),
      loops_active = as.integer(loops_active),
      status       = as.character(status),
      stringsAsFactors = FALSE
    )
  )

  invisible(NULL)
}

#' Transform roborev_raw into the roborev_findings vignette contract schema
#'
#' Converts the output of [scrape_roborev()] into the tibble expected by
#' [plan_roborev_vignette()]. Column mapping:
#'
#' | raw column    | findings column | notes                           |
#' |---------------|-----------------|----------------------------------|
#' | `job_id`      | `review_id`     |                                  |
#' | `commit_sha`  | `review_commit` |                                  |
#' | `severity`    | `severity`      | coerced to character             |
#' | `primary_file`| `primary_file`  |                                  |
#' | `problem_text`| `problem_text`  |                                  |
#' | `problem_text`| `summary`       | first 80 characters              |
#' | `reviewed_at` | `found_at`      |                                  |
#' | —             | `resolved_at`   | `NA` (not tracked at scrape time)|
#' | —             | `fix_commit`    | `NA` (not tracked at scrape time)|
#' | `project`     | `project`       |                                  |
#' | `agent`       | `agent_id`      |                                  |
#' | —             | `finding_id`    | sequential integer               |
#'
#' Returns an empty tibble with the correct schema when `raw_df` has zero rows,
#' so downstream targets degrade gracefully when roborev is unavailable.
#'
#' @param raw_df Tibble returned by [scrape_roborev()].
#' @return A tibble conforming to the `roborev_findings` vignette contract.
#' @keywords internal
.roborev_raw_to_findings <- function(raw_df) {
  checkmate::assert_data_frame(raw_df)

  empty_schema <- tibble::tibble(
    finding_id   = integer(),
    review_id    = integer(),
    review_commit = character(),
    severity     = character(),
    primary_file = character(),
    problem_text = character(),
    summary      = character(),
    found_at     = as.POSIXct(character(), tz = "UTC"),
    resolved_at  = as.POSIXct(character(), tz = "UTC"),
    fix_commit   = character(),
    project      = character(),
    agent_id     = character()
  )

  if (nrow(raw_df) == 0L) {
    return(empty_schema)
  }

  result <- raw_df |>
    dplyr::mutate(
      finding_id    = seq_len(dplyr::n()),
      review_id     = as.integer(job_id),
      review_commit = as.character(commit_sha),
      severity      = as.character(severity),
      summary       = substr(problem_text, 1L, 80L),
      found_at      = reviewed_at,
      resolved_at   = as.POSIXct(NA_real_, tz = "UTC"),
      fix_commit    = NA_character_,
      agent_id      = as.character(agent)
    ) |>
    dplyr::select(
      finding_id, review_id, review_commit, severity, primary_file,
      problem_text, summary, found_at, resolved_at, fix_commit,
      project, agent_id
    )

  result
}

#' Migrate unified.duckdb — create roborev tables idempotently
#'
#' Opens the DuckDB at `db_path` and creates all four roborev tables plus
#' sequences if they do not already exist. Safe to call multiple times.
#'
#' @param db_path Character path to the DuckDB file. Defaults to
#'   `Sys.getenv("LLMTELEMETRY_UNIFIED_DB", "~/.claude/logs/unified.duckdb")`.
#' @return Invisibly NULL.
#' @export
migrate_unified_db <- function(
    db_path = Sys.getenv(
      "LLMTELEMETRY_UNIFIED_DB",
      path.expand("~/.claude/logs/unified.duckdb")
    )
) {
  checkmate::assert_string(db_path)

  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  .migrate_roborev_tables(con)

  cli::cli_inform("Roborev tables migrated in {.path {db_path}}")
  invisible(NULL)
}
