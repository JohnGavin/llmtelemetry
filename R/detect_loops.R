# Loop detection for roborev issue patterns (Phase 2 of #144).
#
# A "loop" is when the same (or near-identical) roborev finding recurs across
# multiple review cycles, indicating that previous fixer attempts did not
# permanently resolve it.  This module detects such loops and assigns a
# tier (watch / block / escalate) based on cycle count.

# ---- internal helpers --------------------------------------------------------

#' Extract first N words from a character string
#'
#' @param text Character scalar.
#' @param n Integer number of words to keep (default 12).
#' @return Character scalar.
#' @keywords internal
.first_n_words <- function(text, n = 12L) {
  words <- strsplit(trimws(text), "\\s+")[[1]]
  paste(words[seq_len(min(n, length(words)))], collapse = " ")
}

#' Tokenise text for Jaccard similarity
#'
#' Splits on whitespace and punctuation, lowercases, and drops a small set of
#' English stopwords.
#'
#' @param text Character scalar.
#' @return Character vector of tokens.
#' @keywords internal
.tokenize <- function(text) {
  stopwords <- c("a", "an", "the", "of", "in", "to", "for", "with", "is",
                 "it", "at", "by", "on", "as", "or", "be", "has", "was",
                 "are", "that", "this", "and", "but", "not", "from", "have")
  raw <- tolower(text)
  tokens <- unlist(strsplit(raw, "[[:space:][:punct:]]+"))
  tokens <- tokens[nzchar(tokens)]
  tokens[!tokens %in% stopwords]
}

#' Jaccard similarity between two texts' token sets
#'
#' @param a Character scalar.
#' @param b Character scalar.
#' @return Numeric in the range \[0, 1\].
#' @keywords internal
.token_jaccard <- function(a, b) {
  ta <- unique(.tokenize(a))
  tb <- unique(.tokenize(b))
  if (length(ta) == 0L && length(tb) == 0L) return(1.0)
  if (length(ta) == 0L || length(tb) == 0L) return(0.0)
  n_inter <- length(intersect(ta, tb))
  n_union  <- length(union(ta, tb))
  n_inter / n_union
}

#' Assign loop tier based on cycle count
#'
#' @param cycles Integer.
#' @return Character: "watch", "block", or "escalate".
#' @keywords internal
.assign_tier <- function(cycles) {
  dplyr::case_when(
    cycles >= 10L ~ "escalate",
    cycles >= 5L  ~ "block",
    cycles >= 3L  ~ "watch",
    TRUE          ~ NA_character_
  )
}

# ---- exported functions ------------------------------------------------------

#' Detect recurring roborev finding loops
#'
#' Groups `findings_df` by `content_hash` (computed from severity + file +
#' first 12 words of problem text) and counts "cycles" — pairs of consecutive
#' occurrences where a matching fix commit appears in `agent_runs_df` between
#' the two reviews.  Findings with different hashes but high Jaccard similarity
#' on the same file are collapsed to a canonical hash before counting.
#'
#' Only groups with ≥ 3 cycles are returned.  Tiers:
#'   - `watch`:    3–4 cycles
#'   - `block`:    5–9 cycles
#'   - `escalate`: ≥ 10 cycles
#'
#' @param findings_df A tibble with columns matching the `roborev_findings`
#'   schema (see issue #144).  Required: `reviewed_at` (POSIXct), `severity`
#'   (chr/factor), `primary_file` (chr), `problem_text` (chr), `commit_sha`
#'   (chr), `branch` (chr).
#' @param agent_runs_df Optional tibble of agent runs used to detect fix
#'   commits.  Columns used: `agent_type` (chr), `commit_sha` (chr),
#'   `started_at` (POSIXct), `total_cost_usd` (numeric).  If `NULL`, cycles
#'   are still counted (any consecutive re-occurrence counts as a cycle), but
#'   `estimated_wasted_usd` is `NA`.
#'
#'   NOTE: The interface contract specifies `session_id` and other columns on
#'   `agent_runs`; only the columns listed above are consumed.  If the
#'   production schema changes, extend via `dplyr::any_of()` below.
#' @param jaccard_threshold Numeric in the range (0, 1\].  Findings on the same file
#'   with Jaccard ≥ this value are treated as the same content.  Default 0.6.
#' @param window_days Integer.  Only findings within this many days of the
#'   most recent finding are included.  Default 30.
#' @return A tibble with the `roborev_loops` schema.
#' @export
detect_loops <- function(
  findings_df,
  agent_runs_df    = NULL,
  jaccard_threshold = 0.6,
  window_days       = 30L
) {
  # --- empty input guard -------------------------------------------------------
  empty_result <- tibble::tibble(
    content_hash         = character(),
    severity             = character(),
    primary_file         = character(),
    summary              = character(),
    first_seen           = as.POSIXct(character()),
    last_seen            = as.POSIXct(character()),
    cycles               = integer(),
    tier                 = character(),
    fix_commit_shas      = character(),
    estimated_wasted_usd = numeric(),
    ack_by               = character(),
    ack_at               = as.POSIXct(character()),
    ack_reason           = character(),
    ack_until            = as.POSIXct(character())
  )

  if (is.null(findings_df) || nrow(findings_df) == 0L) {
    return(empty_result)
  }

  # --- coerce types --------------------------------------------------------
  df <- findings_df |>
    dplyr::mutate(
      severity     = as.character(severity),
      primary_file = as.character(primary_file),
      problem_text = as.character(problem_text),
      commit_sha   = as.character(commit_sha),
      reviewed_at  = as.POSIXct(reviewed_at)
    )

  # --- window filter -------------------------------------------------------
  cutoff <- max(df$reviewed_at, na.rm = TRUE) - as.difftime(window_days, units = "days")
  df <- df[df$reviewed_at >= cutoff, , drop = FALSE]
  if (nrow(df) == 0L) return(empty_result)

  # --- Step 1: compute content_hash per finding ----------------------------
  df <- df |>
    dplyr::mutate(
      content_hash = vapply(
        seq_len(nrow(df)),
        function(i) {
          digest::digest(
            paste(severity[i], primary_file[i],
                  .first_n_words(problem_text[i], 12L)),
            algo = "md5", serialize = FALSE
          )
        },
        character(1L)
      )
    )

  # --- Step 3 (pre): Jaccard collapse within same (severity, primary_file) -
  # For each group, merge hashes with pairwise Jaccard >= threshold.
  df <- .jaccard_collapse(df, jaccard_threshold)

  # --- Step 2: count cycles per content_hash --------------------------------
  fix_agent_types <- c("fixer", "quick-fix", "r-debugger", "claude-code")

  loop_rows <- purrr::map_dfr(
    split(df, df$content_hash),
    function(grp) {
      grp <- grp[order(grp$reviewed_at), ]
      n <- nrow(grp)
      if (n < 2L) return(NULL)

      cycles_count <- 0L
      all_fix_shas <- character()
      total_cost   <- 0.0

      for (i in seq_len(n - 1L)) {
        f_i   <- grp[i, ]
        f_ip1 <- grp[i + 1L, ]

        # Look for a fix commit between these two findings
        if (!is.null(agent_runs_df) && nrow(agent_runs_df) > 0L) {
          runs <- agent_runs_df |>
            dplyr::filter(
              .data$agent_type %in% fix_agent_types,
              !is.na(.data$started_at),
              .data$started_at >= f_i$reviewed_at,
              .data$started_at <= f_ip1$reviewed_at
            )

          if (!is.null(runs) && nrow(runs) > 0L) {
            cycles_count <- cycles_count + 1L
            fix_shas <- runs$commit_sha[!is.na(runs$commit_sha)]
            all_fix_shas <- c(all_fix_shas, fix_shas)
            if ("total_cost_usd" %in% names(runs)) {
              total_cost <- total_cost + sum(runs$total_cost_usd, na.rm = TRUE)
            }
          }
        } else {
          # No agent_runs: any consecutive pair counts as a cycle
          cycles_count <- cycles_count + 1L
        }
      }

      if (cycles_count < 3L) return(NULL)

      tier <- .assign_tier(cycles_count)
      if (is.na(tier)) return(NULL)

      tibble::tibble(
        content_hash         = grp$content_hash[1L],
        severity             = grp$severity[1L],
        primary_file         = grp$primary_file[1L],
        summary              = substr(grp$problem_text[1L], 1L, 80L),
        first_seen           = min(grp$reviewed_at),
        last_seen            = max(grp$reviewed_at),
        cycles               = cycles_count,
        tier                 = tier,
        fix_commit_shas      = paste(unique(all_fix_shas), collapse = ","),
        estimated_wasted_usd = if (is.null(agent_runs_df)) NA_real_ else total_cost,
        ack_by               = NA_character_,
        ack_at               = as.POSIXct(NA),
        ack_reason           = NA_character_,
        ack_until            = as.POSIXct(NA)
      )
    }
  )

  if (is.null(loop_rows) || nrow(loop_rows) == 0L) return(empty_result)
  loop_rows
}

#' Jaccard-based hash collapse within (severity, primary_file) groups
#'
#' Within each `(severity, primary_file)` group, if two findings have
#' different `content_hash` values but their `problem_text` token sets have
#' Jaccard ≥ `threshold`, the later hash is replaced by the first-seen hash
#' (canonical).
#'
#' @param df Data frame with columns `severity`, `primary_file`,
#'   `problem_text`, `content_hash`, `reviewed_at`.
#' @param threshold Numeric threshold.
#' @return `df` with `content_hash` updated.
#' @keywords internal
.jaccard_collapse <- function(df, threshold = 0.6) {
  groups <- split(df, paste(df$severity, df$primary_file, sep = "|||"))

  result <- purrr::map_dfr(groups, function(grp) {
    grp <- grp[order(grp$reviewed_at), ]
    hashes <- grp$content_hash
    texts  <- grp$problem_text
    n      <- nrow(grp)

    if (n < 2L) return(grp)

    # Union-Find mapping: each unique hash -> canonical hash
    canonical <- setNames(hashes, hashes)

    for (i in seq_len(n - 1L)) {
      for (j in seq(i + 1L, n)) {
        if (canonical[hashes[i]] == canonical[hashes[j]]) next  # already same
        jac <- .token_jaccard(texts[i], texts[j])
        if (jac >= threshold) {
          # Replace j's canonical with i's canonical (i is earlier)
          old_canon <- canonical[hashes[j]]
          new_canon <- canonical[hashes[i]]
          # Update all entries pointing to old_canon
          canonical[canonical == old_canon] <- new_canon
        }
      }
    }

    grp$content_hash <- vapply(
      hashes,
      function(h) canonical[[h]],
      character(1L)
    )
    grp
  })

  result
}

#' Upsert loop rows into the roborev_loops DuckDB table
#'
#' Inserts new rows from `loops_df` into the `roborev_loops` table in the
#' specified DuckDB database, updating cycle counts, tiers, and timestamps
#' for existing rows.  **ACK fields (`ack_by`, `ack_at`, `ack_reason`,
#' `ack_until`) are preserved from the existing row and never overwritten by
#' a NULL in the incoming data.**
#'
#' @param loops_df Tibble from [detect_loops()].
#' @param db_path Path to the DuckDB database file.
#' @return Invisibly returns the number of rows upserted.
#' @export
upsert_loops <- function(loops_df, db_path) {
  if (is.null(loops_df) || nrow(loops_df) == 0L) return(invisible(0L))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Ensure table exists
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS roborev_loops (
      content_hash         VARCHAR PRIMARY KEY,
      severity             VARCHAR,
      primary_file         VARCHAR,
      summary              VARCHAR,
      first_seen           TIMESTAMPTZ,
      last_seen            TIMESTAMPTZ,
      cycles               INTEGER,
      tier                 VARCHAR,
      fix_commit_shas      VARCHAR,
      estimated_wasted_usd DOUBLE,
      ack_by               VARCHAR,
      ack_at               TIMESTAMPTZ,
      ack_reason           VARCHAR,
      ack_until            TIMESTAMPTZ
    )
  ")

  # Write incoming rows to a temp table
  DBI::dbWriteTable(con, "loops_incoming", loops_df, overwrite = TRUE)

  # Upsert: preserve ack fields from existing rows
  DBI::dbExecute(con, "
    INSERT INTO roborev_loops BY NAME
    SELECT
      i.content_hash,
      i.severity,
      i.primary_file,
      i.summary,
      i.first_seen,
      i.last_seen,
      i.cycles,
      i.tier,
      i.fix_commit_shas,
      i.estimated_wasted_usd,
      COALESCE(e.ack_by,     i.ack_by)     AS ack_by,
      COALESCE(e.ack_at,     i.ack_at)     AS ack_at,
      COALESCE(e.ack_reason, i.ack_reason) AS ack_reason,
      COALESCE(e.ack_until,  i.ack_until)  AS ack_until
    FROM loops_incoming i
    LEFT JOIN roborev_loops e USING (content_hash)
    ON CONFLICT (content_hash) DO UPDATE SET
      severity             = EXCLUDED.severity,
      primary_file         = EXCLUDED.primary_file,
      summary              = EXCLUDED.summary,
      first_seen           = EXCLUDED.first_seen,
      last_seen            = EXCLUDED.last_seen,
      cycles               = EXCLUDED.cycles,
      tier                 = EXCLUDED.tier,
      fix_commit_shas      = EXCLUDED.fix_commit_shas,
      estimated_wasted_usd = EXCLUDED.estimated_wasted_usd,
      ack_by               = COALESCE(roborev_loops.ack_by,     EXCLUDED.ack_by),
      ack_at               = COALESCE(roborev_loops.ack_at,     EXCLUDED.ack_at),
      ack_reason           = COALESCE(roborev_loops.ack_reason, EXCLUDED.ack_reason),
      ack_until            = COALESCE(roborev_loops.ack_until,  EXCLUDED.ack_until)
  ")

  DBI::dbRemoveTable(con, "loops_incoming")
  invisible(nrow(loops_df))
}
