#!/usr/bin/env Rscript
# Refresh Codex usage cache from ~/.codex/log/codex-tui.log (OTEL-structured).
#
# Writes:
#   inst/extdata/codex_daily.json    — aggregated by (date, canonical_project, model, source)
#   inst/extdata/codex_sessions.json — per-session detail keyed by thread_id
#
# Incremental: tracks last byte offset in inst/extdata/.codex_log_offset.txt.
# Rotation-safe: restarts at byte 0 if log size < recorded offset.
#
# Run: Rscript inst/scripts/refresh_codex_cache.R
# Or:  nix-shell /path/to/default.nix --run "timeout 120 Rscript inst/scripts/refresh_codex_cache.R"

library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)
library(purrr)

# Allow override via env var so worktree runs don't resolve to main checkout
pkg_root <- if (nzchar(Sys.getenv("PKG_ROOT"))) Sys.getenv("PKG_ROOT") else here::here()
extdata  <- file.path(pkg_root, "inst", "extdata")

# ── Canonicalize helper ────────────────────────────────────────────────────────
# Source .canonicalize_project_local from R/canonicalize.R if available,
# otherwise define a minimal fallback for use outside the package context.

.canonicalize_from_cwd <- function(cwd) {
  if (is.na(cwd) || !nzchar(cwd)) return(NA_character_)

  # Known container/namespace segments that are NOT real project names.
  # Must be kept in sync with the same list in the fallback below.
  containers <- c(
    "data", "proj", "pers", "NHS", "health", "antigravity", "finance",
    "stats", "personal",
    "worktrees", "worktree", "Users", "johngavin", "docs_gh", "docs-gh",
    ".claude"
  )

  # If the path contains docs_gh/ or docs-gh/, walk the segments that follow
  # until we find one that is not a container prefix.
  # e.g. /Users/johngavin/docs_gh/llmtelemetry/foo/bar -> "llmtelemetry"
  # e.g. /Users/johngavin/docs_gh/proj/data/micromort/x -> "micromort"
  # e.g. /Users/johngavin/docs_gh/proj/finance/data/historical-momentum-vol -> "historical-momentum-vol"
  docs_match <- regmatches(cwd,
    regexpr("(?:docs_gh|docs-gh)/.*", cwd, perl = TRUE))
  if (length(docs_match) > 0L && nzchar(docs_match)) {
    # Strip the docs_gh/ prefix and split remaining path
    after_root <- sub("^(?:docs_gh|docs-gh)/", "", docs_match, perl = TRUE)
    sub_parts  <- strsplit(after_root, "/", fixed = TRUE)[[1L]]
    sub_parts  <- sub_parts[nzchar(sub_parts)]
    for (seg in sub_parts) {
      if (!seg %in% containers && !grepl("^[0-9]+$", seg) && !startsWith(seg, ".")) {
        return(seg)
      }
    }
  }

  # Fallback: walk parts from right, skip generic container names
  parts <- strsplit(cwd, "/", fixed = TRUE)[[1L]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) return(NA_character_)
  for (i in seq(length(parts), 1L)) {
    seg <- parts[i]
    if (!seg %in% containers && !grepl("^[0-9]+$", seg) && !startsWith(seg, ".")) {
      return(seg)
    }
  }
  NA_character_
}

canonicalize_project <- function(cwd) {
  # Prefer the package's own helper when available (loaded via pkgload::load_all
  # or installed), falling back to the simple filesystem-segment extractor.
  canon_fn <- tryCatch(
    getFromNamespace(".canonicalize_project_local", "llmtelemetry"),
    error = function(e) NULL
  )
  if (!is.null(canon_fn)) {
    # Package helper takes a dash-form project key, not a raw path.
    # Convert the raw filesystem path to a dash-form key first.
    # e.g. /Users/johngavin/docs_gh/llmtelemetry -> docs-gh-llmtelemetry
    dash_key <- function(p) {
      if (is.na(p) || !nzchar(p)) return(NA_character_)
      tail_path <- sub("^.*/docs_gh/", "", p)
      tail_path <- sub("^.*/docs[-_]gh/", "", tail_path)
      tail_path <- gsub("/", "-", tail_path, fixed = TRUE)
      tail_path
    }
    vapply(cwd, function(x) {
      dk <- dash_key(x)
      if (is.na(dk)) return(NA_character_)
      result <- tryCatch(canon_fn(dk), error = function(e) NA_character_)
      if (is.na(result) || !nzchar(result)) .canonicalize_from_cwd(x)
      else result
    }, character(1L), USE.NAMES = FALSE)
  } else {
    vapply(cwd, .canonicalize_from_cwd, character(1L), USE.NAMES = FALSE)
  }
}

# ── Pricing ────────────────────────────────────────────────────────────────────
#' Load codex pricing from inst/extdata/codex_pricing.json.
#' Warns if updated_at is >30 days old.
#' @return list with $models (named list of per-model pricing)
#' @keywords internal
load_pricing <- function(extdata_dir = extdata) {
  pricing_path <- file.path(extdata_dir, "codex_pricing.json")
  if (!file.exists(pricing_path)) {
    warning("codex_pricing.json not found at: ", pricing_path)
    return(NULL)
  }
  p <- jsonlite::read_json(pricing_path)
  # Stale-date warning
  if (!is.null(p$updated_at)) {
    age_days <- as.numeric(Sys.Date() - as.Date(p$updated_at))
    if (!is.na(age_days) && age_days > 30) {
      warning(sprintf(
        "codex_pricing.json is %d days old (updated_at: %s). Verify prices at openai.com/api/pricing/ before depending on cost figures.",
        as.integer(age_days), p$updated_at
      ))
    }
  }
  p
}

# ── Cost computation ───────────────────────────────────────────────────────────
#' Compute estimated cost (USD) for a single turn record.
#' Returns NA_real_ (with warning) when model is not in pricing.
#' @param non_cached_input_tokens integer
#' @param cached_input_tokens     integer
#' @param output_tokens           integer
#' @param model                   character
#' @param pricing                 list from load_pricing()
#' @return numeric (USD) or NA_real_
#' @keywords internal
compute_cost <- function(non_cached_input_tokens, cached_input_tokens,
                         output_tokens, model, pricing) {
  if (is.null(pricing)) return(NA_real_)
  m <- pricing$models[[model]]
  if (is.null(m)) {
    warning(sprintf("Model '%s' not found in codex_pricing.json. est_cost_usd = NA.", model))
    return(NA_real_)
  }
  (non_cached_input_tokens * m$input_per_mtok +
    cached_input_tokens     * m$cached_input_per_mtok +
    output_tokens           * m$output_per_mtok) / 1e6
}

# ── Rotation detection ────────────────────────────────────────────────────────
#' Return the byte offset to start reading from.
#' If current file size < recorded offset (rotation), returns 0L.
#' @param log_path  character path to log file
#' @param offset    numeric recorded offset (may be Inf for "start fresh")
#' @return integer byte offset (0 = start from beginning)
#' @keywords internal
detect_rotation <- function(log_path, offset) {
  if (!file.exists(log_path)) return(0L)
  fsize <- file.info(log_path)$size
  if (is.na(fsize) || fsize < offset) return(0L)
  as.integer(offset)
}

# ── Parse a single turn-close OTEL line ──────────────────────────────────────
#' Parse a single OTEL turn-close line from codex-tui.log.
#'
#' Matches lines that contain `codex.turn.token_usage.total_tokens=` and
#' extracts: timestamp, thread_id, model, reasoning_effort, and all 6
#' token_usage fields.
#'
#' Fields may be duplicated in the log (emitted twice); we take the first match.
#'
#' @param line character (single string)
#' @return named list with parsed fields, or NULL if line does not match
#' @keywords internal
parse_otel_turn_line <- function(line) {
  if (!grepl("codex\\.turn\\.token_usage\\.total_tokens=", line)) {
    return(NULL)
  }

  extract1 <- function(pattern, x) {
    m <- regmatches(x, regexpr(pattern, x, perl = TRUE))
    if (length(m) == 0L || !nzchar(m)) return(NA_character_)
    sub(pattern, "\\1", m, perl = TRUE)
  }

  # Timestamp: ISO 8601 at start of line
  ts_raw <- extract1("^(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d+Z)", line)
  # thread.id from span attributes (newer format: thread.id=UUID)
  thread_id <- extract1("thread\\.id=([0-9a-f-]{36})", line)
  # model
  model <- extract1("model=([^ }]+)", line)
  # reasoning effort
  reasoning_effort <- extract1("codex\\.turn\\.reasoning_effort=([^ }]+)", line)

  extract_tok <- function(field) {
    pattern <- paste0("codex\\.turn\\.token_usage\\.", field, "=(\\d+)")
    val <- extract1(pattern, line)
    if (is.na(val)) NA_integer_ else as.integer(val)
  }

  input_tokens            <- extract_tok("input_tokens")
  cached_input_tokens     <- extract_tok("cached_input_tokens")
  non_cached_input_tokens <- extract_tok("non_cached_input_tokens")
  output_tokens           <- extract_tok("output_tokens")
  reasoning_output_tokens <- extract_tok("reasoning_output_tokens")
  total_tokens            <- extract_tok("total_tokens")

  # Require at minimum: thread_id + total_tokens
  if (is.na(thread_id) || is.na(total_tokens)) return(NULL)

  # Derive non_cached if not explicit
  if (is.na(non_cached_input_tokens) && !is.na(input_tokens) && !is.na(cached_input_tokens)) {
    non_cached_input_tokens <- input_tokens - cached_input_tokens
  }

  list(
    timestamp               = ts_raw,
    thread_id               = thread_id,
    model                   = model,
    reasoning_effort        = reasoning_effort,
    input_tokens            = input_tokens,
    cached_input_tokens     = cached_input_tokens,
    non_cached_input_tokens = non_cached_input_tokens,
    output_tokens           = output_tokens,
    reasoning_output_tokens = reasoning_output_tokens,
    total_tokens            = total_tokens
  )
}

# ── CWD extraction from log lines ─────────────────────────────────────────────
# Extract workdir/cwd from lines in the log.
# Two formats:
#   Old: run_turn{turn_id=N model=M cwd=/path/to/dir}
#   New: "workdir":"/path/to/dir" (JSON in exec_command/shell_command payloads)
.extract_cwd_from_line <- function(line) {
  # Old span format: cwd=/Users/...
  m_old <- regmatches(line, regexpr("\\bcwd=(/[^ }]+)", line, perl = TRUE))
  if (length(m_old) > 0L && nzchar(m_old)) {
    return(sub("^cwd=", "", m_old))
  }
  # New JSON format: "workdir":"/..."  (double-quoted JSON value)
  m_new <- regmatches(line, regexpr('"workdir":"(/[^"]+)"', line, perl = TRUE))
  if (length(m_new) > 0L && nzchar(m_new)) {
    return(gsub('^"workdir":"', "", gsub('"$', "", m_new)))
  }
  NA_character_
}

# ── Vectorized regex helper ───────────────────────────────────────────────────
# Extract first capture group from all elements of a character vector.
# Returns NA_character_ where pattern doesn't match.
.extract_vec <- function(pattern, x) {
  m <- regmatches(x, regexpr(pattern, x, perl = TRUE))
  result <- rep(NA_character_, length(x))
  hit_idx <- which(nchar(m) > 0L)
  if (length(hit_idx) > 0L) {
    result[hit_idx] <- sub(pattern, "\\1", m[hit_idx], perl = TRUE)
  }
  result
}

# ── Read and parse log (incremental, vectorized) ──────────────────────────────
#' Read codex-tui.log incrementally and return a tibble of parsed turn records.
#' Uses vectorized regex for performance on large log files (84+ MB).
#'
#' @param log_path      path to the main log file
#' @param offset_path   path to the sidecar offset file
#' @return list(turns = tibble, offset_used = integer)
#' @keywords internal
read_codex_log <- function(log_path, offset_path) {
  if (!file.exists(log_path)) {
    message("codex-tui.log not found at: ", log_path)
    return(list(turns = tibble(), offset_used = 0L))
  }

  # Load recorded offset
  recorded_offset <- if (file.exists(offset_path)) {
    tryCatch(as.numeric(readLines(offset_path, n = 1L, warn = FALSE)),
             error = function(e) 0)
  } else {
    0
  }

  # Handle rotation
  start_offset <- detect_rotation(log_path, recorded_offset)

  # Also process rotated sibling files (codex-tui.log.1, etc.)
  log_dir  <- dirname(log_path)
  log_base <- basename(log_path)
  siblings <- sort(list.files(log_dir,
    pattern = paste0("^", log_base, "\\."),
    full.names = TRUE))

  all_lines <- character(0L)
  for (sib in siblings) {
    all_lines <- c(all_lines, readLines(sib, warn = FALSE))
  }

  # Current log from start_offset
  fsize <- file.info(log_path)$size
  if (start_offset < fsize) {
    con <- file(log_path, open = "rb")
    if (start_offset > 0L) seek(con, where = start_offset)
    cur_lines <- readLines(con, warn = FALSE)
    close(con)
    all_lines  <- c(all_lines, cur_lines)
    new_offset <- fsize
  } else {
    new_offset <- start_offset
  }

  if (length(all_lines) == 0L) {
    return(list(turns = tibble(), offset_used = as.integer(new_offset)))
  }

  # ── Step 1: Identify token-usage lines (vectorized) ─────────────────────────
  is_turn_line <- grepl("codex\\.turn\\.token_usage\\.total_tokens=", all_lines)
  turn_lines   <- all_lines[is_turn_line]

  if (length(turn_lines) == 0L) {
    return(list(turns = tibble(), offset_used = as.integer(new_offset)))
  }

  # ── Step 2: Extract all token fields via vectorized regex ───────────────────
  tok <- function(field) {
    vals <- .extract_vec(
      paste0("codex\\.turn\\.token_usage\\.", field, "=(\\d+)"),
      turn_lines
    )
    suppressWarnings(as.integer(vals))
  }

  timestamps    <- .extract_vec(
    "^(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d+Z)", turn_lines)
  thread_ids    <- .extract_vec(
    "thread\\.id=([0-9a-f-]{36})", turn_lines)
  models        <- .extract_vec("model=([^ }]+)", turn_lines)
  eff           <- .extract_vec("codex\\.turn\\.reasoning_effort=([^ }]+)", turn_lines)
  input_tok     <- tok("input_tokens")
  cached_tok    <- tok("cached_input_tokens")
  noncached_tok <- tok("non_cached_input_tokens")
  output_tok    <- tok("output_tokens")
  reasoning_tok <- tok("reasoning_output_tokens")
  total_tok     <- tok("total_tokens")

  # Derive non_cached where absent
  noncached_tok <- ifelse(
    is.na(noncached_tok) & !is.na(input_tok) & !is.na(cached_tok),
    input_tok - cached_tok,
    noncached_tok
  )

  # Keep only rows where thread_id and total_tokens are non-NA
  keep <- !is.na(thread_ids) & !is.na(total_tok)
  if (!any(keep)) {
    return(list(turns = tibble(), offset_used = as.integer(new_offset)))
  }

  turns <- tibble(
    timestamp               = timestamps[keep],
    thread_id               = thread_ids[keep],
    model                   = models[keep],
    reasoning_effort        = eff[keep],
    input_tokens            = input_tok[keep],
    cached_input_tokens     = cached_tok[keep],
    non_cached_input_tokens = noncached_tok[keep],
    output_tokens           = output_tok[keep],
    reasoning_output_tokens = reasoning_tok[keep],
    total_tokens            = total_tok[keep]
  )

  # ── Step 3: Build thread_id -> cwd lookup from cwd-bearing lines ─────────────
  # Only scan cwd-bearing lines (much smaller subset)
  # Matches both: cwd=/path  and  "workdir":"/path"
  has_cwd   <- grepl('\\bcwd=/|"workdir":"/', all_lines, perl = TRUE)
  cwd_lines <- all_lines[has_cwd]

  cwd_by_thread <- list()
  if (length(cwd_lines) > 0L) {
    cwd_vals <- vapply(cwd_lines, .extract_cwd_from_line, character(1L),
                       USE.NAMES = FALSE)
    tid_vals <- .extract_vec("thread[_.]id=([0-9a-f-]{36})", cwd_lines)
    for (i in seq_along(cwd_lines)) {
      tid <- tid_vals[i]
      cwd <- cwd_vals[i]
      if (!is.na(tid) && !is.na(cwd) && is.null(cwd_by_thread[[tid]])) {
        cwd_by_thread[[tid]] <- cwd
      }
    }
  }

  # Attach cwd to turns
  turns$cwd <- vapply(turns$thread_id, function(tid) {
    cwd_by_thread[[tid]] %||% NA_character_
  }, character(1L), USE.NAMES = FALSE)

  list(turns = turns, offset_used = as.integer(new_offset))
}

# ── NULL coalescing operator ───────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── Roborev join ──────────────────────────────────────────────────────────────
#' Fetch roborev job list and join with turns by session_id == thread_id.
#' Sets source = "roborev" for matches, "interactive" otherwise.
#' Also attaches repo_id and repo_name for matched rows.
#'
#' @param turns tibble with thread_id column
#' @return tibble with source, repo_id, repo_name added
#' @keywords internal
join_roborev <- function(turns) {
  turns <- turns |>
    mutate(source = "interactive", repo_id = NA_character_, repo_name = NA_character_)

  # Try to get roborev data
  roborev_json <- tryCatch({
    res <- system2("roborev", c("list", "--json", "--limit", "5000"),
                   stdout = TRUE, stderr = FALSE)
    if (length(res) > 0L) paste(res, collapse = "\n") else NULL
  }, error = function(e) NULL,
     warning = function(e) NULL)

  if (is.null(roborev_json) || !nzchar(roborev_json)) {
    message("roborev list not available; all turns marked as 'interactive'")
    return(turns)
  }

  roborev_data <- tryCatch(
    jsonlite::fromJSON(roborev_json, simplifyDataFrame = TRUE),
    error = function(e) NULL
  )

  if (is.null(roborev_data) || !is.data.frame(roborev_data)) {
    message("roborev list JSON could not be parsed; all turns marked as 'interactive'")
    return(turns)
  }

  needed <- c("session_id")
  if (!all(needed %in% names(roborev_data))) {
    message("roborev JSON missing 'session_id' column; all turns marked as 'interactive'")
    return(turns)
  }

  # Build a lookup: session_id -> (repo_id, repo_name)
  # Defensively select only columns that exist in roborev_data
  rv_cols <- intersect(c("session_id", "repo_id", "repo_name"), names(roborev_data))
  rv <- roborev_data[, rv_cols, drop = FALSE] |>
    as_tibble() |>
    # Ensure repo_id / repo_name exist even if absent from roborev output
    {function(d) {
      if (!"repo_id"   %in% names(d)) d$repo_id   <- NA_character_
      if (!"repo_name" %in% names(d)) d$repo_name <- NA_character_
      d
    }}() |>
    distinct(session_id, .keep_all = TRUE) |>
    rename(thread_id = session_id)

  # Add a sentinel so we can detect join success independently of repo_id being NA.
  # A matched row will have .roborev_matched = TRUE even if repo_id is NA.
  rv_with_sentinel <- rv |>
    mutate(.roborev_matched = TRUE)

  # Left join: matched rows get repo_id/repo_name from rv; source set accordingly
  turns_joined <- turns |>
    left_join(rv_with_sentinel, by = "thread_id", suffix = c("", ".rb"))

  has_rb_id   <- "repo_id.rb"   %in% names(turns_joined)
  has_rb_name <- "repo_name.rb" %in% names(turns_joined)

  turns_joined <- turns_joined |>
    mutate(
      # Use join sentinel — source = "roborev" whenever the thread_id matched a
      # roborev row, regardless of whether repo_id/repo_name are NA.
      source    = if_else(!is.na(.data$.roborev_matched), "roborev", "interactive"),
      repo_id   = if (has_rb_id)
                    dplyr::coalesce(.data$repo_id.rb,   .data$repo_id)
                  else .data$repo_id,
      repo_name = if (has_rb_name)
                    dplyr::coalesce(.data$repo_name.rb, .data$repo_name)
                  else .data$repo_name
    ) |>
    select(-any_of(c("repo_id.rb", "repo_name.rb", ".roborev_matched")))

  turns_joined
}

# ── Aggregate daily ───────────────────────────────────────────────────────────
#' Aggregate per-turn records to daily summaries.
#' @param turns tibble with columns: timestamp, canonical_project, model,
#'              source, cached_input_tokens, non_cached_input_tokens,
#'              output_tokens, reasoning_output_tokens, total_tokens, est_cost_usd
#' @return tibble with one row per (date, canonical_project, model, source)
#' @keywords internal
aggregate_daily <- function(turns) {
  if (nrow(turns) == 0L) return(tibble())

  turns |>
    mutate(
      date = as.Date(substr(timestamp, 1L, 10L)),
      # Replace NA canonical_project with sentinel for grouping; restore after
      canonical_project = if_else(is.na(canonical_project), "__UNKNOWN__",
                                  canonical_project)
    ) |>
    group_by(date, canonical_project, model, source) |>
    summarise(
      input_tokens            = sum(input_tokens,            na.rm = TRUE),
      cached_input_tokens     = sum(cached_input_tokens,     na.rm = TRUE),
      output_tokens           = sum(output_tokens,           na.rm = TRUE),
      reasoning_output_tokens = sum(reasoning_output_tokens, na.rm = TRUE),
      total_tokens            = sum(total_tokens,            na.rm = TRUE),
      n_turns                 = n(),
      est_cost_usd            = if (all(is.na(est_cost_usd))) NA_real_
                                else sum(est_cost_usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      date = as.character(date),
      # Restore sentinel back to NA
      canonical_project = if_else(canonical_project == "__UNKNOWN__",
                                  NA_character_, canonical_project)
    ) |>
    arrange(date)
}

# ── Main ──────────────────────────────────────────────────────────────────────
if (!interactive()) {
  log_path    <- path.expand("~/.codex/log/codex-tui.log")
  offset_path <- file.path(extdata, ".codex_log_offset.txt")

  cat("=== refresh_codex_cache.R ===\n")
  cat(sprintf("Log file  : %s\n", log_path))
  cat(sprintf("Offset    : %s\n", offset_path))

  # 1. Read log
  read_result <- read_codex_log(log_path, offset_path)
  turns       <- read_result$turns
  new_offset  <- read_result$offset_used

  cat(sprintf("Turns read: %d\n", nrow(turns)))

  if (nrow(turns) == 0L) {
    cat("No new turns to process.\n")
    # Still write empty outputs so downstream scripts can rely on files existing
    if (!file.exists(file.path(extdata, "codex_daily.json"))) {
      jsonlite::write_json(list(), file.path(extdata, "codex_daily.json"),
                           auto_unbox = TRUE)
    }
    if (!file.exists(file.path(extdata, "codex_sessions.json"))) {
      jsonlite::write_json(list(), file.path(extdata, "codex_sessions.json"),
                           auto_unbox = TRUE)
    }
    writeLines(as.character(new_offset), offset_path)
    quit(status = 0L)
  }

  # 2. Sanitize cwd -> canonical_project (drop raw cwd from output)
  turns <- turns |>
    mutate(canonical_project = canonicalize_project(cwd)) |>
    select(-cwd)

  # 3. Load pricing and compute cost
  pricing <- load_pricing(extdata)
  turns <- turns |>
    rowwise() |>
    mutate(
      est_cost_usd = compute_cost(
        non_cached_input_tokens = non_cached_input_tokens %||% 0L,
        cached_input_tokens     = cached_input_tokens     %||% 0L,
        output_tokens           = output_tokens           %||% 0L,
        model                   = model,
        pricing                 = pricing
      )
    ) |>
    ungroup()

  # 4. Join with roborev
  turns <- join_roborev(turns)

  # 5. Write per-session detail
  sessions <- turns |>
    group_by(thread_id) |>
    summarise(
      started_at              = min(timestamp, na.rm = TRUE),
      ended_at                = max(timestamp, na.rm = TRUE),
      canonical_project       = first(canonical_project[!is.na(canonical_project)]),
      model                   = first(model),
      source                  = first(source),
      total_tokens            = sum(total_tokens,  na.rm = TRUE),
      est_cost_usd            = if (all(is.na(est_cost_usd))) NA_real_
                                else sum(est_cost_usd, na.rm = TRUE),
      n_turns                 = n(),
      .groups = "drop"
    )
  # Attach repo_id / repo_name from turns (take first non-NA)
  if ("repo_id" %in% names(turns)) {
    repo_info <- turns |>
      filter(!is.na(repo_id)) |>
      distinct(thread_id, repo_id, repo_name)
    sessions <- sessions |>
      left_join(repo_info, by = "thread_id")
  }

  # ── Helper: atomic JSON write via temp-file rename ─────────────────────────
  # Writes to a sibling .tmp file then renames atomically so that a SIGKILL /
  # disk-full mid-write cannot truncate the production file.  Also asserts that
  # the combined row count is monotone (>= existing) before writing so that an
  # accidental upstream parse failure cannot silently truncate history.
  write_json_atomic <- function(data, path, min_rows = 0L, label = basename(path)) {
    n_rows <- nrow(data)
    if (n_rows < min_rows) {
      cli::cli_abort(c(
        "x" = "Refusing to write {label}: row count would shrink.",
        "i" = "Existing rows: {min_rows}; new combined rows: {n_rows}.",
        "i" = "This indicates an upstream parse failure — aborting to protect history."
      ))
    }
    tmp_path <- paste0(path, ".tmp")
    jsonlite::write_json(data, tmp_path, auto_unbox = TRUE, digits = 6)
    file.rename(tmp_path, path)
    invisible(n_rows)
  }

  # Read existing sessions and merge (incremental update)
  sessions_path <- file.path(extdata, "codex_sessions.json")
  existing_sessions <- if (file.exists(sessions_path)) {
    tryCatch(jsonlite::fromJSON(sessions_path, simplifyDataFrame = TRUE),
             error = function(e) tibble())
  } else tibble()

  n_existing_sessions <- if (is.data.frame(existing_sessions)) nrow(existing_sessions) else 0L
  if (is.data.frame(existing_sessions) && nrow(existing_sessions) > 0L) {
    existing_sessions <- as_tibble(existing_sessions)
    # Combine; new rows overwrite existing for same thread_id
    all_sessions <- bind_rows(
      existing_sessions |> filter(!thread_id %in% sessions$thread_id),
      sessions
    )
  } else {
    all_sessions <- sessions
  }

  write_json_atomic(all_sessions, sessions_path,
                    min_rows = n_existing_sessions, label = "codex_sessions.json")
  cat(sprintf("Sessions  : %d total (%d new/updated)\n",
              nrow(all_sessions), nrow(sessions)))

  # 6. Aggregate daily
  daily <- aggregate_daily(turns)

  # Read existing daily and merge
  daily_path <- file.path(extdata, "codex_daily.json")
  existing_daily <- if (file.exists(daily_path)) {
    tryCatch(jsonlite::fromJSON(daily_path, simplifyDataFrame = TRUE),
             error = function(e) tibble())
  } else tibble()

  n_existing_daily <- if (is.data.frame(existing_daily)) nrow(existing_daily) else 0L
  if (is.data.frame(existing_daily) && nrow(existing_daily) > 0L) {
    existing_daily <- as_tibble(existing_daily)
    # Remove rows that will be replaced by new data for same keys
    key_cols <- c("date", "canonical_project", "model", "source")
    key_cols_existing <- intersect(key_cols, names(existing_daily))
    if (length(key_cols_existing) == length(key_cols)) {
      new_keys <- daily |> select(all_of(key_cols))
      existing_daily <- existing_daily |>
        anti_join(new_keys, by = key_cols)
    }
    all_daily <- bind_rows(existing_daily, daily) |> arrange(date)
  } else {
    all_daily <- daily
  }

  write_json_atomic(all_daily, daily_path,
                    min_rows = n_existing_daily, label = "codex_daily.json")
  cat(sprintf("Daily rows: %d total (%d new/updated)\n",
              nrow(all_daily), nrow(daily)))

  # 7. Update offset
  writeLines(as.character(new_offset), offset_path)
  cat(sprintf("Offset    : updated to %d bytes\n", new_offset))

  # 8. Summary
  if (nrow(turns) > 0L) {
    dates  <- as.Date(substr(turns$timestamp, 1L, 10L))
    cat(sprintf("\nSummary   : %d turns | dates %s to %s\n",
                nrow(turns), min(dates, na.rm = TRUE), max(dates, na.rm = TRUE)))
    cat(sprintf("  Total tokens : %s\n",
                format(sum(turns$total_tokens, na.rm = TRUE), big.mark = ",")))
    cost_total <- sum(turns$est_cost_usd, na.rm = TRUE)
    cat(sprintf("  Est cost USD : %.4f\n", cost_total))
  }

  cat("=== done ===\n")
}
