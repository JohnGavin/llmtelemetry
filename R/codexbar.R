# CodexBar Usage and Cost Parsers
# Parsers for CodexBar CLI JSON outputs (complementary to ccusage pipeline).
# CodexBar CLI: /opt/homebrew/bin/codexbar v0.29.0
# Do NOT replace ccusage — these are a second, complementary usage source.

utils::globalVariables(c(
  # parse_codexbar_usage
  "provider", "source", "used_pct", "window_minutes", "resets_at",
  "credits_remaining", "updated_at", "limit_window",
  # parse_codexbar_cost
  "date", "model", "cost", "total_tokens", "input_tokens", "output_tokens",
  "cache_read_tokens", "cache_creation_tokens", "modelName",
  "modelBreakdowns", "historyDays", "daily"
))

# ---------------------------------------------------------------------------
# parse_codexbar_usage
# ---------------------------------------------------------------------------

#' Parse CodexBar usage JSON into a tidy tibble
#'
#' Reads the sanitised `codexbar_usage.json` file produced by
#' `inst/scripts/sanitize_codexbar.R` (PII already stripped).  Returns one row
#' per provider per limit window (primary / secondary / tertiary).  The raw
#' `accountEmail`, `accountOrganization`, `identity`, and `loginMethod` fields
#' are never present in the sanitised input; this function does not need to
#' strip them.
#'
#' @param path Character(1).  Path to the sanitised `codexbar_usage.json`.
#'   Defaults to `inst/extdata/codexbar_usage.json` resolved via [here::here()].
#' @return A [tibble::tibble()] with columns:
#'   \describe{
#'     \item{provider}{character — provider name (e.g. "claude", "openai")}
#'     \item{limit_window}{character — one of "primary", "secondary", "tertiary"}
#'     \item{used_pct}{numeric — percentage of the window limit consumed (0-100)}
#'     \item{window_minutes}{integer — length of the rate-limit window in minutes}
#'     \item{resets_at}{character — ISO-8601 timestamp when the window resets}
#'     \item{credits_remaining}{numeric — credits remaining (NA if not applicable)}
#'     \item{source}{character — data source field from the raw payload}
#'     \item{updated_at}{character — ISO-8601 timestamp of the last update}
#'   }
#'   Returns a zero-row tibble with those columns if the file does not exist or
#'   contains no parseable records.
#' @export
#' @examples
#' \dontrun{
#' # Read from the default package location
#' usage <- parse_codexbar_usage()
#'
#' # Read from an explicit path (e.g. a test fixture)
#' usage <- parse_codexbar_usage("tests/testthat/fixtures/codexbar_usage_fixture.json")
#' }
parse_codexbar_usage <- function(
    path = NULL
) {
  if (is.null(path)) {
    candidates <- c(
      file.path("inst", "extdata", "codexbar_usage.json"),
      file.path("..", "inst", "extdata", "codexbar_usage.json")
    )
    if (requireNamespace("here", quietly = TRUE)) {
      candidates <- c(here::here("inst", "extdata", "codexbar_usage.json"), candidates)
    }
    path <- Find(file.exists, candidates)
  }

  empty_tbl <- tibble::tibble(
    provider          = character(),
    limit_window      = character(),
    used_pct          = numeric(),
    window_minutes    = integer(),
    resets_at         = character(),
    credits_remaining = numeric(),
    source            = character(),
    updated_at        = character()
  )

  if (is.null(path) || !file.exists(path)) {
    message("codexbar_usage.json not found: ", path %||% "<no path resolved>")
    return(empty_tbl)
  }

  providers_list <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      message("Failed to parse codexbar_usage.json: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(providers_list) || !is.list(providers_list)) return(empty_tbl)

  window_names <- c("primary", "secondary", "tertiary")

  rows <- purrr::map_dfr(providers_list, function(prov) {
    pname   <- prov[["provider"]] %||% NA_character_
    psrc    <- prov[["source"]]   %||% NA_character_
    usage   <- prov[["usage"]]
    credits <- prov[["credits"]]

    credits_rem <- tryCatch(
      as.numeric(credits[["remaining"]] %||% NA_real_),
      error = function(e) NA_real_
    )
    upd_at <- tryCatch(
      as.character(usage[["updatedAt"]] %||% NA_character_),
      error = function(e) NA_character_
    )

    purrr::map_dfr(window_names, function(wname) {
      win <- usage[[wname]]
      if (is.null(win)) return(NULL)
      tibble::tibble(
        provider          = as.character(pname),
        limit_window      = wname,
        used_pct          = as.numeric(win[["usedPercent"]]    %||% NA_real_),
        window_minutes    = as.integer(win[["windowMinutes"]]  %||% NA_integer_),
        resets_at         = as.character(win[["resetsAt"]]     %||% NA_character_),
        credits_remaining = credits_rem,
        source            = as.character(psrc),
        updated_at        = upd_at
      )
    })
  })

  if (is.null(rows) || nrow(rows) == 0L) return(empty_tbl)
  rows
}

# ---------------------------------------------------------------------------
# parse_codexbar_cost
# ---------------------------------------------------------------------------

#' Parse CodexBar cost JSON into a tidy per-provider per-date per-model tibble
#'
#' Reads the sanitised `codexbar_cost_daily.json` file produced by
#' `inst/scripts/sanitize_codexbar.R`.  Expands the nested `modelBreakdowns`
#' array so that each row represents one provider × date × model combination.
#' Providers or dates with no `modelBreakdowns` entry are omitted (the top-level
#' aggregate fields such as `last30DaysCostUSD` are not included here — use a
#' separate rollup if needed).
#'
#' Note: CodexBar cost data uses `source:"local"` — it is a local estimate
#' analogous to ccusage, not provider billing data.
#'
#' @param path Character(1).  Path to the sanitised `codexbar_cost_daily.json`.
#'   Defaults to `inst/extdata/codexbar_cost_daily.json` resolved via
#'   [here::here()].
#' @return A [tibble::tibble()] with columns:
#'   \describe{
#'     \item{provider}{character — provider name}
#'     \item{date}{character — ISO-8601 date string (YYYY-MM-DD)}
#'     \item{model}{character — model name}
#'     \item{cost}{numeric — estimated cost in USD for this model on this date}
#'     \item{total_tokens}{numeric — total tokens consumed}
#'     \item{input_tokens}{numeric — input tokens (prompt)}
#'     \item{output_tokens}{numeric — output tokens (completion)}
#'     \item{cache_read_tokens}{numeric — cache-read tokens (0 if provider does not report)}
#'     \item{cache_creation_tokens}{numeric — cache-creation tokens (0 if not reported)}
#'   }
#'   Returns a zero-row tibble with those columns if the file does not exist or
#'   contains no parseable records.
#' @export
#' @examples
#' \dontrun{
#' # Read from the default package location
#' cost <- parse_codexbar_cost()
#'
#' # Read from a test fixture
#' cost <- parse_codexbar_cost("tests/testthat/fixtures/codexbar_cost_fixture.json")
#' }
parse_codexbar_cost <- function(
    path = NULL
) {
  if (is.null(path)) {
    candidates <- c(
      file.path("inst", "extdata", "codexbar_cost_daily.json"),
      file.path("..", "inst", "extdata", "codexbar_cost_daily.json")
    )
    if (requireNamespace("here", quietly = TRUE)) {
      candidates <- c(
        here::here("inst", "extdata", "codexbar_cost_daily.json"),
        candidates
      )
    }
    path <- Find(file.exists, candidates)
  }

  empty_tbl <- tibble::tibble(
    provider              = character(),
    date                  = character(),
    model                 = character(),
    cost                  = numeric(),
    total_tokens          = numeric(),
    input_tokens          = numeric(),
    output_tokens         = numeric(),
    cache_read_tokens     = numeric(),
    cache_creation_tokens = numeric()
  )

  if (is.null(path) || !file.exists(path)) {
    message("codexbar_cost_daily.json not found: ", path %||% "<no path resolved>")
    return(empty_tbl)
  }

  providers_list <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      message("Failed to parse codexbar_cost_daily.json: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(providers_list) || !is.list(providers_list)) return(empty_tbl)

  rows <- purrr::map_dfr(providers_list, function(prov) {
    pname <- prov[["provider"]] %||% NA_character_
    daily <- prov[["daily"]]
    if (is.null(daily) || !is.list(daily)) return(NULL)

    purrr::map_dfr(daily, function(day_entry) {
      d <- as.character(day_entry[["date"]] %||% NA_character_)
      breakdowns <- day_entry[["modelBreakdowns"]]
      if (is.null(breakdowns) || !is.list(breakdowns) || length(breakdowns) == 0L) {
        return(NULL)
      }
      purrr::map_dfr(breakdowns, function(mb) {
        tibble::tibble(
          provider              = as.character(pname),
          date                  = d,
          model                 = as.character(mb[["modelName"]] %||% NA_character_),
          cost                  = as.numeric(mb[["cost"]]          %||% 0),
          total_tokens          = as.numeric(mb[["totalTokens"]]   %||% 0),
          input_tokens          = as.numeric(
            mb[["inputTokens"]]         %||%
            mb[["input_tokens"]]        %||% 0
          ),
          output_tokens         = as.numeric(
            mb[["outputTokens"]]        %||%
            mb[["output_tokens"]]       %||% 0
          ),
          cache_read_tokens     = as.numeric(
            mb[["cacheReadTokens"]]     %||%
            mb[["cache_read_tokens"]]   %||% 0
          ),
          cache_creation_tokens = as.numeric(
            mb[["cacheCreationTokens"]] %||%
            mb[["cache_creation_tokens"]] %||% 0
          )
        )
      })
    })
  })

  if (is.null(rows) || nrow(rows) == 0L) return(empty_tbl)
  rows
}

# ---------------------------------------------------------------------------
# Internal helper — NULL-coalescing operator (not imported from rlang to keep
# the dependency surface small).
# ---------------------------------------------------------------------------

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0L && !is.na(x[1L])) x else y
