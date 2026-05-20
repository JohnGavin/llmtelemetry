# Tests for refresh_codex_cache.R — pure function unit tests
# Pure functions are sourced from the script and tested in isolation.
#
# Coverage:
#   parse_otel_turn_line()   — regex parsing of OTEL log lines
#   compute_cost()           — cost calculation from token counts + pricing
#   detect_rotation()        — log rotation handling
#   aggregate_daily()        — per-turn -> daily aggregation
#   canonicalize_project()   — cwd sanitization (privacy)
#   join_roborev()           — source attribution

# ── Load pure helpers from script ─────────────────────────────────────────────
# Strategy: locate the script, strip the `if (!interactive())` main block,
# write a temp version, then source it so library() calls attach to the
# search path and function closures can resolve dplyr/tibble functions.
# The helpers are then available in the current env after sourcing.

local({
  script <- system.file("scripts", "refresh_codex_cache.R",
                        package = "llmtelemetry")
  if (!nzchar(script)) {
    script_candidate <- tryCatch(
      file.path(here::here(), "inst", "scripts", "refresh_codex_cache.R"),
      error = function(e) ""
    )
    if (nzchar(script_candidate) && file.exists(script_candidate)) {
      script <- script_candidate
    }
  }
  if (!nzchar(script) || !file.exists(script)) return(invisible(NULL))

  lines <- readLines(script, warn = FALSE)

  # Strip `if (!interactive()) { ... }` main block (last block in file)
  main_start <- which(grepl("^if \\(!interactive\\(\\)\\)", lines))[1L]
  if (!is.na(main_start)) lines <- lines[seq_len(main_start - 1L)]

  # Replace here::here() so pkg_root assignment doesn't fail in test context
  lines <- gsub("here::here()", 'tempdir()', lines, fixed = TRUE)

  tmp <- tempfile(fileext = ".R")
  writeLines(lines, tmp)
  # source() into the calling frame (globalenv equivalent for test-file scope)
  # suppressMessages suppresses library() startup banners
  suppressMessages(source(tmp, local = FALSE))
  unlink(tmp)
})

# ── Fixtures ───────────────────────────────────────────────────────────────────

# A known-good OTEL turn-close line (from the investigation scope)
GOOD_LINE <- paste0(
  "2026-05-19T17:52:13.305783Z  INFO ",
  "session_loop{thread_id=019e415d-ba37-7072-8eee-38e184710377}:",
  "submission_dispatch{otel.name=\"op.dispatch.user_input_with_turn_context\" ",
  "submission.id=\"019e415d-bbfe-75d2-9ca7-e865bbeff6d8\" ",
  "codex.op=\"user_input_with_turn_context\"}:",
  "turn{otel.name=\"session_task.turn\" ",
  "thread.id=019e415d-ba37-7072-8eee-38e184710377 ",
  "turn.id=019e415d-bbfe-75d2-9ca7-e865bbeff6d8 ",
  "model=gpt-5.4 ",
  "codex.turn.reasoning_effort=high ",
  "codex.turn.token_usage.input_tokens=19608 ",
  "codex.turn.token_usage.input_tokens=19608 ",
  "codex.turn.token_usage.cached_input_tokens=9088 ",
  "codex.turn.token_usage.cached_input_tokens=9088 ",
  "codex.turn.token_usage.non_cached_input_tokens=10520 ",
  "codex.turn.token_usage.non_cached_input_tokens=10520 ",
  "codex.turn.token_usage.output_tokens=277 ",
  "codex.turn.token_usage.output_tokens=277 ",
  "codex.turn.token_usage.reasoning_output_tokens=234 ",
  "codex.turn.token_usage.reasoning_output_tokens=234 ",
  "codex.turn.token_usage.total_tokens=19885 ",
  "codex.turn.token_usage.total_tokens=19885}: codex_core::tasks: close ",
  "time.busy=23.1ms time.idle=10.7s"
)

MINIMAL_PRICING <- list(
  updated_at = format(Sys.Date(), "%Y-%m-%d"),
  models = list(
    `gpt-5.4` = list(
      input_per_mtok         = 0.40,
      cached_input_per_mtok  = 0.04,
      output_per_mtok        = 1.60
    )
  )
)

# ── parse_otel_turn_line ───────────────────────────────────────────────────────

test_that("parse_otel_turn_line extracts all fields from a known-good line", {
  skip_if(is.null(parse_otel_turn_line),
          "parse_otel_turn_line not found (script may not be installed)")

  result <- parse_otel_turn_line(GOOD_LINE)

  expect_false(is.null(result))
  expect_equal(result$thread_id,               "019e415d-ba37-7072-8eee-38e184710377")
  expect_equal(result$model,                    "gpt-5.4")
  expect_equal(result$reasoning_effort,         "high")
  expect_equal(result$input_tokens,             19608L)
  expect_equal(result$cached_input_tokens,      9088L)
  expect_equal(result$non_cached_input_tokens,  10520L)
  expect_equal(result$output_tokens,            277L)
  expect_equal(result$reasoning_output_tokens,  234L)
  expect_equal(result$total_tokens,             19885L)
  expect_match(result$timestamp, "^2026-05-19T")
})

test_that("parse_otel_turn_line returns NULL for a non-token-usage line", {
  skip_if(is.null(parse_otel_turn_line),
          "parse_otel_turn_line not found")

  expect_null(parse_otel_turn_line(
    "2026-05-19T17:49:44.267014Z  INFO session_loop{thread_id=abc}: codex_core: new"
  ))
  expect_null(parse_otel_turn_line(""))
  expect_null(parse_otel_turn_line("not a log line at all"))
})

test_that("parse_otel_turn_line handles missing non_cached_input_tokens gracefully", {
  skip_if(is.null(parse_otel_turn_line),
          "parse_otel_turn_line not found")

  line_no_noncached <- gsub(
    "codex\\.turn\\.token_usage\\.non_cached_input_tokens=10520 ",
    "",
    GOOD_LINE
  )
  result <- parse_otel_turn_line(line_no_noncached)
  expect_false(is.null(result))
  # Should derive non_cached = input - cached
  expect_equal(result$non_cached_input_tokens, 19608L - 9088L)
})

test_that("parse_otel_turn_line snapshot — structure is stable", {
  skip_if(is.null(parse_otel_turn_line),
          "parse_otel_turn_line not found")

  result <- parse_otel_turn_line(GOOD_LINE)
  expect_snapshot(sort(names(result)))
})

# ── compute_cost ──────────────────────────────────────────────────────────────

test_that("compute_cost returns correct USD for known inputs", {
  skip_if(is.null(compute_cost), "compute_cost not found")

  # non_cached=10520, cached=9088, output=277  with gpt-5.4 pricing:
  # = (10520 * 0.40 + 9088 * 0.04 + 277 * 1.60) / 1e6
  # = (4208 + 363.52 + 443.2) / 1e6 = 5014.72 / 1e6 = 0.00501472
  expected <- (10520 * 0.40 + 9088 * 0.04 + 277 * 1.60) / 1e6
  result <- compute_cost(
    non_cached_input_tokens = 10520L,
    cached_input_tokens     = 9088L,
    output_tokens           = 277L,
    model                   = "gpt-5.4",
    pricing                 = MINIMAL_PRICING
  )
  expect_equal(result, expected, tolerance = 1e-9)
})

test_that("compute_cost returns NA with warning when model not in pricing", {
  skip_if(is.null(compute_cost), "compute_cost not found")

  expect_warning(
    result <- compute_cost(
      non_cached_input_tokens = 1000L,
      cached_input_tokens     = 0L,
      output_tokens           = 100L,
      model                   = "gpt-99-unknown",
      pricing                 = MINIMAL_PRICING
    ),
    regexp = "not found in codex_pricing\\.json"
  )
  expect_true(is.na(result))
})

test_that("compute_cost returns NA when pricing is NULL", {
  skip_if(is.null(compute_cost), "compute_cost not found")

  result <- compute_cost(1000L, 0L, 100L, "gpt-5.4", pricing = NULL)
  expect_true(is.na(result))
})

test_that("compute_cost snapshot — error message wording is stable", {
  skip_if(is.null(compute_cost), "compute_cost not found")

  expect_snapshot({
    withCallingHandlers(
      compute_cost(100L, 0L, 50L, "nonexistent-model", MINIMAL_PRICING),
      warning = function(w) {
        cat("WARNING:", conditionMessage(w), "\n")
        invokeRestart("muffleWarning")
      }
    )
  })
})

# ── detect_rotation ────────────────────────────────────────────────────────────

test_that("detect_rotation returns 0 when offset exceeds file size", {
  skip_if(is.null(detect_rotation), "detect_rotation not found")

  tmp <- tempfile()
  writeLines("hello", tmp)
  fsize <- file.info(tmp)$size
  result <- detect_rotation(tmp, fsize + 1000L)
  expect_equal(result, 0L)
  unlink(tmp)
})

test_that("detect_rotation returns offset when file is larger", {
  skip_if(is.null(detect_rotation), "detect_rotation not found")

  tmp <- tempfile()
  writeLines(rep("x", 100), tmp)
  fsize <- file.info(tmp)$size
  small_offset <- as.integer(fsize / 2)
  result <- detect_rotation(tmp, small_offset)
  expect_equal(result, small_offset)
  unlink(tmp)
})

test_that("detect_rotation returns 0 for non-existent file", {
  skip_if(is.null(detect_rotation), "detect_rotation not found")

  expect_equal(detect_rotation("/nonexistent/path/log.txt", 999L), 0L)
})

# ── aggregate_daily ────────────────────────────────────────────────────────────

test_that("aggregate_daily sums tokens correctly for 3 same-key turns", {
  skip_if(is.null(aggregate_daily), "aggregate_daily not found")

  turns <- tibble::tibble(
    timestamp               = c("2026-05-19T10:00:00Z",
                                 "2026-05-19T11:00:00Z",
                                 "2026-05-19T12:00:00Z"),
    canonical_project       = c("llmtelemetry", "llmtelemetry", "llmtelemetry"),
    model                   = c("gpt-5.4", "gpt-5.4", "gpt-5.4"),
    source                  = c("interactive", "interactive", "interactive"),
    input_tokens            = c(1000L, 2000L, 3000L),
    cached_input_tokens     = c(500L,  800L,  1200L),
    non_cached_input_tokens = c(500L,  1200L, 1800L),
    output_tokens           = c(100L,  200L,  300L),
    reasoning_output_tokens = c(50L,   80L,   120L),
    total_tokens            = c(1100L, 2200L, 3300L),
    est_cost_usd            = c(0.001, 0.002, 0.003)
  )
  result <- aggregate_daily(turns)
  expect_equal(nrow(result), 1L)
  expect_equal(result$n_turns,       3L)
  expect_equal(result$total_tokens,  6600L)
  expect_equal(result$output_tokens, 600L)
  expect_equal(result$est_cost_usd,  0.006, tolerance = 1e-9)
  expect_equal(result$date, "2026-05-19")
})

test_that("aggregate_daily splits across dates correctly", {
  skip_if(is.null(aggregate_daily), "aggregate_daily not found")

  turns <- tibble::tibble(
    timestamp               = c("2026-05-18T23:00:00Z", "2026-05-19T01:00:00Z"),
    canonical_project       = c("llm", "llm"),
    model                   = c("gpt-5.4", "gpt-5.4"),
    source                  = c("interactive", "interactive"),
    input_tokens            = c(100L, 200L),
    cached_input_tokens     = c(0L,   0L),
    non_cached_input_tokens = c(100L, 200L),
    output_tokens           = c(10L,  20L),
    reasoning_output_tokens = c(0L,   0L),
    total_tokens            = c(110L, 220L),
    est_cost_usd            = c(0.01, 0.02)
  )
  result <- aggregate_daily(turns)
  expect_equal(nrow(result), 2L)
  expect_equal(sort(result$date), c("2026-05-18", "2026-05-19"))
})

test_that("aggregate_daily snapshot — column names are stable", {
  skip_if(is.null(aggregate_daily), "aggregate_daily not found")

  turns <- tibble::tibble(
    timestamp               = "2026-05-19T10:00:00Z",
    canonical_project       = "testproject",
    model                   = "gpt-5.4",
    source                  = "interactive",
    input_tokens            = 100L,
    cached_input_tokens     = 10L,
    non_cached_input_tokens = 90L,
    output_tokens           = 20L,
    reasoning_output_tokens = 5L,
    total_tokens            = 120L,
    est_cost_usd            = 0.001
  )
  result <- aggregate_daily(turns)
  expect_snapshot(names(result))
})

test_that("aggregate_daily returns empty tibble for empty input", {
  skip_if(is.null(aggregate_daily), "aggregate_daily not found")

  result <- aggregate_daily(tibble::tibble())
  expect_equal(nrow(result), 0L)
})

# ── canonicalize_project (cwd sanitisation) ───────────────────────────────────

test_that("canonicalize_project extracts project name from /docs_gh/llmtelemetry path", {
  skip_if(is.null(canonicalize_project), "canonicalize_project not found")

  result <- canonicalize_project("/Users/johngavin/docs_gh/llmtelemetry/foo/bar")
  expect_equal(result, "llmtelemetry")
})

test_that("canonicalize_project handles NA and empty cwd", {
  skip_if(is.null(canonicalize_project), "canonicalize_project not found")

  expect_true(is.na(canonicalize_project(NA_character_)))
  expect_true(is.na(canonicalize_project("")))
})

test_that("canonicalize_project snapshot — output for known paths is stable", {
  skip_if(is.null(canonicalize_project), "canonicalize_project not found")

  paths <- c(
    "/Users/johngavin/docs_gh/llmtelemetry",
    "/Users/johngavin/docs_gh/llm",
    "/Users/johngavin/docs_gh/mycare",
    NA_character_
  )
  expect_snapshot(canonicalize_project(paths))
})

# ── stale pricing warning ─────────────────────────────────────────────────────

test_that("load_pricing warns when pricing is more than 30 days old", {
  skip_if(is.null(load_pricing), "load_pricing not found")

  tmp_dir <- tempdir()
  old_date <- format(Sys.Date() - 31L, "%Y-%m-%d")
  pricing_json <- jsonlite::toJSON(list(
    updated_at = old_date,
    models = list(
      `gpt-5.4` = list(
        input_per_mtok        = 0.40,
        cached_input_per_mtok = 0.04,
        output_per_mtok       = 1.60
      )
    )
  ), auto_unbox = TRUE)
  writeLines(pricing_json, file.path(tmp_dir, "codex_pricing.json"))

  expect_warning(
    load_pricing(tmp_dir),
    regexp = "days old"
  )
})

test_that("load_pricing does NOT warn when pricing is fresh", {
  skip_if(is.null(load_pricing), "load_pricing not found")

  tmp_dir <- tempdir()
  fresh_date <- format(Sys.Date(), "%Y-%m-%d")
  pricing_json <- jsonlite::toJSON(list(
    updated_at = fresh_date,
    models = list(
      `gpt-5.4` = list(
        input_per_mtok        = 0.40,
        cached_input_per_mtok = 0.04,
        output_per_mtok       = 1.60
      )
    )
  ), auto_unbox = TRUE)
  writeLines(pricing_json, file.path(tmp_dir, "codex_pricing.json"))

  # Should not warn
  expect_no_warning(load_pricing(tmp_dir))
})

# ── roborev join (offline mode) ───────────────────────────────────────────────

test_that("join_roborev marks all turns as interactive when roborev not available", {
  skip_if(is.null(join_roborev), "join_roborev not found")

  turns <- tibble::tibble(
    thread_id         = c("abc-123", "def-456"),
    canonical_project = c("llm", "llm"),
    model             = c("gpt-5.4", "gpt-5.4")
  )
  # roborev binary likely not on PATH in test env; expect graceful fallback
  suppressMessages(
    result <- join_roborev(turns)
  )
  expect_equal(result$source, c("interactive", "interactive"))
})

test_that("join_roborev snapshot — column names after join are stable", {
  skip_if(is.null(join_roborev), "join_roborev not found")

  turns <- tibble::tibble(
    thread_id         = "abc-123",
    canonical_project = "llm",
    model             = "gpt-5.4"
  )
  suppressMessages(result <- join_roborev(turns))
  expect_snapshot(sort(names(result)))
})
