# Adversarial QA tests for llmtelemetry exported functions
# Tests attack vectors: NULL, wrong types, empty data, NA, missing columns,
# negative numbers, Inf, NaN, zero-length, injection strings

# ============================================================================
# Helper: create minimal valid data structures
# ============================================================================

make_daily_data <- function(n = 5) {
  tibble::tibble(
    date = as.character(Sys.Date() - seq_len(n)),
    totalCost = runif(n, 0, 10),
    totalTokens = as.integer(runif(n, 1000, 50000)),
    project = rep("test-project", n)
  )
}

make_daily_data_with_models <- function(n = 3) {
  dd <- make_daily_data(n)
  dd$modelBreakdowns <- lapply(seq_len(n), function(i) {
    data.frame(
      modelName = c("claude-opus-4-6", "claude-sonnet-4-5-20250929"),
      cost = c(runif(1, 0, 5), runif(1, 0, 2)),
      inputTokens = c(1000L, 500L),
      outputTokens = c(2000L, 1000L),
      cacheCreationTokens = c(100L, 50L),
      cacheReadTokens = c(200L, 100L),
      stringsAsFactors = FALSE
    )
  })
  dd
}

make_blocks_data <- function(n = 5) {
  tibble::tibble(
    timestamp = Sys.time() - seq_len(n) * 3600,
    totalTokens = as.integer(runif(n, 1000, 20000)),
    costUSD = runif(n, 0, 5)
  )
}

# ============================================================================
# normalize_to_char_vec
# ============================================================================

test_that("normalize_to_char_vec handles NULL", {

expect_identical(normalize_to_char_vec(NULL), character(0))
})

test_that("normalize_to_char_vec handles empty list", {
  expect_identical(normalize_to_char_vec(list()), character(0))
})

test_that("normalize_to_char_vec handles single string", {
  expect_identical(normalize_to_char_vec("model-a"), "model-a")
})

test_that("normalize_to_char_vec handles character vector", {
  expect_identical(
    normalize_to_char_vec(c("model-a", "model-b")),
    c("model-a", "model-b")
  )
})

test_that("normalize_to_char_vec handles nested list", {
  expect_identical(
    normalize_to_char_vec(list("model-a", "model-b")),
    c("model-a", "model-b")
  )
})

test_that("normalize_to_char_vec handles numeric input", {
  expect_identical(normalize_to_char_vec(42), "42")
})

test_that("normalize_to_char_vec handles NA", {
  result <- normalize_to_char_vec(NA)
  expect_type(result, "character")
})

test_that("normalize_to_char_vec handles logical", {
  result <- normalize_to_char_vec(TRUE)
  expect_type(result, "character")
})

# ============================================================================
# parse_ccusage_json
# ============================================================================

test_that("parse_ccusage_json returns NULL for NULL input", {
  expect_null(parse_ccusage_json(NULL))
})

test_that("parse_ccusage_json returns NULL for empty list", {
  expect_null(parse_ccusage_json(list()))
})

test_that("parse_ccusage_json returns NULL for missing projects key", {
  expect_null(parse_ccusage_json(list(foo = "bar")))
})

test_that("parse_ccusage_json returns NULL for empty projects", {
  expect_null(parse_ccusage_json(list(projects = list())))
})

test_that("parse_ccusage_json returns NULL when filter matches nothing", {
  json_data <- list(
    projects = list(
      "project-a" = data.frame(date = "2026-01-01", totalCost = 1.0)
    )
  )
  expect_null(parse_ccusage_json(json_data, project_filter = "nonexistent"))
})

test_that("parse_ccusage_json parses valid data", {
  json_data <- list(
    projects = list(
      "project-a" = data.frame(
        date = c("2026-01-01", "2026-01-02"),
        totalCost = c(1.0, 2.0),
        totalTokens = c(100L, 200L)
      )
    )
  )
  result <- parse_ccusage_json(json_data)
  expect_s3_class(result, "tbl_df")
  expect_true("project" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("parse_ccusage_json handles project with NULL data", {
  json_data <- list(
    projects = list(
      "project-a" = NULL,
      "project-b" = data.frame(date = "2026-01-01", totalCost = 1.0)
    )
  )
  result <- parse_ccusage_json(json_data)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
})

# ============================================================================
# load_cached_ccusage
# ============================================================================

test_that("load_cached_ccusage returns NULL for nonexistent directory", {
  result <- load_cached_ccusage("daily", cache_dir = "/nonexistent/path")
  expect_null(result)
})

test_that("load_cached_ccusage validates type argument", {
  expect_error(load_cached_ccusage("invalid_type"))
})

test_that("load_cached_ccusage returns NULL for empty directory", {
  tmp <- tempdir()
  result <- load_cached_ccusage("daily", cache_dir = tmp)
  expect_null(result)
})

# ============================================================================
# summarize_llm_usage
# ============================================================================

test_that("summarize_llm_usage handles NULL input", {
  result <- summarize_llm_usage(NULL)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("summarize_llm_usage handles empty tibble", {
  result <- summarize_llm_usage(tibble::tibble())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("summarize_llm_usage handles valid data", {
  dd <- make_daily_data(5)
  result <- summarize_llm_usage(dd)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 6)
  expect_true(all(c("metric", "value") %in% names(result)))
})

test_that("summarize_llm_usage handles data with NAs", {
  dd <- make_daily_data(3)
  dd$totalCost[1] <- NA
  dd$totalTokens[2] <- NA
  result <- summarize_llm_usage(dd)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 6)
})

test_that("summarize_llm_usage handles single row", {
  dd <- make_daily_data(1)
  result <- summarize_llm_usage(dd)
  expect_equal(nrow(result), 6)
})

test_that("summarize_llm_usage handles zero costs", {
  dd <- make_daily_data(3)
  dd$totalCost <- 0
  result <- summarize_llm_usage(dd)
  expect_s3_class(result, "tbl_df")
})

# ============================================================================
# get_model_breakdown
# ============================================================================

test_that("get_model_breakdown handles NULL", {
  expect_null(get_model_breakdown(NULL))
})

test_that("get_model_breakdown handles empty tibble", {
  expect_null(get_model_breakdown(tibble::tibble()))
})

test_that("get_model_breakdown handles data without modelBreakdowns column", {
  dd <- make_daily_data(3)
  expect_null(get_model_breakdown(dd))
})

test_that("get_model_breakdown handles valid data with model breakdowns", {
  dd <- make_daily_data_with_models(3)
  result <- get_model_breakdown(dd)
  expect_s3_class(result, "tbl_df")
  expect_true("modelName" %in% names(result))
  expect_true("total_cost" %in% names(result))
})

test_that("get_model_breakdown handles empty breakdowns in some rows", {
  dd <- make_daily_data_with_models(3)
  dd$modelBreakdowns[[2]] <- data.frame()
  result <- get_model_breakdown(dd)
  expect_s3_class(result, "tbl_df")
})

# ============================================================================
# find_activity_gaps
# ============================================================================

test_that("find_activity_gaps handles NULL", {
  expect_null(find_activity_gaps(NULL))
})

test_that("find_activity_gaps handles empty tibble", {
  expect_null(find_activity_gaps(tibble::tibble()))
})

test_that("find_activity_gaps handles single date", {
  dd <- make_daily_data(1)
  expect_null(find_activity_gaps(dd))
})

test_that("find_activity_gaps finds gaps in data", {
  dd <- tibble::tibble(
    date = c("2026-01-01", "2026-01-02", "2026-01-05", "2026-01-06"),
    totalCost = c(1, 2, 3, 4)
  )
  result <- find_activity_gaps(dd)
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true(all(c("gap_start", "gap_end", "gap_days") %in% names(result)))
})

test_that("find_activity_gaps returns empty tibble for consecutive dates", {
  dd <- tibble::tibble(
    date = as.character(Sys.Date() - 0:4),
    totalCost = 1:5
  )
  result <- find_activity_gaps(dd)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("find_activity_gaps handles duplicate dates", {
  dd <- tibble::tibble(
    date = c("2026-01-01", "2026-01-01", "2026-01-03"),
    totalCost = c(1, 2, 3)
  )
  result <- find_activity_gaps(dd)
  expect_s3_class(result, "tbl_df")
})

# ============================================================================
# show_usage_progress - input validation via checkmate
# ============================================================================

test_that("show_usage_progress rejects NULL current", {
  expect_error(show_usage_progress(NULL, 100))
})

test_that("show_usage_progress rejects negative current", {
  expect_error(show_usage_progress(-1, 100))
})

test_that("show_usage_progress rejects zero limit", {
  expect_error(show_usage_progress(50, 0))
})

test_that("show_usage_progress rejects negative limit", {
  expect_error(show_usage_progress(50, -10))
})

test_that("show_usage_progress rejects non-numeric current", {
  expect_error(show_usage_progress("abc", 100))
})

test_that("show_usage_progress rejects Inf current", {
  expect_error(show_usage_progress(Inf, 100))
})

test_that("show_usage_progress rejects NaN current", {
  expect_error(show_usage_progress(NaN, 100))
})

test_that("show_usage_progress rejects NA current", {
  expect_error(show_usage_progress(NA, 100))
})

test_that("show_usage_progress rejects non-string label", {
  expect_error(show_usage_progress(50, 100, label = 42))
})

test_that("show_usage_progress rejects non-logical show_tokens", {
  expect_error(show_usage_progress(50, 100, show_tokens = "yes"))
})

test_that("show_usage_progress works with valid inputs", {
  result <- show_usage_progress(50, 100, label = "Test")
  expect_equal(result, 50)
})

test_that("show_usage_progress handles current > limit (over 100%)", {
  result <- show_usage_progress(150, 100)
  expect_equal(result, 100) # capped at 100
})

test_that("show_usage_progress handles zero current", {
  result <- show_usage_progress(0, 100)
  expect_equal(result, 0)
})

test_that("show_usage_progress handles show_tokens with valid token params", {
  result <- show_usage_progress(50, 100, show_tokens = TRUE,
                                 tokens_current = 5000, tokens_limit = 10000)
  expect_equal(result, 50)
})

# ============================================================================
# get_current_block_window
# ============================================================================

test_that("get_current_block_window works with default (Sys.time())", {
  result <- get_current_block_window()
  expect_type(result, "list")
  expect_true(all(c("block_start", "block_end", "time_remaining") %in% names(result)))
  expect_s3_class(result$block_start, "POSIXct")
  expect_s3_class(result$block_end, "POSIXct")
})

test_that("get_current_block_window calculates correct 5-hour blocks", {
  # Test at hour 7 -> should be in block 5-10
  test_time <- as.POSIXct("2026-01-15 07:30:00")
  result <- get_current_block_window(test_time)
  expect_equal(as.integer(format(result$block_start, "%H")), 5)
})

test_that("get_current_block_window calculates correct block at midnight", {
  test_time <- as.POSIXct("2026-01-15 00:30:00")
  result <- get_current_block_window(test_time)
  expect_equal(as.integer(format(result$block_start, "%H")), 0)
})

test_that("get_current_block_window calculates correct block at hour 23", {
  test_time <- as.POSIXct("2026-01-15 23:30:00")
  result <- get_current_block_window(test_time)
  expect_equal(as.integer(format(result$block_start, "%H")), 20)
})

test_that("get_current_block_window rejects invalid input types", {
  expect_error(get_current_block_window(42))
  expect_error(get_current_block_window(list()))
  expect_error(get_current_block_window(data.frame()))
})

test_that("get_current_block_window accepts character date string", {
  result <- get_current_block_window("2026-01-15 12:00:00")
  expect_type(result, "list")
})

test_that("get_current_block_window rejects NA", {
  expect_error(get_current_block_window(NA))
})

test_that("get_current_block_window rejects invalid date string", {
  expect_error(get_current_block_window("not-a-date"))
})

# ============================================================================
# calculate_block_usage
# ============================================================================

test_that("calculate_block_usage handles NULL blocks_data", {
  window <- get_current_block_window()
  result <- calculate_block_usage(NULL, window)
  expect_type(result, "list")
  expect_equal(result$tokens_used, 0)
})

test_that("calculate_block_usage handles empty blocks_data", {
  window <- get_current_block_window()
  result <- calculate_block_usage(tibble::tibble(), window)
  expect_type(result, "list")
  expect_equal(result$tokens_used, 0)
})

test_that("calculate_block_usage rejects invalid window", {
  expect_error(calculate_block_usage(make_blocks_data(), list()))
  expect_error(calculate_block_usage(make_blocks_data(), list(foo = 1)))
})

test_that("calculate_block_usage rejects non-list window", {
  expect_error(calculate_block_usage(make_blocks_data(), "not a list"))
})

test_that("calculate_block_usage works with valid data", {
  blocks <- make_blocks_data(3)
  window <- get_current_block_window()
  result <- calculate_block_usage(blocks, window)
  expect_type(result, "list")
  expect_true(all(c("tokens_used", "tokens_limit", "usage_pct") %in% names(result)))
})

# ============================================================================
# show_daily_progress - input validation
# ============================================================================

test_that("show_daily_progress rejects negative daily_limit", {
  expect_error(show_daily_progress(daily_limit = -10))
})

test_that("show_daily_progress rejects non-numeric daily_limit", {
  expect_error(show_daily_progress(daily_limit = "fifty"))
})

test_that("show_daily_progress rejects invalid cache_dir", {
  expect_error(show_daily_progress(cache_dir = "/definitely/not/a/real/path"))
})

# ============================================================================
# show_weekly_progress - input validation
# ============================================================================

test_that("show_weekly_progress rejects negative weekly_limit", {
  expect_error(show_weekly_progress(weekly_limit = -10))
})

test_that("show_weekly_progress rejects non-numeric weekly_limit", {
  expect_error(show_weekly_progress(weekly_limit = "hundred"))
})

# ============================================================================
# show_usage_dashboard - input validation
# ============================================================================

test_that("show_usage_dashboard rejects negative daily_limit", {
  expect_error(show_usage_dashboard(daily_limit = -10))
})

test_that("show_usage_dashboard rejects non-logical show_max5", {
  expect_error(show_usage_dashboard(show_max5 = "yes"))
})

# ============================================================================
# get_block_history - input validation
# ============================================================================

test_that("get_block_history rejects zero days", {
  expect_error(get_block_history(days = 0))
})

test_that("get_block_history rejects negative days", {
  expect_error(get_block_history(days = -1))
})

test_that("get_block_history rejects non-integer days", {
  expect_error(get_block_history(days = 1.5))
})

test_that("get_block_history rejects character days", {
  expect_error(get_block_history(days = "three"))
})

# ============================================================================
# append_to_duckdb - edge cases
# ============================================================================

test_that("append_to_duckdb handles NULL data", {
  expect_message(append_to_duckdb(NULL), "No data to append")
})

test_that("append_to_duckdb handles empty tibble", {
  expect_message(append_to_duckdb(tibble::tibble()), "No data to append")
})

test_that("append_to_duckdb writes to temp database", {
  tmp_db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp_db), add = TRUE)

  data <- tibble::tibble(
    date = "2026-01-01",
    totalCost = 1.5,
    totalTokens = 1000L
  )
  result <- append_to_duckdb(data, db_path = tmp_db, table_name = "test_table")
  expect_true(file.exists(tmp_db))
})

# ============================================================================
# query_latest_usage - edge cases
# ============================================================================

test_that("query_latest_usage returns NULL for nonexistent db", {
  result <- query_latest_usage(db_path = "/nonexistent/db.duckdb")
  expect_null(result)
})

# ============================================================================
# Injection / boundary tests
# ============================================================================

test_that("parse_ccusage_json handles injection in project names", {
  json_data <- list(
    projects = list(
      "'; DROP TABLE--" = data.frame(
        date = "2026-01-01",
        totalCost = 1.0,
        totalTokens = 100L
      )
    )
  )
  result <- parse_ccusage_json(json_data)
  expect_s3_class(result, "tbl_df")
  expect_equal(result$project[1], "'; DROP TABLE--")
})

test_that("summarize_llm_usage handles extreme values", {
  dd <- tibble::tibble(
    date = c("2026-01-01", "2026-01-02"),
    totalCost = c(.Machine$double.xmax / 2, .Machine$double.xmax / 2),
    totalTokens = c(.Machine$integer.max, .Machine$integer.max)
  )
  result <- summarize_llm_usage(dd)
  expect_s3_class(result, "tbl_df")
})

test_that("find_activity_gaps handles very long date range", {
  dates <- as.character(seq(as.Date("2020-01-01"), as.Date("2026-12-31"), by = "month"))
  dd <- tibble::tibble(
    date = dates,
    totalCost = rep(1, length(dates))
  )
  result <- find_activity_gaps(dd)
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0) # monthly data has gaps
})
