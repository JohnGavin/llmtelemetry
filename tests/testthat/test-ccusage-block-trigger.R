# Tests for classify_block_trigger() and default_automation_windows()
# Phase 1 heuristic: time-window classification of 5-hour billing blocks.
#
# A block is "scheduled" iff its billing-window interval [block_start_hour,
# block_end_hour] is fully contained within a known automation window.
# All other blocks are "interactive".  This is deliberately approximate —
# mixed blocks (e.g. 05:00-10:00, which contains both the 09:00 roborev
# poller AND interactive morning work) are labelled interactive.

# ---- default_automation_windows -------------------------------------------

test_that("default_automation_windows returns a non-empty list", {
  w <- llmtelemetry:::default_automation_windows()
  expect_type(w, "list")
  expect_true(length(w) >= 1L)
})

test_that("each automation window has numeric start and end", {
  w <- llmtelemetry:::default_automation_windows()
  for (win in w) {
    expect_true(all(c("start", "end") %in% names(win)),
      info = "window must have 'start' and 'end' fields")
    expect_true(is.numeric(win$start) || is.integer(win$start))
    expect_true(is.numeric(win$end)   || is.integer(win$end))
    expect_true(win$end > win$start,
      info = "automation window end must be strictly after start")
  }
})

# ---- classify_block_trigger: scalar cases ----------------------------------

test_that("00:00-05:00 block (fully overnight) is scheduled", {
  # The 00:00-05:00 billing block falls entirely inside the overnight window
  # [0, 7].  This is the clean automated case (overnight digest, config_pulse).
  expect_identical(llmtelemetry:::classify_block_trigger(0L, 5L), "scheduled")
})

test_that("05:00-10:00 block straddling the 07:00 boundary is interactive", {
  # [5, 10] is NOT fully inside [0, 7] (10 > 7) -> interactive.
  # Documents the known Phase 1 approximation: the 09:00 roborev poller fires
  # in this block but the whole block cannot be tagged scheduled.
  expect_identical(llmtelemetry:::classify_block_trigger(5L, 10L), "interactive")
})

test_that("business-hours block containing 09:00 poller is interactive", {
  # roborev fires Mon-Fri at 09:00 inside the 05:00-10:00 billing block.
  # Because the block is not fully inside any automation window, Phase 1
  # attributes its entire cost to interactive.  This is the documented
  # approximation that Phase 2 (per-session provenance) will fix.
  expect_identical(llmtelemetry:::classify_block_trigger(5L, 10L), "interactive")
})

test_that("10:00-15:00 block (contains 13:00 poller) is interactive", {
  expect_identical(llmtelemetry:::classify_block_trigger(10L, 15L), "interactive")
})

test_that("15:00-20:00 block (contains 17:00 poller) is interactive", {
  expect_identical(llmtelemetry:::classify_block_trigger(15L, 20L), "interactive")
})

test_that("20:00-01:00 block (midnight crossover) is interactive", {
  # The 20:00-01:00 billing block crosses midnight; pass end_hour=25 to
  # represent 01:00 of the following day.  [20, 25] is NOT inside [0, 7].
  expect_identical(llmtelemetry:::classify_block_trigger(20L, 25L), "interactive")
})

# ---- classify_block_trigger: vectorized -------------------------------------

test_that("classify_block_trigger is vectorized over all five daily blocks", {
  # The five billing blocks that partition a 24-hour day (with 20:00 block
  # represented as ending at hour 25 for midnight crossover).
  start_hours <- c(0L,  5L,  10L, 15L, 20L)
  end_hours   <- c(5L, 10L,  15L, 20L, 25L)
  result <- llmtelemetry:::classify_block_trigger(start_hours, end_hours)
  expect_length(result, 5L)
  expect_identical(result[[1L]], "scheduled")   # overnight block only
  expect_true(all(result[-1L] == "interactive"))  # remaining blocks
})

test_that("classify_block_trigger returns character vector", {
  result <- llmtelemetry:::classify_block_trigger(c(0L, 5L), c(5L, 10L))
  expect_type(result, "character")
})

# ---- classify_block_trigger: custom windows --------------------------------

test_that("custom windows parameter overrides default", {
  # Make the 10:00-15:00 block scheduled by defining a matching window.
  custom_w <- list(list(start = 10L, end = 15L))
  expect_identical(
    llmtelemetry:::classify_block_trigger(10L, 15L, windows = custom_w),
    "scheduled"
  )
  # The overnight block is no longer scheduled under the custom definition.
  expect_identical(
    llmtelemetry:::classify_block_trigger(0L, 5L, windows = custom_w),
    "interactive"
  )
})

test_that("empty windows list makes all blocks interactive", {
  start_hours <- c(0L, 5L, 10L)
  end_hours   <- c(5L, 10L, 15L)
  result <- llmtelemetry:::classify_block_trigger(
    start_hours, end_hours, windows = list()
  )
  expect_true(all(result == "interactive"))
})

test_that("multiple windows allow multiple scheduled blocks", {
  multi_w <- list(
    list(start = 0L,  end = 7L),   # overnight
    list(start = 20L, end = 25L)   # late-night
  )
  expect_identical(
    llmtelemetry:::classify_block_trigger(0L, 5L, windows = multi_w),
    "scheduled"
  )
  expect_identical(
    llmtelemetry:::classify_block_trigger(20L, 25L, windows = multi_w),
    "scheduled"
  )
  expect_identical(
    llmtelemetry:::classify_block_trigger(5L, 10L, windows = multi_w),
    "interactive"
  )
})
