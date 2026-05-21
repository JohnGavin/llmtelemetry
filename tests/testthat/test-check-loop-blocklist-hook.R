# test-check-loop-blocklist-hook.R — Integration tests for
# inst/hooks/check_loop_blocklist.sh
#
# Uses system2() to invoke the hook with synthetic stdin JSON.
# Skipped on Windows (hook uses bash).

skip_on_os("windows")

# ---- helper ------------------------------------------------------------------

run_hook <- function(json_str, blocklist_path = NULL, extra_env = character()) {
  hook_path <- system.file("hooks", "check_loop_blocklist.sh",
                           package = "llmtelemetry")
  if (!nzchar(hook_path)) {
    # dev fallback
    hook_path <- here::here("inst", "hooks", "check_loop_blocklist.sh")
  }

  if (!file.exists(hook_path)) skip("Hook script not found")

  if (!nzchar(Sys.which("jq"))) skip("jq not available in PATH")
  if (!nzchar(Sys.which("bash"))) skip("bash not available in PATH")

  env_vars <- if (!is.null(blocklist_path)) {
    c(sprintf("ROBOREV_BLOCKLIST_PATH=%s", blocklist_path), extra_env)
  } else {
    extra_env
  }

  # Write JSON to a temp file so we can pipe it
  tmp_input <- withr::local_tempfile(fileext = ".json")
  writeLines(json_str, tmp_input)

  result <- suppressWarnings(
    system2(
      "bash",
      args   = hook_path,
      stdin  = tmp_input,
      stdout = TRUE,
      stderr = TRUE,
      env    = env_vars,
      wait   = TRUE
    )
  )
  status <- attr(result, "status")
  list(
    output = result,
    status = if (is.null(status)) 0L else as.integer(status)
  )
}

make_task_json <- function(prompt, extra_files = NULL) {
  # Build a PreToolUse JSON for a Task tool call
  jsonlite::toJSON(
    list(
      tool_name  = "Task",
      tool_input = list(
        prompt = prompt
      )
    ),
    auto_unbox = TRUE
  )
}

make_blocklist_file <- function(
  path,
  primary_file = "R/sanitize.R",
  tier         = "block",
  hash         = "testhash001",
  cycles       = 6L,
  summary      = "Sanitizer does not handle NULL bytes in the narrow edge case"
) {
  entry <- list(
    content_hash         = hash,
    severity             = "major",
    primary_file         = primary_file,
    summary              = summary,
    cycles               = cycles,
    tier                 = tier,
    ack_by               = NULL,
    ack_at               = NULL,
    ack_reason           = NULL,
    ack_until            = NULL,
    updated_at           = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  )
  jsonlite::write_json(list(entry), path, auto_unbox = TRUE, pretty = TRUE)
}

# ============================================================================
# 1. Exit 0 on non-Task tool (hook is not applicable)
# ============================================================================

test_that("hook exits 0 for non-Task tool calls", {
  json <- jsonlite::toJSON(
    list(tool_name = "Bash", tool_input = list(command = "ls -la")),
    auto_unbox = TRUE
  )
  result <- run_hook(json, blocklist_path = "/tmp/no_file_here.json")
  expect_equal(result$status, 0L)
})

# ============================================================================
# 2. Exit 0 when blocklist file does not exist
# ============================================================================

test_that("hook exits 0 when blocklist file does not exist", {
  json   <- make_task_json("Fix something in R/rollup_costs.R")
  result <- run_hook(json, blocklist_path = "/tmp/nonexistent_blocklist_xyz.json")
  expect_equal(result$status, 0L)
})

# ============================================================================
# 3. Exit 0 when no files match
# ============================================================================

test_that("hook exits 0 when file mentioned is not in blocklist", {
  bl_path <- withr::local_tempfile(fileext = ".json")
  make_blocklist_file(bl_path, primary_file = "R/sanitize.R")

  json   <- make_task_json("Please update R/rollup_costs.R with better validation")
  result <- run_hook(json, blocklist_path = bl_path)
  expect_equal(result$status, 0L)
})

# ============================================================================
# 4. Exit 2 when matched file is in blocklist
# ============================================================================

test_that("hook exits 2 when prompt mentions a blocked file", {
  bl_path <- withr::local_tempfile(fileext = ".json")
  make_blocklist_file(bl_path, primary_file = "R/sanitize.R", tier = "block", cycles = 6L)

  json   <- make_task_json("Fix the issue in R/sanitize.R that causes the test to fail")
  result <- run_hook(json, blocklist_path = bl_path)
  expect_equal(result$status, 2L)
})

# ============================================================================
# 5. Stderr contains hash and cycle count on block
# ============================================================================

test_that("hook stderr contains hash and cycle count when blocked", {
  bl_path <- withr::local_tempfile(fileext = ".json")
  make_blocklist_file(
    bl_path,
    primary_file = "R/sanitize.R",
    tier    = "block",
    cycles  = 8L,
    hash    = "deadbeef42"
  )

  json   <- make_task_json("Fix R/sanitize.R")
  result <- run_hook(json, blocklist_path = bl_path)

  combined_output <- paste(result$output, collapse = "\n")
  expect_match(combined_output, "deadbeef42", fixed = TRUE)
  expect_match(combined_output, "8", fixed = TRUE)
})

# ============================================================================
# 6. Stderr contains ack command example on block
# ============================================================================

test_that("hook stderr mentions ack command when blocked", {
  bl_path <- withr::local_tempfile(fileext = ".json")
  make_blocklist_file(bl_path, primary_file = "R/sanitize.R", tier = "escalate")

  json   <- make_task_json("Fix R/sanitize.R now")
  result <- run_hook(json, blocklist_path = bl_path)

  combined_output <- paste(result$output, collapse = "\n")
  expect_match(combined_output, "roborev_loop_ack", fixed = TRUE)
})

# ============================================================================
# 7. Exit 0 when blocklist is empty JSON array
# ============================================================================

test_that("hook exits 0 when blocklist is an empty array", {
  bl_path <- withr::local_tempfile(fileext = ".json")
  writeLines("[]", bl_path)

  json   <- make_task_json("Fix R/sanitize.R")
  result <- run_hook(json, blocklist_path = bl_path)
  expect_equal(result$status, 0L)
})

# ============================================================================
# 8. Missing prompt field exits 0 gracefully
# ============================================================================

test_that("hook exits 0 when Task prompt is missing from JSON", {
  json <- jsonlite::toJSON(
    list(tool_name = "Task", tool_input = list(description = "no prompt key")),
    auto_unbox = TRUE
  )
  result <- run_hook(json, blocklist_path = "/tmp/no_file.json")
  expect_equal(result$status, 0L)
})
