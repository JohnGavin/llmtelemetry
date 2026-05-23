#!/usr/bin/env Rscript
# check_shinylive_parse.R
#
# Build-time guard: extract EVERY {shinylive-r} chunk from a .qmd file and
# parse each as R code. Exit 0 only if ALL chunks parse cleanly. Exit 1 with a
# clear message identifying the failing chunk on any parse error.
#
# Usage:
#   Rscript inst/scripts/check_shinylive_parse.R vignettes/dashboard_shinylive.qmd
#
# Designed to run in CI BEFORE quarto render so a broken chunk fails the build
# pre-deploy (preventing a repeat of issue #156).
#
# The script:
#   1. Reads the .qmd file
#   2. Locates ALL ```{shinylive-r} opening fences
#   3. For each, locates the next closing ``` fence
#   4. Extracts the chunk body (excluding the fence lines)
#   5. Writes the body to a temp file
#   6. Calls base::parse() on it
#   7. Repeats for every chunk
#   8. Exits 0 only if ALL chunks parse; exits 1 on the FIRST parse error,
#      reporting which chunk (index + approximate line number)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat("Usage: Rscript check_shinylive_parse.R <path-to.qmd>\n", file = stderr())
  quit(status = 1)
}

qmd_path <- args[[1]]

if (!file.exists(qmd_path)) {
  cat(sprintf(
    "ERROR: file not found: %s\n", qmd_path
  ), file = stderr())
  quit(status = 1)
}

lines <- readLines(qmd_path, warn = FALSE)

# --- Locate ALL {shinylive-r} chunk fences -----------------------------------
# Opening fence: a line that is exactly ```{shinylive-r} (possibly with options)
open_fence_pattern  <- "^```\\{shinylive-r\\}"
close_fence_pattern <- "^```\\s*$"

open_indices <- grep(open_fence_pattern, lines)

if (length(open_indices) == 0) {
  cat(sprintf(
    "ERROR: No {shinylive-r} chunk found in %s\n", qmd_path
  ), file = stderr())
  quit(status = 1)
}

cat(sprintf(
  "INFO: Found %d {shinylive-r} chunk(s) in %s\n",
  length(open_indices), qmd_path
))

# --- Iterate over every chunk and parse ---------------------------------------
for (chunk_num in seq_along(open_indices)) {
  open_line <- open_indices[[chunk_num]]

  # Closing fence: first ``` on its own line AFTER the opening fence
  after_open <- seq(open_line + 1L, length(lines))
  close_candidates <- after_open[grepl(close_fence_pattern, lines[after_open])]

  if (length(close_candidates) == 0) {
    cat(sprintf(
      "ERROR: No closing ``` fence found after line %d (chunk %d) in %s\n",
      open_line, chunk_num, qmd_path
    ), file = stderr())
    quit(status = 1)
  }

  close_line <- close_candidates[[1]]

  # Body is lines strictly between the two fences
  body_lines <- lines[seq(open_line + 1L, close_line - 1L)]

  # Strip chunk options (#| ...) — they are not R code
  body_lines <- body_lines[!grepl("^#\\|", body_lines)]

  # Write chunk body to a temp file and parse
  tmp <- tempfile(fileext = ".R")

  writeLines(body_lines, tmp)

  result <- tryCatch(
    {
      parse(file = tmp)
      NULL  # success
    },
    error = function(e) e
  )

  if (is.null(result)) {
    cat(sprintf(
      "OK: chunk %d/%d (opening fence at line %d) in %s parses cleanly (%d lines of R code)\n",
      chunk_num, length(open_indices), open_line, qmd_path,
      length(body_lines)
    ))
    unlink(tmp)
  } else {
    cat(sprintf(
      "PARSE ERROR in {shinylive-r} chunk %d/%d (opening fence at line %d) in %s\n",
      chunk_num, length(open_indices), open_line, qmd_path
    ), file = stderr())
    cat(sprintf(
      "  Chunk body written to: %s\n", tmp
    ), file = stderr())
    cat(sprintf(
      "  Error: %s\n", conditionMessage(result)
    ), file = stderr())
    cat(
      "  Fix the syntax error in the {shinylive-r} chunk before deploying.\n",
      file = stderr()
    )
    # Keep temp file for inspection when running interactively
    quit(status = 1)
  }
}

cat(sprintf(
  "OK: all %d {shinylive-r} chunk(s) in %s parse cleanly\n",
  length(open_indices), qmd_path
))
quit(status = 0)
