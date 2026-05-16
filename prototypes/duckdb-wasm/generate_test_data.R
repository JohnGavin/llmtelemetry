#!/usr/bin/env Rscript
# generate_test_data.R
# Phase 0 feasibility prototype for llmtelemetry issue #83.
# Writes a 20-row Parquet to test.parquet using duckdb (arrow not in nix shell).
# Run via:
#   nix-shell /Users/johngavin/docs_gh/llmtelemetry/default.nix \
#     --run "Rscript /Users/johngavin/docs_gh/llmtelemetry-proto83/prototypes/duckdb-wasm/generate_test_data.R"

library(DBI)
library(duckdb)

set.seed(42)
df <- data.frame(
  project       = c(rep("llm", 7), rep("llmtelemetry", 6), rep("footbet", 5), rep("mycare", 2)),
  date          = as.character(seq(as.Date("2026-05-01"), by = "day", length.out = 20)),
  session_id    = sprintf("sess-%04d", 1:20),
  cost_usd      = round(runif(20, 0.5, 25), 2),
  input_tokens  = as.integer(sample(1000L:1000000L, 20)),
  output_tokens = as.integer(sample(100L:100000L, 20)),
  valid_from    = format(as.POSIXct("2026-05-15 12:00:00", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
  stringsAsFactors = FALSE
)

# Resolve output path to same directory as this script
args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[startsWith(args, "--file=")]
if (length(file_arg)) {
  script_dir <- dirname(normalizePath(sub("--file=", "", file_arg)))
} else {
  script_dir <- "."
}
out_path <- file.path(script_dir, "test.parquet")
cat("Writing to:", out_path, "\n")

# Use duckdb to write parquet — arrow not available in this nix shell
con <- dbConnect(duckdb(), ":memory:")
dbWriteTable(con, "test_data", df, overwrite = TRUE)

# COPY to parquet via DuckDB SQL — use DBI::dbExecute
DBI::dbExecute(con, sprintf(
  "COPY test_data TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)",
  out_path
))
dbDisconnect(con)

# Verify
stopifnot(file.exists(out_path))
sz <- file.info(out_path)$size
cat(sprintf("Written: %s (%.1f KB)\n", out_path, sz / 1024))

# Round-trip verify
con2 <- dbConnect(duckdb(), ":memory:")
verify <- DBI::dbGetQuery(con2, sprintf("SELECT COUNT(*) AS n FROM '%s'", out_path))
cat(sprintf("Round-trip row count: %d (expected 20)\n", verify$n))
stopifnot(verify$n == 20L)

counts <- DBI::dbGetQuery(con2, sprintf(
  "SELECT project, COUNT(*) AS n FROM '%s' GROUP BY project ORDER BY project",
  out_path
))
print(counts)
dbDisconnect(con2)
cat("OK — test.parquet is well-formed.\n")
