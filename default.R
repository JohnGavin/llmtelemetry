library(rix)
rix(
  r_pkgs = c(
    "duckdb", "duckplyr", "dplyr", "tidyr",
    "ggplot2", "jsonlite", "blastula", "here",
    "scales", "tibble", "lubridate",
    "checkmate", "cli", "DBI", "purrr",
    "pkgload", "devtools", "testthat"
  ),
  ide = "none",
  date = "2026-01-05",
  project_path = "/Users/johngavin/docs_gh/llmtelemetry",
  overwrite = TRUE,
  shell_hook = "echo 'Welcome to llmtelemetry app shell'; source $PWD/.nix-shellhook.sh 2>/dev/null || true"
)
