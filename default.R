library(rix)
rix(
  r_pkgs = c(
    "duckdb", "duckplyr", "dplyr", "tidyr",
    "ggplot2", "jsonlite", "blastula", "here",
    "scales", "tibble", "lubridate",
    "checkmate", "cli", "DBI", "purrr",
    "pkgload", "devtools", "testthat",
    "otel", "shinylive", "httr2"
  ),
  system_pkgs = c("cmake", "protobuf_21", "pkg-config", "curl", "zlib"),
  ide = "none",
  date = "2026-01-05",
  project_path = ".",
  overwrite = TRUE,
  shell_hook = "echo 'Welcome to llmtelemetry app shell'; source $PWD/scripts/nix-shellhook.sh 2>/dev/null || true"
)
