library(rix)
rix(
  r_ver = "4.3.3",
  r_pkgs = c("duckdb", "dplyr", "ggplot2", "jsonlite", "blastula", "here", "scales", "tibble", "lubridate"),
  ide = "none",
  project_path = "/Users/johngavin/docs_gh/proj/data/llm/telemetry",
  overwrite = TRUE,
  shell_hook = "echo 'Welcome to llmtelemetry app shell'"
)
