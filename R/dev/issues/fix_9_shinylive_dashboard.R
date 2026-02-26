# Issue #9: Add Shinylive interactive telemetry dashboard
# PR #10: feat/shinylive-dashboard (merged to main)
# Follow-up: fix/dashboard-ci-and-check
#
# What was done:
#   1. Created inst/scripts/export_dashboard_data.R
#      - Flattens nested ccusage JSON to flat arrays
#      - Exports Gemini DuckDB tables to JSON (using dbReadTable, not tbl/dbplyr)
#      - Parses cmonitor text summary via regex
#      - Output: 6 JSON files (~73 KB total) in vignettes/data/
#
#   2. Created vignettes/dashboard_shinylive.qmd
#      - 5-tab Shinylive dashboard (Overview, Cost Trends, Tokens, Sessions, Blocks)
#      - Uses plotly (NOT ggplot2 - avoids munsell/WebR issue)
#      - All data loaded from JSON via relative URLs
#      - Interactive: sliders, DT tables, plotly rangesliders, heatmaps
#
#   3. Created .github/workflows/deploy-dashboard.yaml
#      - Triggers on push to main (vignettes/** or inst/extdata/**)
#      - Installs R + Quarto + shinylive extension
#      - Runs export script, renders dashboard, deploys to GitHub Pages
#
#   4. Updated inst/scripts/send_daily_email.R
#      - Dashboard link: johngavin.github.io/llm/... -> johngavin.github.io/llmtelemetry/
#
#   5. Created .gitignore and .Rbuildignore
#      - Excludes nix-shell-root, generated dashboard files, quarto cache
#
# CI fix (follow-up commit):
#   - export_dashboard_data.R: replaced tbl() |> collect() with dbReadTable()
#     to remove dbplyr dependency (not needed in CI)
#   - .Rbuildignore: added entries for shinylive/quarto generated files
#     to fix R CMD check warnings about 100+ byte paths
#
# R CMD check notes (pre-existing, not from this PR):
#   - NSE binding warnings in R/ccusage.R (needs globalVariables())
#   - MIT license needs + file LICENSE
#   - NAMESPACE not managed by roxygen2
