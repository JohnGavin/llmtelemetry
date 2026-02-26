# Fix: Dashboard shows all zeros - data loading and CI deploy
# Issue: https://github.com/JohnGavin/llmtelemetry/issues/12
# Date: 2026-02-26
#
# Root causes:
# 1. CI workflow didn't copy vignettes/data/ to _site/ (404 on JSON files)
# 2. fromJSON("data/...") relative path doesn't work in Shinylive WebR worker
# 3. Value box icons wasted space
#
# Changes:
# - vignettes/dashboard_shinylive.qmd: Replace fromJSON() with download.file()
#   using absolute GitHub Pages URLs (irishbuoys proven pattern)
# - .github/workflows/deploy-dashboard.yaml: Add cp -r vignettes/data _site/data
# - vignettes/dashboard_shinylive.qmd: Remove showcase = icon() from value boxes
#
# Verification:
# 1. After CI deploy: curl https://johngavin.github.io/llmtelemetry/data/ccusage_daily.json
# 2. Open dashboard in browser, check F12 console for errors
# 3. Confirm Overview tab shows non-zero cost/tokens/days/sessions
