#!/usr/bin/env bash
# export_and_deploy_data.sh — Export local telemetry data and push to trigger deploy
# Called from session_stop.sh hook or manually
# Exports data that only exists locally (predictions, unified.duckdb, cmonitor-rs)
# then commits to llmtelemetry repo and pushes (CI deploys automatically)

set -uo pipefail

TELEMETRY_REPO="$HOME/docs_gh/llmtelemetry"
DATA_DIR="$TELEMETRY_REPO/vignettes/data"
EXPORT_SCRIPT="$TELEMETRY_REPO/inst/scripts/export_dashboard_data.R"
ROLLUP_SCRIPT="$TELEMETRY_REPO/inst/scripts/run_rollup.R"

# Quick check: is the repo available?
if [ ! -d "$TELEMETRY_REPO/.git" ]; then
  echo "TELEMETRY: repo not found at $TELEMETRY_REPO"
  exit 0
fi

# Check if export script exists
if [ ! -f "$EXPORT_SCRIPT" ]; then
  echo "TELEMETRY: export script not found"
  exit 0
fi

# Run export (from repo root so here::here() works)
echo "TELEMETRY: exporting dashboard data..."
(cd "$TELEMETRY_REPO" && timeout 120 Rscript "$EXPORT_SCRIPT" 2>&1) | tail -5

# Regenerate v1 PIT parquets from the just-written JSONs (#210).
# run_rollup.R needs duckdb/arrow/pkgload so we enter the project nix shell.
# Guard: a rollup failure is logged but does NOT abort the export+commit — the
# parquet is regenerable; the JSON export is the primary deliverable.
if [ -f "$ROLLUP_SCRIPT" ]; then
  echo "TELEMETRY: regenerating v1 parquets via run_rollup.R..."
  rollup_out=$((cd "$TELEMETRY_REPO" && nix-shell "$TELEMETRY_REPO/default.nix" --run "timeout 180 Rscript '$ROLLUP_SCRIPT'") 2>&1) && \
    echo "TELEMETRY: rollup OK — $rollup_out" || \
    echo "TELEMETRY: rollup FAILED (non-fatal) — $rollup_out"
else
  echo "TELEMETRY: run_rollup.R not found, skipping parquet regeneration"
fi

# Check if any data files changed (JSON exports + v1 parquets)
CHANGED=$(git -C "$TELEMETRY_REPO" diff --name-only -- vignettes/data/ inst/extdata/telemetry/v1/ 2>/dev/null | wc -l | tr -d ' ')
UNTRACKED=$(git -C "$TELEMETRY_REPO" ls-files --others --exclude-standard -- vignettes/data/ inst/extdata/telemetry/v1/ 2>/dev/null | wc -l | tr -d ' ')

if [ "$CHANGED" -eq 0 ] && [ "$UNTRACKED" -eq 0 ]; then
  echo "TELEMETRY: no data changes"
  exit 0
fi

echo "TELEMETRY: $CHANGED changed, $UNTRACKED new data files"

# Commit data-only changes (JSON exports + regenerated v1 parquets)
git -C "$TELEMETRY_REPO" add vignettes/data/*.json
git -C "$TELEMETRY_REPO" add inst/extdata/telemetry/v1/*.parquet 2>/dev/null || true
git -C "$TELEMETRY_REPO" commit -m "data: update telemetry data $(date +%Y-%m-%d)

Auto-exported from local sources (predictions, unified.duckdb, cmonitor-rs).
v1 parquets regenerated from fresh JSON exports (#210).
Triggered by session-end hook." --no-verify 2>/dev/null

if [ $? -eq 0 ]; then
  # Push (triggers CI deploy)
  git -C "$TELEMETRY_REPO" push 2>/dev/null
  if [ $? -eq 0 ]; then
    echo "TELEMETRY: data pushed, CI will deploy"
  else
    echo "TELEMETRY: push failed (network?), data committed locally"
  fi
else
  echo "TELEMETRY: nothing to commit"
fi
