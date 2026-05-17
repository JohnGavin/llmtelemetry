#!/usr/bin/env bash
# serve_pilot.sh — copy parquet + serve the Phase 2A pilot HTML locally
# Usage: bash scripts/serve_pilot.sh [PORT]
# Default port: 8900

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
PORT="${1:-8900}"

echo "[serve_pilot] Copying parquet to vignettes/data/v1/ ..."
mkdir -p "${REPO_ROOT}/vignettes/data/v1"
cp "${REPO_ROOT}/inst/extdata/telemetry/v1/sessions.parquet" \
   "${REPO_ROOT}/vignettes/data/v1/sessions.parquet"
echo "[serve_pilot] Copied sessions.parquet ($(du -sh "${REPO_ROOT}/vignettes/data/v1/sessions.parquet" | cut -f1))"

echo "[serve_pilot] Serving ${REPO_ROOT}/vignettes/ on http://127.0.0.1:${PORT}"
echo "[serve_pilot] Open: http://127.0.0.1:${PORT}/dashboard_v1_pilot.html"
echo "[serve_pilot] Press Ctrl-C to stop."
python3 -m http.server "${PORT}" --bind 127.0.0.1 --directory "${REPO_ROOT}/vignettes"
