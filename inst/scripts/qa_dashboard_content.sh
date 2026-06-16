#!/usr/bin/env bash
# qa_dashboard_content.sh — validate deployed dashboard content
# Usage: bash inst/scripts/qa_dashboard_content.sh [url]
# Default URL: https://johngavin.github.io/llmtelemetry/
# Exit 0 = pass, Exit 1 = fail (critical data missing)
# Checks for "no data available" and other empty-state indicators
set -euo pipefail

URL="${1:-https://johngavin.github.io/llmtelemetry/}"
ERRORS=0
WARNINGS=0
TODAY=$(date -u +%Y-%m-%d)

echo "=== Dashboard Content QA: $URL ==="

# Fetch the dashboard HTML
TMP=$(mktemp)
trap "rm -f $TMP" EXIT

if ! curl -sL --fail "$URL" -o "$TMP" 2>/dev/null; then
  echo "FAIL: Could not fetch $URL"
  exit 1
fi

echo "Fetched $(wc -c < "$TMP" | tr -d ' ') bytes"

# --- Check rendered HTML for known error patterns (issue #69) ---
echo ""
echo "=== Checking rendered HTML for error patterns ==="
ERROR_PATTERNS=(
  "no method"
  "S3 class:"
  "could not find function"
  "object .* not found"
)
for pat in "${ERROR_PATTERNS[@]}"; do
  if grep -iqE "$pat" "$TMP"; then
    MATCH=$(grep -inE "$pat" "$TMP" | head -1)
    echo "FAIL: rendered HTML contains error pattern '$pat'"
    echo "  Match: $MATCH"
    ERRORS=$((ERRORS + 1))
  fi
done

# --- Check data JSON files directly ---
DATA_URL="${URL%/}/data"
declare -a CRITICAL_DATA=(
  # (#281 Phase 5a): legacy_ccusage_daily.json and legacy_ccusage_blocks.json removed —
  # frozen extdata snapshots, no longer written at runtime.
  "unified_sessions.json"
  "git_commits.json"
  "git_velocity.json"
  "git_file_growth.json"
)

for f in "${CRITICAL_DATA[@]}"; do
  JSON_URL="$DATA_URL/$f"
  CONTENT=$(curl -sL --fail "$JSON_URL" 2>/dev/null || echo "FETCH_ERROR")

  if [ "$CONTENT" = "FETCH_ERROR" ]; then
    echo "FAIL: Could not fetch $f"
    ERRORS=$((ERRORS + 1))
  elif [ "$CONTENT" = "[]" ] || [ "$CONTENT" = "{}" ] || [ -z "$CONTENT" ]; then
    echo "FAIL: $f is empty — dashboard will show 'No data available'"
    ERRORS=$((ERRORS + 1))
  else
    # Count array elements or check for data
    if echo "$CONTENT" | grep -q '^\['; then
      COUNT=$(echo "$CONTENT" | grep -o '{' | wc -l | tr -d ' ')
      echo "OK: $f has ~$COUNT records"
    else
      echo "OK: $f has data"
    fi
  fi
done

# --- Check optional data files (warn only) ---
declare -a OPTIONAL_DATA=(
  "gemini_daily.json"
  "gemini_sessions.json"
  "predictions.json"
  "calibration_buckets.json"
  "github_issue_events.json"
  "cost_by_project_estimated.json"
)

for f in "${OPTIONAL_DATA[@]}"; do
  JSON_URL="$DATA_URL/$f"
  CONTENT=$(curl -sL --fail "$JSON_URL" 2>/dev/null || echo "FETCH_ERROR")

  if [ "$CONTENT" = "FETCH_ERROR" ]; then
    echo "WARN: Could not fetch $f (optional)"
    WARNINGS=$((WARNINGS + 1))
  elif [ "$CONTENT" = "[]" ] || [ "$CONTENT" = "{}" ] || [ -z "$CONTENT" ]; then
    echo "WARN: $f is empty (optional data)"
    WARNINGS=$((WARNINGS + 1))
  else
    echo "OK: $f has data"
  fi
done

# --- Stale session data check ---
echo ""
echo "=== Checking session data freshness ==="
SESSIONS_URL="$DATA_URL/unified_sessions.json"
SESSIONS_CONTENT=$(curl -sL --fail "$SESSIONS_URL" 2>/dev/null || echo "[]")

if [ "$SESSIONS_CONTENT" = "[]" ]; then
  echo "WARN: No session data to check freshness"
  WARNINGS=$((WARNINGS + 1))
else
  # Extract most recent session date (first element, started_at field)
  LATEST_DATE=$(echo "$SESSIONS_CONTENT" | python3 -c "
import sys, json
d = json.load(sys.stdin)
if d and isinstance(d, list) and 'started_at' in d[0]:
    print(d[0]['started_at'][:10])
else:
    print('UNKNOWN')
" 2>/dev/null || echo "PARSE_ERROR")

  if [ "$LATEST_DATE" = "UNKNOWN" ] || [ "$LATEST_DATE" = "PARSE_ERROR" ]; then
    echo "WARN: Could not parse latest session date"
    WARNINGS=$((WARNINGS + 1))
  else
    echo "Latest session: $LATEST_DATE (today: $TODAY)"

    # Calculate staleness (days since latest session)
    if command -v python3 &> /dev/null; then
      DAYS_OLD=$(python3 -c "
from datetime import datetime, timezone
latest = datetime.strptime('$LATEST_DATE', '%Y-%m-%d').replace(tzinfo=timezone.utc)
now = datetime.now(timezone.utc)
print((now.date() - latest.date()).days)
" 2>/dev/null || echo "?")

      if [ "$DAYS_OLD" != "?" ]; then
        if [ "$DAYS_OLD" -gt 1 ]; then
          echo "WARN: Session data is $DAYS_OLD days old (threshold: 1 day)"
          WARNINGS=$((WARNINGS + 1))
        else
          echo "OK: Session data is fresh ($DAYS_OLD days old)"
        fi
      fi
    fi
  fi
fi

# --- Stale cost data check ---
echo ""
echo "=== Checking cost data freshness ==="
DAILY_URL="$DATA_URL/legacy_ccusage_daily.json"
DAILY_CONTENT=$(curl -sL --fail "$DAILY_URL" 2>/dev/null || echo "[]")

if [ "$DAILY_CONTENT" = "[]" ]; then
  echo "WARN: No cost data to check freshness"
  WARNINGS=$((WARNINGS + 1))
else
  # Extract most recent date from legacy_ccusage_daily.json
  LATEST_COST_DATE=$(echo "$DAILY_CONTENT" | python3 -c "
import sys, json
d = json.load(sys.stdin)
if d and isinstance(d, list) and 'date' in d[0]:
    print(d[0]['date'][:10])
else:
    print('UNKNOWN')
" 2>/dev/null || echo "PARSE_ERROR")

  if [ "$LATEST_COST_DATE" = "UNKNOWN" ] || [ "$LATEST_COST_DATE" = "PARSE_ERROR" ]; then
    echo "WARN: Could not parse latest cost date"
    WARNINGS=$((WARNINGS + 1))
  else
    echo "Latest cost data: $LATEST_COST_DATE (today: $TODAY)"

    if command -v python3 &> /dev/null; then
      COST_DAYS_OLD=$(python3 -c "
from datetime import datetime, timezone
latest = datetime.strptime('$LATEST_COST_DATE', '%Y-%m-%d').replace(tzinfo=timezone.utc)
now = datetime.now(timezone.utc)
print((now.date() - latest.date()).days)
" 2>/dev/null || echo "?")

      if [ "$COST_DAYS_OLD" != "?" ]; then
        if [ "$COST_DAYS_OLD" -gt 1 ]; then
          echo "WARN: Cost data is $COST_DAYS_OLD days old (threshold: 1 day)"
          WARNINGS=$((WARNINGS + 1))
        else
          echo "OK: Cost data is fresh ($COST_DAYS_OLD days old)"
        fi
      fi
    fi
  fi
fi

# --- Summary ---
echo ""
echo "=== Results: $ERRORS errors, $WARNINGS warnings ==="

if [ "$ERRORS" -gt 0 ]; then
  echo "FAIL: $ERRORS critical data file(s) empty — dashboard will have missing sections"
  exit 1
fi

if [ "$WARNINGS" -gt 0 ]; then
  echo "WARN: $WARNINGS optional data file(s) empty or stale — some sections may show 'No data'"
fi

echo "PASS: All critical data files have content"
exit 0
