#!/usr/bin/env bash
# qa_email_output.sh — validate email HTML output
# Usage: bash inst/scripts/qa_email_output.sh /tmp/email_qa.html
# Exit 0 = pass, Exit 1 = fail
# Checks: negative (error patterns), positive (structural features),
#          ordering (blocks before summary), QA markers (day grouping, models)
set -euo pipefail

FILE="${1:?Usage: qa_email_output.sh <html_file>}"
if [ ! -f "$FILE" ]; then
  echo "SKIP: $FILE not found"
  exit 0
fi

ERRORS=0
WARNINGS=0

echo "=== Email QA: $FILE ==="

# --- Negative assertions: error patterns that should NOT appear ---
for pat in "Error in" "Error:" "invalid 'trim'" "prettyNum" "object .* not found"; do
  COUNT=$(grep -cEi "$pat" "$FILE" || true)
  if [ "$COUNT" -gt 0 ]; then
    echo "FAIL: '$pat' found ($COUNT hits)"
    ERRORS=$((ERRORS + COUNT))
  fi
done

# NaN/NULL — skip inside HTML comments (QA markers may contain "NULL" legitimately)
for pat in "NaN" ">NULL<" "NA_real_"; do
  COUNT=$(grep -v '<!--' "$FILE" | grep -ci "$pat" || true)
  if [ "$COUNT" -gt 0 ]; then
    echo "FAIL: '$pat' in visible content ($COUNT hits)"
    ERRORS=$((ERRORS + COUNT))
  fi
done

# --- Positive assertions: structural features that MUST appear ---
declare -a REQUIRED_FEATURES=(
  "Summary"
  "font-weight: bold"
  "johngavin.github.io/llmtelemetry"
)
# Features that depend on local data (cmonitor-rs, model_daily.json)
declare -a OPTIONAL_FEATURES=(
  "Time Block Activity"
  "Daily Cost by Model"
  "MTok"
  "blocks)"
)

for feat in "${REQUIRED_FEATURES[@]}"; do
  if ! grep -qi "$feat" "$FILE"; then
    echo "FAIL: missing required feature: '$feat'"
    ERRORS=$((ERRORS + 1))
  fi
done

for feat in "${OPTIONAL_FEATURES[@]}"; do
  if ! grep -qi "$feat" "$FILE"; then
    echo "WARN: missing optional feature: '$feat' (data source may be unavailable)"
    WARNINGS=$((WARNINGS + 1))
  fi
done

# At least one cost value ($N.NN format)
if ! grep -qE '\$[0-9]+\.[0-9]{2}' "$FILE"; then
  echo "FAIL: no cost values found (\$N.NN format)"
  ERRORS=$((ERRORS + 1))
fi

# --- Ordering: Time Block Activity must appear before Summary ---
BLOCKS_LINE=$(grep -n "Time Block Activity" "$FILE" | head -1 | cut -d: -f1)
SUMMARY_LINE=$(grep -n ">Summary<" "$FILE" | head -1 | cut -d: -f1)
if [ -n "$BLOCKS_LINE" ] && [ -n "$SUMMARY_LINE" ]; then
  if [ "$BLOCKS_LINE" -gt "$SUMMARY_LINE" ]; then
    echo "FAIL: Time Block Activity (line $BLOCKS_LINE) appears AFTER Summary (line $SUMMARY_LINE)"
    ERRORS=$((ERRORS + 1))
  else
    echo "OK: ordering — blocks (L$BLOCKS_LINE) before summary (L$SUMMARY_LINE)"
  fi
fi

# --- QA markers: machine-readable assertions ---
if grep -q "QA:blocks_grouped_by_day=" "$FILE"; then
  DAYS=$(grep -o 'QA:blocks_grouped_by_day=[0-9]*' "$FILE" | head -1 | cut -d= -f2)
  echo "OK: blocks grouped by $DAYS days"
  [ "$DAYS" = "0" ] && echo "WARN: 0 days grouped" && WARNINGS=$((WARNINGS + 1))
else
  echo "FAIL: no QA:blocks_grouped_by_day marker"
  ERRORS=$((ERRORS + 1))
fi

if grep -q "QA:model_breakdown_days=" "$FILE"; then
  MDAYS=$(grep -o 'QA:model_breakdown_days=[0-9]*' "$FILE" | head -1 | cut -d= -f2)
  if [ "$MDAYS" = "0" ]; then
    echo "WARN: model breakdown has 0 days (data source unavailable)"
    WARNINGS=$((WARNINGS + 1))
  else
    echo "OK: model breakdown for $MDAYS days"
  fi
else
  echo "WARN: no QA:model_breakdown_days marker (model data unavailable)"
  WARNINGS=$((WARNINGS + 1))
fi

if grep -q "QA:models_found=" "$FILE"; then
  MFOUND=$(grep -o 'QA:models_found=[^ "]*' "$FILE" | head -1 | cut -d= -f2)
  if [ "$MFOUND" = "none" ]; then
    echo "WARN: no models found (data source unavailable)"
    WARNINGS=$((WARNINGS + 1))
  else
    MODELS=$(echo "$MFOUND" | tr ',' '\n' | wc -l | tr -d ' ')
    echo "OK: $MODELS distinct models found"
  fi
else
  echo "WARN: no QA:models_found marker (model data unavailable)"
  WARNINGS=$((WARNINGS + 1))
fi

# --- Summary ---
echo "=== Results: $ERRORS errors, $WARNINGS warnings ==="
if [ "$ERRORS" -gt 0 ]; then
  echo "FAIL: $ERRORS QA issue(s) — email should be blocked"
  exit 1
fi
echo "PASS: all email QA checks passed"
exit 0
