#!/usr/bin/env bash
# check_loop_blocklist.sh — PreToolUse:Task hook for Claude Code.
#
# Reads a PreToolUse event JSON from stdin.  If the agent prompt or files
# mentioned match a known roborev loop in the blocklist, the hook exits 2
# (blocking the tool call) and prints a diagnostic to stderr.
#
# Install: copy to ~/.claude/hooks/ and chmod +x.
# Register in ~/.claude/settings.json under hooks.PreToolUse[].
#
# Dependencies (hot path, no R):
#   - jq   (for JSON parsing)
#   - bash (POSIX-compatible)
#
# Exit codes:
#   0  — prompt is not blocked; tool call proceeds
#   1  — internal error (jq not found, blocklist parse error)
#   2  — prompt is blocked; tool call is prevented (PreToolUse convention)
#
# Blocklist location:
#   Default: ~/.claude/state/roborev_blocklist.json
#   Override: ROBOREV_BLOCKLIST_PATH env var

set -euo pipefail

# ---- configuration -----------------------------------------------------------
BLOCKLIST_PATH="${ROBOREV_BLOCKLIST_PATH:-$HOME/.claude/state/roborev_blocklist.json}"

# ---- dependency check --------------------------------------------------------
if ! command -v jq >/dev/null 2>&1; then
  echo "check_loop_blocklist: jq not found in PATH; hook cannot run (skipping)" >&2
  exit 0
fi

# ---- read stdin --------------------------------------------------------------
# The PreToolUse hook receives a JSON object on stdin.
HOOK_INPUT=$(cat)

# Extract the agent prompt from the Task tool's 'prompt' field.
# If the tool is not Task, or prompt is missing, exit 0 (not applicable).
TOOL_NAME=$(echo "$HOOK_INPUT" | jq -r '.tool_name // empty' 2>/dev/null || true)
if [ "$TOOL_NAME" != "Task" ]; then
  exit 0
fi

PROMPT=$(echo "$HOOK_INPUT" | jq -r '.tool_input.prompt // empty' 2>/dev/null || true)
if [ -z "$PROMPT" ]; then
  exit 0
fi

# ---- extract file paths from prompt -----------------------------------------
# Regex matches common R package source paths.
# Patterns: R/*.R, inst/**/*.R, tests/**/*.R, vignettes/**/*.qmd,
#           .github/workflows/*.yml or *.yaml
FILE_REGEX='(R/[^[:space:]]+\.R|inst/[^[:space:]]+\.R|tests/[^[:space:]]+\.R|vignettes/[^[:space:]]+\.qmd|\.github/workflows/[^[:space:]]+\.ya?ml)'

MENTIONED_FILES=$(echo "$PROMPT" | grep -oE "$FILE_REGEX" | sort -u || true)

# ---- blocklist presence check -----------------------------------------------
if [ ! -f "$BLOCKLIST_PATH" ]; then
  # No blocklist file => nothing to block
  exit 0
fi

# Validate blocklist is parseable JSON (non-empty array)
BL_LEN=$(jq 'if type == "array" then length else 0 end' "$BLOCKLIST_PATH" 2>/dev/null || echo "0")
if [ "$BL_LEN" = "0" ]; then
  exit 0
fi

# ---- match 1: file path intersection ----------------------------------------
BLOCKED_ENTRY=""
BLOCKED_REASON=""
BLOCKED_HASH=""
BLOCKED_CYCLES=""
BLOCKED_TIER=""

if [ -n "$MENTIONED_FILES" ]; then
  while IFS= read -r file_path; do
    # Look for this file in blocklist primary_file fields
    MATCH=$(jq -r --arg fp "$file_path" \
      '.[] | select(.primary_file == $fp) | "\(.content_hash)\t\(.cycles)\t\(.tier)\t\(.summary)"' \
      "$BLOCKLIST_PATH" 2>/dev/null | head -1 || true)
    if [ -n "$MATCH" ]; then
      BLOCKED_HASH=$(echo "$MATCH"   | cut -f1)
      BLOCKED_CYCLES=$(echo "$MATCH" | cut -f2)
      BLOCKED_TIER=$(echo "$MATCH"   | cut -f3)
      BLOCKED_ENTRY="$file_path"
      BLOCKED_REASON=$(echo "$MATCH" | cut -f4)
      break
    fi
  done <<< "$MENTIONED_FILES"
fi

# ---- match 2: summary Jaccard (simplified bash version) ----------------------
# Full Jaccard is implemented in R; here we do a fast substring check.
# For each blocklist entry, test if 3+ distinct tokens from the summary
# appear in the prompt.  This is a conservative approximation that avoids
# false positives while catching high-overlap prompts.
if [ -z "$BLOCKED_ENTRY" ]; then
  PROMPT_LOWER=$(echo "$PROMPT" | tr '[:upper:]' '[:lower:]')

  # Read each entry's summary and check token overlap
  while IFS=$'\t' read -r hash cycles tier summary; do
    [ -z "$hash" ] && continue
    # Tokenize summary: lowercase, split on non-alnum, drop short tokens
    SUMMARY_LOWER=$(echo "$summary" | tr '[:upper:]' '[:lower:]')
    TOKENS=$(echo "$SUMMARY_LOWER" | tr -cs 'a-z0-9' '\n' | \
             awk 'length($0) >= 4' | sort -u)
    MATCH_COUNT=0
    while IFS= read -r token; do
      [ -z "$token" ] && continue
      if echo "$PROMPT_LOWER" | grep -q "$token"; then
        MATCH_COUNT=$((MATCH_COUNT + 1))
      fi
    done <<< "$TOKENS"
    TOKEN_COUNT=$(echo "$TOKENS" | grep -c . || true)

    # Require at least 3 token hits AND >= 50% of summary tokens match
    if [ "$MATCH_COUNT" -ge 3 ] && [ "$TOKEN_COUNT" -gt 0 ]; then
      # Compute ratio (integer arithmetic, scaled ×100)
      RATIO=$(( MATCH_COUNT * 100 / TOKEN_COUNT ))
      if [ "$RATIO" -ge 50 ]; then
        BLOCKED_HASH="$hash"
        BLOCKED_CYCLES="$cycles"
        BLOCKED_TIER="$tier"
        BLOCKED_ENTRY="prompt"
        BLOCKED_REASON="$summary"
        break
      fi
    fi
  done < <(jq -r '.[] | "\(.content_hash)\t\(.cycles)\t\(.tier)\t\(.summary)"' \
               "$BLOCKLIST_PATH" 2>/dev/null || true)
fi

# ---- emit result -------------------------------------------------------------
if [ -n "$BLOCKED_ENTRY" ]; then
  cat >&2 <<EOF

BLOCKED by roborev loop blocklist
  Hash   : $BLOCKED_HASH
  Tier   : $BLOCKED_TIER
  Cycles : $BLOCKED_CYCLES
  Match  : $BLOCKED_ENTRY
  Summary: $BLOCKED_REASON

This finding has recurred $BLOCKED_CYCLES times. The loop tier is '$BLOCKED_TIER'.
To acknowledge and suppress this block, run:

  Rscript inst/scripts/roborev_loop_ack.R \\
    --hash "$BLOCKED_HASH" \\
    --reason "your reason here" \\
    --until 2026-06-01   # optional expiry date

EOF
  exit 2
fi

exit 0
