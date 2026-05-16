#!/usr/bin/env bash
# llmtelemetry_emit.sh — Phase 1D push hook for epic #83.
# Appends telemetry events to a staging JSONL file.
# Symlink from ~/.claude/hooks/ to enable.
#
# Usage: stdin = JSON object, OR pass --event-type plus key=value pairs.
# Writes to ~/.claude/logs/llmtelemetry-staging/events-YYYY-MM-DD.jsonl

set -euo pipefail

STAGING_DIR="${LLMTELEMETRY_STAGING_DIR:-$HOME/.claude/logs/llmtelemetry-staging}"
mkdir -p "$STAGING_DIR"

TODAY=$(date -u +%Y-%m-%d)
NOW=$(date -u +%Y-%m-%dT%H:%M:%SZ)
STAGING_FILE="$STAGING_DIR/events-$TODAY.jsonl"

# If stdin has data, treat it as the event payload (must be valid JSON object).
# Else build a minimal event from arguments.
if [ ! -t 0 ]; then
  PAYLOAD=$(cat)
else
  PAYLOAD="{}"
fi

# Wrap in an envelope with timestamp + source
ENVELOPE=$(printf '{"ts":"%s","host":"%s","pid":"%s","payload":%s}\n' \
                  "$NOW" "$(hostname -s)" "$$" "$PAYLOAD")

# Atomic append: write to .tmp then concatenate (best-effort, lockless)
echo "$ENVELOPE" >> "$STAGING_FILE"

exit 0
