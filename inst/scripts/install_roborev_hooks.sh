#!/usr/bin/env bash
# install_roborev_hooks.sh — Install the roborev loop blocklist hook.
#
# Copies inst/hooks/check_loop_blocklist.sh to ~/.claude/hooks/ and prints
# the JSON fragment to add to ~/.claude/settings.json.
#
# Usage:
#   bash inst/scripts/install_roborev_hooks.sh
#
# This script does NOT modify settings.json directly — it prints the exact
# JSON fragment for the user to add manually (or via jq).

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

HOOK_SRC="$PKG_ROOT/inst/hooks/check_loop_blocklist.sh"
HOOK_DST="$HOME/.claude/hooks/check_loop_blocklist.sh"
HOOKS_DIR="$HOME/.claude/hooks"
STATE_DIR="$HOME/.claude/state"

echo "=== roborev loop blocklist hook installer ==="
echo ""

# ---- verify source exists ---------------------------------------------------
if [ ! -f "$HOOK_SRC" ]; then
  echo "ERROR: Source hook not found: $HOOK_SRC"
  echo "       Run this script from the package root."
  exit 1
fi

# ---- create directories -----------------------------------------------------
mkdir -p "$HOOKS_DIR"
mkdir -p "$STATE_DIR"
echo "[1/3] Directories created: $HOOKS_DIR, $STATE_DIR"

# ---- copy hook --------------------------------------------------------------
cp "$HOOK_SRC" "$HOOK_DST"
chmod +x "$HOOK_DST"
echo "[2/3] Hook installed: $HOOK_DST"

# ---- print settings.json fragment -------------------------------------------
echo ""
echo "[3/3] Add the following entry to ~/.claude/settings.json under"
echo "      the 'hooks' section (inside the 'PreToolUse' array):"
echo ""
cat <<'SETTINGS_FRAGMENT'
{
  "matcher": "Task",
  "hooks": [
    {
      "type": "command",
      "command": "bash ~/.claude/hooks/check_loop_blocklist.sh"
    }
  ]
}
SETTINGS_FRAGMENT

echo ""
echo "---- OR, if you prefer to add it via jq: ----------------------------"
echo ""
cat <<'JQ_CMD'
# Read current settings, add the PreToolUse hook, write back atomically:
SETTINGS="$HOME/.claude/settings.json"
TMP="$SETTINGS.$$.tmp"
jq '
  .hooks.PreToolUse //= [] |
  .hooks.PreToolUse += [{
    "matcher": "Task",
    "hooks": [{
      "type": "command",
      "command": "bash ~/.claude/hooks/check_loop_blocklist.sh"
    }]
  }]
' "$SETTINGS" > "$TMP" && mv "$TMP" "$SETTINGS"
JQ_CMD

echo ""
echo "Dependencies:"
echo "  - jq must be in PATH for the hook to run (brew install jq)"
echo ""
echo "Test the hook:"
echo "  echo '{\"tool_name\":\"Task\",\"tool_input\":{\"prompt\":\"fix R/detect_loops.R\"}}' \\"
echo "    | bash $HOOK_DST"
echo ""
echo "Done."
