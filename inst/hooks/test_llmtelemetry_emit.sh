#!/usr/bin/env bash
# test_llmtelemetry_emit.sh — Integration tests for llmtelemetry_emit.sh
#
# Tests:
#   T1: Session A start, Session B start, Session A stop — A emits, B untouched
#   T2: start/stop with no CLAUDE_SESSION_ID — still pairs correctly (stable fallback)
#   T3: Mid-session stop (no sentinel) — no emit, state preserved
#   T4: Per-session sentinel consumed only by its owner session
#
# Run: bash inst/hooks/test_llmtelemetry_emit.sh
# Returns: exit 0 if all tests pass, exit 1 if any fail.

set -uo pipefail

HOOK="$(cd "$(dirname "$0")" && pwd)/llmtelemetry_emit.sh"
if [ ! -x "$HOOK" ] && [ ! -f "$HOOK" ]; then
  echo "ERROR: hook not found at $HOOK"
  exit 1
fi

# ── Test harness setup ────────────────────────────────────────────────────────
PASS=0
FAIL=0

pass() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

assert_file_exists()   { [ -f "$1" ] && pass "$2 (file exists: $1)" || fail "$2 (expected file missing: $1)"; }
assert_file_missing()  { [ ! -f "$1" ] && pass "$2 (file absent: $1)" || fail "$2 (expected absent but found: $1)"; }
assert_file_contains() { grep -q "$2" "$1" 2>/dev/null && pass "$3" || fail "$3 (pattern '$2' not in $1)"; }

# ── Temporary isolated environment ───────────────────────────────────────────
TMP=$(mktemp -d /tmp/llmtelemetry_test_XXXXXX)
trap 'rm -rf "$TMP"' EXIT

# Fake HOME so we don't pollute the real ~/.claude
export HOME="$TMP/home"
mkdir -p "$HOME/.claude/logs/llmtelemetry-staging"

# Enable opt-in flag
touch "$HOME/.claude/.llmtelemetry_emit"

LOG_DIR="$HOME/.claude/logs"
STAGING_DIR="$HOME/.claude/logs/llmtelemetry-staging"
export LLMTELEMETRY_STAGING_DIR="$STAGING_DIR"

# Unset env vars that would interfere with fallback testing
unset CLAUDE_SESSION_ID 2>/dev/null || true
unset CLAUDE_PROJECT_DIR 2>/dev/null || true

# ── Helper: run hook with a specific session ID ───────────────────────────────
run_hook() {
  local mode="$1"
  local sid="$2"  # empty string = no CLAUDE_SESSION_ID (test fallback)
  local label="${3:-}"
  if [ -n "$sid" ]; then
    CLAUDE_SESSION_ID="$sid" bash "$HOOK" "$mode" 2>/dev/null
  else
    bash "$HOOK" "$mode" 2>/dev/null
  fi
}

staging_event_count() {
  find "$STAGING_DIR" -name "*.jsonl" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print $1}' || echo 0
}

staging_contains_sid() {
  grep -rl "\"session_id\":\"$1\"" "$STAGING_DIR" 2>/dev/null | wc -l | tr -d ' '
}

echo "=== llmtelemetry_emit.sh concurrency + session-id tests ==="
echo ""

# ────────────────────────────────────────────────────────────────────────────
# T1: Session A start, Session B start, Session A stop via per-session sentinel
# ────────────────────────────────────────────────────────────────────────────
echo "T1: Concurrent sessions — A stop should emit, B state should be untouched"

SID_A="test-session-AAAA"
SID_B="test-session-BBBB"

# Both sessions start
run_hook start "$SID_A"
run_hook start "$SID_B"

# Verify start state files exist for both
assert_file_exists "$LOG_DIR/.llmtelemetry_started_at.${SID_A}" "T1: Session A start state written"
assert_file_exists "$LOG_DIR/.llmtelemetry_started_at.${SID_B}" "T1: Session B start state written"

# Write per-session sentinel for A only
touch "$HOME/.claude/.bye-requested.${SID_A}"

# Verify B's sentinel does NOT exist
assert_file_missing "$HOME/.claude/.bye-requested.${SID_B}" "T1: Session B sentinel absent before A stop"

# Run stop for session A
run_hook stop "$SID_A"

# A should have emitted a JSONL event
count=$(staging_event_count)
[ "$count" -ge 1 ] && pass "T1: Session A emitted event (count=$count)" || fail "T1: Session A should have emitted (count=$count)"

# A's sentinel should be consumed
assert_file_missing "$HOME/.claude/.bye-requested.${SID_A}" "T1: Session A sentinel consumed"

# A's state files should be cleaned up
assert_file_missing "$LOG_DIR/.llmtelemetry_started_at.${SID_A}" "T1: Session A state files cleaned up"

# B's state files must still be intact (NOT consumed by A's stop)
assert_file_exists "$LOG_DIR/.llmtelemetry_started_at.${SID_B}" "T1: Session B start state still intact"
assert_file_exists "$LOG_DIR/.llmtelemetry_session_id.${SID_B}"  "T1: Session B SID state still intact"

# The emitted event should contain A's session_id, not B's
n_a=$(staging_contains_sid "$SID_A")
n_b=$(staging_contains_sid "$SID_B")
[ "$n_a" -ge 1 ] && pass "T1: Emitted event has session_id = SID_A" || fail "T1: Emitted event missing SID_A (files: $(ls "$STAGING_DIR" 2>/dev/null))"
[ "$n_b" -eq 0 ] && pass "T1: Emitted event does NOT contain SID_B" || fail "T1: Emitted event incorrectly contains SID_B"

# Legacy global sentinel must NOT have been created
assert_file_missing "$HOME/.claude/.bye-requested" "T1: Legacy global sentinel absent"

echo ""

# ────────────────────────────────────────────────────────────────────────────
# T2: start/stop with NO CLAUDE_SESSION_ID — stable fallback pairs correctly
# ────────────────────────────────────────────────────────────────────────────
echo "T2: No CLAUDE_SESSION_ID — fallback session ID pairs start/stop correctly"

# The hook uses PPID as the stable anchor key. When the test script calls
# `bash "$HOOK" start` and `bash "$HOOK" stop`, both hook invocations are child
# processes of this test script, so their PPID = $$  (this script's PID).
# The anchor file will be: $LOG_DIR/.llmtelemetry_sid_ppid.$$

count_before=$(staging_event_count)
ANCHOR="$LOG_DIR/.llmtelemetry_sid_ppid.$$"

# Remove any leftover anchor from prior test runs
rm -f "$ANCHOR" 2>/dev/null || true

# start with no CLAUDE_SESSION_ID
bash "$HOOK" start 2>/dev/null

# Verify anchor file was written
if [ ! -f "$ANCHOR" ]; then
  fail "T2: PPID anchor file not written by start"
  GENERATED_SID=""
else
  GENERATED_SID=$(cat "$ANCHOR" 2>/dev/null || echo "")
  pass "T2: Fallback session ID generated via PPID anchor: $GENERATED_SID"
fi

if [ -n "$GENERATED_SID" ]; then
  # Write the per-session sentinel so stop will emit
  touch "$HOME/.claude/.bye-requested.${GENERATED_SID}"

  # stop with no CLAUDE_SESSION_ID — must pair with start via anchor
  bash "$HOOK" stop 2>/dev/null

  count_after=$(staging_event_count)
  new_events=$((count_after - count_before))
  [ "$new_events" -ge 1 ] && pass "T2: Fallback session emitted event" || fail "T2: Fallback session did not emit (before=$count_before after=$count_after)"

  # Verify the emitted event contains the generated SID
  n_fallback=$(staging_contains_sid "$GENERATED_SID")
  [ "$n_fallback" -ge 1 ] && pass "T2: Emitted event has the fallback session_id" || fail "T2: Emitted event missing fallback session_id ($GENERATED_SID)"

  # Anchor should be cleaned up
  assert_file_missing "$ANCHOR" "T2: PPID anchor cleaned up after stop"
else
  fail "T2: Fallback session did not emit (no generated SID)"
  fail "T2: Cannot verify emitted event session_id"
fi

echo ""

# ────────────────────────────────────────────────────────────────────────────
# T3: Mid-session stop (no sentinel) — no emit, state preserved
# ────────────────────────────────────────────────────────────────────────────
echo "T3: Mid-session stop without sentinel should NOT emit"

SID_C="test-session-CCCC"
count_before=$(staging_event_count)

run_hook start "$SID_C"
assert_file_exists "$LOG_DIR/.llmtelemetry_started_at.${SID_C}" "T3: Session C start state written"

# Stop without writing any sentinel
run_hook stop "$SID_C"

count_after=$(staging_event_count)
new_events=$((count_after - count_before))
[ "$new_events" -eq 0 ] && pass "T3: No event emitted on mid-session stop" || fail "T3: Unexpected event emitted on mid-session stop (count=$new_events)"

# State files should still be intact for the eventual real stop
assert_file_exists "$LOG_DIR/.llmtelemetry_started_at.${SID_C}" "T3: Session C start state preserved after mid-session stop"

echo ""

# ────────────────────────────────────────────────────────────────────────────
# T4: Session B stop (from T1) — should NOT consume A's already-consumed sentinel
# ────────────────────────────────────────────────────────────────────────────
echo "T4: Session B stop with its own sentinel — emits B, not A"

count_before=$(staging_event_count)

# Write per-session sentinel for B
touch "$HOME/.claude/.bye-requested.${SID_B}"

run_hook stop "$SID_B"

count_after=$(staging_event_count)
new_events=$((count_after - count_before))
[ "$new_events" -ge 1 ] && pass "T4: Session B emitted event" || fail "T4: Session B did not emit"

# B's sentinel should be consumed
assert_file_missing "$HOME/.claude/.bye-requested.${SID_B}" "T4: Session B sentinel consumed"

# The new event should contain B's session_id
n_b=$(staging_contains_sid "$SID_B")
[ "$n_b" -ge 1 ] && pass "T4: Emitted event has session_id = SID_B" || fail "T4: Emitted event missing SID_B"

echo ""

# ────────────────────────────────────────────────────────────────────────────
# Summary
# ────────────────────────────────────────────────────────────────────────────
echo "=== Results: $PASS passed, $FAIL failed ==="

if [ "$FAIL" -eq 0 ]; then
  echo "ALL TESTS PASSED"
  exit 0
else
  echo "SOME TESTS FAILED"
  exit 1
fi
