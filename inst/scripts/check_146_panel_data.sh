#!/usr/bin/env bash
# check_146_panel_data.sh
#
# Reports whether the upstream llm-side data contracts are in place for
# the dashboard panels backing issue #146 (ROI / "is roborev worth it?").
#
# Upstream contracts checked:
#   llm#380 — roborev_agent_performance.total_cost_usd populated
#             (unblocks #146 Q1 / Q11 / Q20: cost ROI panels)
#   llm#379 — roborev_review_lifecycle.{fix_commit_sha, fix_commit_at, fix_method}
#             (unblocks #146 Q4 / Q9 / Q14: time-to-fix / false-positive / funnel)
#
# Future llmtelemetry sessions can run this in one command and avoid
# re-investigating whether the data is ready to build panels against.
#
# Usage:
#   bash inst/scripts/check_146_panel_data.sh           # full report
#   bash inst/scripts/check_146_panel_data.sh --quiet   # one-line verdict, exits 0/1
#
# Env:
#   DUCKDB_PATH   override the unified.duckdb path
#                 (default: ~/.claude/logs/unified.duckdb)
#
# Requires the `duckdb` CLI on PATH. Typical invocation from the project root:
#   nix-shell default.nix --run "bash inst/scripts/check_146_panel_data.sh"

set -uo pipefail

DUCKDB_PATH="${DUCKDB_PATH:-$HOME/.claude/logs/unified.duckdb}"
QUIET=0
[ "${1:-}" = "--quiet" ] && QUIET=1

if ! command -v duckdb >/dev/null 2>&1; then
  echo "FAIL: duckdb CLI not on PATH. Run inside the project nix-shell:"
  echo "  nix-shell default.nix --run 'bash inst/scripts/check_146_panel_data.sh'"
  exit 2
fi

if [ ! -f "$DUCKDB_PATH" ]; then
  echo "FAIL: DB not found at $DUCKDB_PATH"
  exit 2
fi

# Scalar query: returns empty string on error or no result.
q() {
  duckdb "$DUCKDB_PATH" -init /dev/null -noheader -list -c "$1" 2>/dev/null \
    | tail -n1 | tr -d '[:space:]'
}

# 1 iff column exists in table
has_col() {
  local table=$1 col=$2
  local n
  n=$(q "SELECT COUNT(*) FROM information_schema.columns
         WHERE table_name = '$table' AND column_name = '$col';")
  [ "${n:-0}" -gt 0 ] && echo 1 || echo 0
}

has_table() {
  local n
  n=$(q "SELECT COUNT(*) FROM information_schema.tables
         WHERE table_name = '$1';")
  [ "${n:-0}" -gt 0 ] && echo 1 || echo 0
}

# Track verdicts so we can roll up at the end
COST_READY=1
LIFECYCLE_READY=1
CODEX_READY=1

# ── llm#380 — roborev_agent_performance.total_cost_usd populated ──────────────
RAP_PRESENT=$(has_table roborev_agent_performance)
if [ "$RAP_PRESENT" = 1 ]; then
  COST_COL=$(has_col roborev_agent_performance total_cost_usd)
  if [ "$COST_COL" = 1 ]; then
    RAP_TOTAL=$(q "SELECT COUNT(*) FROM roborev_agent_performance;")
    RAP_WITH_COST=$(q "SELECT COUNT(*) FROM roborev_agent_performance
                       WHERE total_cost_usd IS NOT NULL AND total_cost_usd > 0;")
    [ "${RAP_WITH_COST:-0}" -gt 0 ] || COST_READY=0
  else
    COST_READY=0
    RAP_TOTAL=""; RAP_WITH_COST=""
  fi
else
  COST_READY=0
  RAP_TOTAL=""; RAP_WITH_COST=""; COST_COL=0
fi

# ── llm#379 — roborev_review_lifecycle has the 3 fix-link columns ─────────────
RRL_PRESENT=$(has_table roborev_review_lifecycle)
if [ "$RRL_PRESENT" = 1 ]; then
  FIX_SHA=$(has_col roborev_review_lifecycle fix_commit_sha)
  FIX_AT=$(has_col  roborev_review_lifecycle fix_commit_at)
  FIX_METHOD=$(has_col roborev_review_lifecycle fix_method)
  if [ "$FIX_SHA" = 1 ] && [ "$FIX_AT" = 1 ] && [ "$FIX_METHOD" = 1 ]; then
    RRL_TOTAL=$(q "SELECT COUNT(*) FROM roborev_review_lifecycle;")
    RRL_LINKED=$(q "SELECT COUNT(*) FROM roborev_review_lifecycle
                    WHERE fix_commit_sha IS NOT NULL;")
    [ "${RRL_LINKED:-0}" -gt 0 ] || LIFECYCLE_READY=0
  else
    LIFECYCLE_READY=0
    RRL_TOTAL=""; RRL_LINKED=""
  fi
else
  LIFECYCLE_READY=0
  FIX_SHA=0; FIX_AT=0; FIX_METHOD=0
  RRL_TOTAL=""; RRL_LINKED=""
fi

# ── codex_provider_invocations (cost-join target referenced in llm#380) ───────
CPI_PRESENT=$(has_table codex_provider_invocations)
[ "$CPI_PRESENT" = 1 ] || CODEX_READY=0

if [ "$COST_READY" = 1 ] && [ "$LIFECYCLE_READY" = 1 ] && [ "$CODEX_READY" = 1 ]; then
  OVERALL=READY
  RC=0
else
  OVERALL=NOT_READY
  RC=1
fi

verdict() { [ "$1" = 1 ] && echo READY || echo NOT_READY; }
yesno()   { [ "$1" = 1 ] && echo PRESENT || echo MISSING; }

if [ "$QUIET" = 1 ]; then
  echo "$OVERALL (cost=$(verdict $COST_READY) lifecycle=$(verdict $LIFECYCLE_READY) codex=$(verdict $CODEX_READY))"
  exit $RC
fi

cat <<EOF
=== #146 panel-data readiness ===
DB: $DUCKDB_PATH

[Q1 / Q11 / Q20] cost ROI panels  — upstream llm#380
  table roborev_agent_performance: $(yesno $RAP_PRESENT)
  column total_cost_usd:           $(yesno $COST_COL)
  rows with cost: ${RAP_WITH_COST:-?} / ${RAP_TOTAL:-?}
  verdict: $(verdict $COST_READY)

[Q4 / Q9 / Q14] time-to-fix / false-positive / closing-the-loop  — upstream llm#379
  table roborev_review_lifecycle:  $(yesno $RRL_PRESENT)
  column fix_commit_sha:           $(yesno $FIX_SHA)
  column fix_commit_at:            $(yesno $FIX_AT)
  column fix_method:               $(yesno $FIX_METHOD)
  rows with fix_commit_sha: ${RRL_LINKED:-?} / ${RRL_TOTAL:-?}
  verdict: $(verdict $LIFECYCLE_READY)

[cross-tool join] codex_provider_invocations  — referenced by llm#380
  table codex_provider_invocations: $(yesno $CPI_PRESENT)
  verdict: $(verdict $CODEX_READY)

=== overall: $OVERALL ===
EOF
exit $RC
