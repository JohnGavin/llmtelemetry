# Hooks for llmtelemetry

## llmtelemetry_emit.sh

Phase 1D push hook for the centralised PIT telemetry DB (epic #83).

### Install

Symlink from `~/.claude/hooks/` so Claude Code can invoke it:

```bash
ln -sf "$(pwd)/inst/hooks/llmtelemetry_emit.sh" "$HOME/.claude/hooks/llmtelemetry_emit.sh"
```

### Usage

```bash
# From stdin (recommended)
echo '{"event_type":"session_stop","project":"llm","duration_min":42}' | llmtelemetry_emit.sh

# Output: appends one JSON line to:
#   ~/.claude/logs/llmtelemetry-staging/events-YYYY-MM-DD.jsonl
```

### Consumption

In R:

```r
events <- llmtelemetry::read_staging()
```

A later phase will integrate staged events into the rollup pipeline (deduplication, idempotency, and the events table schema TBD).
