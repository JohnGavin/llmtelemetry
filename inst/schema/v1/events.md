# Event envelope schema (v1)

Each line in `~/.claude/logs/llmtelemetry-staging/events-YYYY-MM-DD.jsonl` is one
JSON object with this envelope:

```json
{
  "ts":      "2026-05-16T18:30:00Z",
  "host":    "hostname",
  "pid":     "12345",
  "payload": { "event_type": "...", "..." }
}
```

The `payload` is event-type-specific. The `ts`, `host`, and `pid` fields are
envelope metadata emitted by the hook at write time.

---

## session_stop  (wired in Phase 1E)

Emitted by `inst/hooks/llmtelemetry_emit.sh` when a Claude Code session ends.

```json
{
  "event_type":   "session_stop",
  "session_id":   "string (required, unique key)",
  "project":      "string — raw project path (dash-form, e.g. docs-gh-llmtelemetry)",
  "started_at":   "ISO 8601 UTC (e.g. 2026-05-16T16:00:00Z)",
  "ended_at":     "ISO 8601 UTC",
  "duration_min": "number — session wall-clock minutes",
  "agent":        "string — model identifier, e.g. claude-sonnet-4-6",
  "source":       "string — always 'claude-code-hook' for hook-emitted events",
  "working_dir":  "string — absolute working directory at session end"
}
```

Maps to the v1 `sessions` table (see `inst/schema/v1/sessions.sql`).
`canonical_project` is derived at rollup time from `project` via
`canonicalize_project()` defined in `inst/scripts/export_dashboard_data.R`.

Missing optional fields (`agent`, `working_dir`) are stored as `NA`.
A `session_stop` event with a missing or empty `session_id` is **skipped**
with a warning; it cannot be deduplicated.

---

## cost_emitted  (Phase 1F — NOT YET WIRED)

Emitted when a session cost record is available from ccusage or cmonitor-rs.

```json
{
  "event_type":  "cost_emitted",
  "session_id":  "string — links to sessions table",
  "source":      "string — e.g. 'ccusage' or 'cmonitor'",
  "cost_usd":    "number",
  "input_tok":   "integer (optional)",
  "output_tok":  "integer (optional)",
  "date":        "ISO 8601 date string"
}
```

Maps to the v1 `costs` table. Phase 1F will wire this into `rollup_costs()`.

---

## git_commit  (Phase 1F — NOT YET WIRED)

Emitted when a git commit is detected in a watched project directory.

```json
{
  "event_type": "git_commit",
  "project":    "string — raw project path",
  "hash":       "string — 40-char SHA",
  "date":       "ISO 8601 UTC",
  "author":     "string",
  "message":    "string — first line of commit message"
}
```

Maps to the v1 `git_commits` table. Phase 1F will wire this into
`rollup_git_commits()`.

---

## Deduplication

`append_sessions_from_staging()` (Phase 1E) deduplicates `session_stop` events
against the existing parquet by `session_id`. An event whose `session_id`
already exists in the parquet is silently skipped. Running the rollup twice
over the same staging files produces identical row counts (idempotent).

Future event-type handlers (Phase 1F+) must implement their own deduplication
strategy appropriate to their natural key.

---

## File naming

Staging files are named `events-YYYY-MM-DD.jsonl` and live in
`~/.claude/logs/llmtelemetry-staging/`. `read_staging()` reads all files
matching `^events-.*\.jsonl$` and combines them. There is no guarantee of
order between files or within a file; consumers must not assume temporal
ordering.
