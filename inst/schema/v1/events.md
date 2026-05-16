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

## cost_emitted  (wired in Phase 1F)

Emitted when a session cost record is available from ccusage or cmonitor-rs.

```json
{
  "event_type":     "cost_emitted",
  "project":        "string — raw project path (required)",
  "date":           "YYYY-MM-DD (required)",
  "source":         "string — e.g. 'ccusage' / 'gemini' / 'estimated' (required)",
  "daily_cost_usd": "number — total cost for the project on this date",
  "n_sessions":     "integer (optional) — number of sessions contributing",
  "duration_min":   "number (optional) — total session minutes for the day"
}
```

Maps to the v1 `costs` table (see `inst/schema/v1/costs.sql`).
`canonical_project` is derived at rollup time from `project` via
`.canonicalize_project_local()` in `R/canonicalize.R`.

Synthetic dedup key: `cost_id = paste(canonical_project, date, source, sep="|")`.
This matches the key constructed by `rollup_costs()` during the JSON backfill,
so a staged event whose cost already appears in the parquet is silently skipped.

A `cost_emitted` event missing any of `project`, `date`, or `source` is
**skipped**; if all staged events are missing required fields a warning is
emitted and 0 rows are appended.

---

## git_commit  (wired in Phase 1F)

Emitted when a git commit is detected in a watched project directory.

```json
{
  "event_type":    "git_commit",
  "project":       "string — raw project path (required)",
  "hash":          "string — full or short commit SHA (required)",
  "date":          "YYYY-MM-DD",
  "message":       "string — first line of commit message",
  "lines_added":   "integer",
  "lines_deleted": "integer",
  "files_changed": "integer",
  "lines_changed": "integer — added + deleted; computed from the above if absent"
}
```

Maps to the v1 `git_commits` table (see `inst/schema/v1/git_commits.sql`).
`canonical_project` is derived at rollup time from `project` via
`.canonicalize_project_local()` in `R/canonicalize.R`.

Composite dedup key: `commit_pk = paste(canonical_project, hash, sep="|")`.
This matches the key constructed by `rollup_git_commits()` during the JSON
backfill.  A staged event whose commit already appears in the parquet is
silently skipped.

`lines_changed` is computed as `lines_added + lines_deleted` when the field
is absent from the payload.

A `git_commit` event missing `project` or `hash` is **skipped**; if all
staged events fail validation a warning is emitted and 0 rows are appended.

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
