# RECOVERY — llmtelemetry

Restore plan for non-reproducible state. Update this file whenever backup infrastructure changes. Test the restore steps at least once per quarter and record the drill date below.

## What needs backing up

| Asset | Path | Why irreproducible |
|---|---|---|
| Daily aggregates | `inst/extdata/ccusage_daily.json`, `model_daily.json` | Generated from local `ccusage` CLI state at runtime; historical sessions cannot be replayed |
| Session/block exports | `inst/extdata/ccusage_session_all.json`, `ccusage_blocks_all.json`, `ccusage_daily_all.json` | Aggregated from session history; loss = lost cost record |
| Daily snapshots | `inst/extdata/ccusage_daily_raw_*.json` | Per-day raw snapshots; pruning the source breaks recoverability |
| Cost history DB | `inst/extdata/llm_usage_history.duckdb` | DuckDB rollup; cannot be reconstructed from JSON alone |
| Gemini history DB | `inst/extdata/gemini_usage.duckdb` | Same as above for Gemini sessions |
| Token-monitor log | `inst/extdata/cmonitor_daily.txt` | Append-only runtime log; old lines cannot be recovered after rotation |
| Dashboard data | `vignettes/data/*.json` | Derived for the published Shinylive dashboard; re-running after a gap loses intervening sessions |

## Where backups live

| Asset class | Backup location | Failure domain | Retention |
|---|---|---|---|
| `inst/extdata/` (DBs + JSON) | `~/.llmtelemetry-data-backup/extdata/` (local) + `https://github.com/JohnGavin/llmtelemetry-data` (remote) | Separate GitHub repo (same account — see note) | Unlimited (git history) |
| `vignettes/data/` (dashboard JSON) | `~/.llmtelemetry-data-backup/vignettes_data/` (local) + same remote | Same as above | Unlimited |

**Failure domain note:** the backup repo is in the same GitHub account as the primary. This protects against local disk failure but NOT against GitHub account loss or compromise. For higher assurance, clone `JohnGavin/llmtelemetry-data` to a second machine or rclone to Backblaze B2.

**Backup script:** `scripts/backup_extdata.sh` — idempotent, exits 0 with no-op when nothing changed. Run after any significant data update or add to a daily launchd job.

## RPO / RTO

| Asset | RPO | RTO | Bottleneck |
|---|---|---|---|
| `inst/extdata/*.duckdb` | 1 day | ~5 min | Backup download speed |
| `inst/extdata/*.json` | 1 day | ~5 min | Backup download speed |
| `vignettes/data/*.json` | 1 day | ~5 min | Backup download speed |
| `inst/extdata/cmonitor_daily.txt` | 1 day | ~1 min | Trivial |

## Restore steps

```bash
# 1. Clone the data backup repo (if local copy missing):
git clone https://github.com/JohnGavin/llmtelemetry-data.git ~/.llmtelemetry-data-backup

# 2. Confirm backup is recent:
git -C ~/.llmtelemetry-data-backup log --oneline -5

# 3. Restore inst/extdata/:
cp -R ~/.llmtelemetry-data-backup/extdata/. /Users/johngavin/docs_gh/llmtelemetry/inst/extdata/

# 4. Restore vignettes/data/:
cp -R ~/.llmtelemetry-data-backup/vignettes_data/. /Users/johngavin/docs_gh/llmtelemetry/vignettes/data/

# 5. Verify (see Verification section below).
```

## Verification

| Asset | Check | Expected minimum | Last verified |
|---|---|---|---|
| `llm_usage_history.duckdb` | `duckdb llm_usage_history.duckdb -c 'SELECT COUNT(*) FROM <main_table>'` | ≥ 1 row per session day | YYYY-MM-DD |
| `inst/extdata/*.json` | `ls inst/extdata/*.json \| wc -l` | ≥ 10 files | YYYY-MM-DD |
| `vignettes/data/*.json` | `ls vignettes/data/*.json \| wc -l` | ≥ 10 files | YYYY-MM-DD |
| `cmonitor_daily.txt` | `wc -l` shows continuous days | No multi-day gaps | YYYY-MM-DD |

## Owner

| Role | Contact |
|---|---|
| Primary owner | john.b.gavin@gmail.com |
| Backup owner | — |

## Last drill

| Date | Who | Outcome | Notes |
|---|---|---|---|
| — | — | — | not yet exercised |

**Cadence:** quarterly. **Drill procedure:** rename local primaries (don't delete), run restore steps, run verification, restore primaries if needed, record outcome here, commit the updated drill log.

## Related rules

- `~/docs_gh/llm/.claude/rules/backup-architecture.md` (global) — failure-domain definition
- `~/docs_gh/llm/.claude/rules/script-destructive-ops.md` (global) — destructive scripts on these assets need recovery trail
- `~/docs_gh/llm/.claude/rules/prod-staging-context-guard.md` (global) — this project is `Environment: prod`
