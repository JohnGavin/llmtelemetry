# llmtelemetry — Project Config

Global config: `~/.claude/CLAUDE.md` (loaded first, all rules apply unless overridden here).

## Project Identity

| Field | Value |
|-------|-------|
| Package name | `llmtelemetry` |
| Primary domain | LLM usage telemetry — cost tracking, session logging, automated reporting |
| Stage | Active development |
| Environment | `prod` |
| Nix shell | `/Users/johngavin/docs_gh/llmtelemetry/default.nix` |
| R version | 4.5.x (from `default.nix`) |

## Production endpoints

- Shinylive dashboard published via CI to `vignettes/dashboard_shinylive.html`
- Data export consumed downstream by `JohnGavin.github.io` portfolio index

## Project-Specific Rules

### Non-reproducible state

This project captures real-world LLM session telemetry that cannot be regenerated from inputs. See `RECOVERY.md` for the backup plan (predictions JSONL, ccusage daily/session/block exports, gemini_usage.duckdb, llm_usage_history.duckdb, cmonitor_daily.txt).

## Session Conventions

- CHANGELOG.md append at session end is mandatory (global rule)
- `.claude/CURRENT_WORK.md` is session-ephemeral (gitignored)
- Destructive ops on `inst/extdata/*.duckdb` or `inst/extdata/*.json` MUST follow `safe-deletion` + `script-destructive-ops` (see global rules)
