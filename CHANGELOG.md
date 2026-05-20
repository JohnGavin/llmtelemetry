# Changelog

All notable changes to this project are documented here.
Format follows [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).
Versions use date stamps (package pre-1.0; semver applies from first public release).

Auto-refresh cache commits (`chore: Auto-refresh ccusage cache …`) are omitted
— they are routine CI noise. Check `git log --oneline --grep "Auto-refresh"` for the
full refresh history.

---

## [Unreleased]

> **Session 2026-05-20 evening (codex-epic followup + process work):** Closed all 3 decision-needed priorities from prior session. **U (codex-epic fixes, `8ebb198`):** 6 post-merge roborev findings fixed in one cluster — `refresh_codex_cache.R` fallback canonicalizer walks all `docs_gh/`-suffix segments past container prefixes (`data`, `finance`, `stats`, `pers`, `proj`, `.claude`); `digest` added to DESCRIPTION Imports; roborev source attribution keyed on join-success not `repo_id` non-NA; email "Sessions" metric uses `n_distinct(thread_id)` from `codex_s`; dashboard Sessions KPI includes codex; new `codex_d_proj` reactive applies project filter to codex weekly charts. 4 new regression tests; 762 total passing. **V (#83 epic audit):** all 7 epic branches classified OBSOLETE — squash-merged via PRs #84–#90 plus 2 direct commits between 2026-05-16 and 2026-05-18; deleted. Phase 4 (backfill, due 2026-05-25) starts fresh from current main. **#129 closed:** `branch-salvage-workflow` rule + `branch-cherry-check.sh` helper landed in `JohnGavin/llm@cb7d907`. 3-step pre-salvage check (patch-id + closing-PR + unique-strings) catches squash-merge and re-implementation cases that `git cherry` alone misses. **llm#191 Tier 3 shipped:** `JohnGavin/llm@bd46d50` extends `auto-delegation` with post-agent verification pattern; `agent-post-verify.sh` provides capture/check semantics with drift classification + recovery instructions; logs to `~/.claude/logs/worktree_post_verify.log` for empirical baseline. Tier 1 prompt-prefix has now held across 4 consecutive agent dispatches (R, T, U, V) — self-check lines confirm worktree boundary respected. **Worktree cleanup:** llmtelemetry branch list is now `* main` only.
>
> **Failed approaches this session:** none — all tracks delivered cleanly. The new worktree-isolation prefix (just shipped in `d7f58d9`) was validated immediately by U and V — both agents' self-check confirmed `pwd` under `.claude/worktrees/agent-*` and branch != `main`.
>
> **Accuracy / Metrics:**
> - Tests: 717 → 762 (+45 new in U: canonicalize nested paths, roborev attribution with NA repo_id, email session-count with codex_s fixture, dashboard codex_d_proj project filter)
> - Open issues llmtelemetry: 3 → 1 (#83 epic remains, decoupled from stale branches)
> - Open issues llm: 2 unchanged (#182 inverse case, #191 Tier 2/4 remain)
> - Open roborev reviews: 50 → 93 (each push triggers fresh review; new findings flagged against the U-merged state — see Known Limitations)
> - Local branches llmtelemetry: 7 stale + main → main only
>
> **Known Limitations (4 new roborev findings against U-merged state):**
> - HIGH: `inst/scripts/refresh_codex_cache.R:603-645` — incremental refresh overwrites existing aggregates instead of merging additively. Re-running for same thread/day discards earlier turns. Fix: merge overlapping keys additively or rebuild affected thread/day from a complete source window.
> - HIGH: `inst/scripts/export_dashboard_data.R:146-167` + `R/rollup_sessions.R:195-268` — public export still writes OLD hash format (6-digit decimal) while staged rollups now use ISO-8601 + 12-char hex per Agent C's #114 fix. `append_sessions_from_staging()` deduplication is broken across these two formats.
> - MEDIUM: `R/canonicalize.R:44-67` + `refresh_codex_cache.R:82-95` — nested underscore-form paths still miscanonicalized. Example: `.../proj/data/weather/irish_buoy_network/irishbuoys` → `weather` instead of `irish_buoy_network`. Beyond U's container-prefix walk.
> - LOW: `tests/testthat/test-export-dashboard-data.R:383-389` — CI fallback regression test skips unconditionally because `fallback_map` no longer exists in export script.

> **Session 2026-05-19/20 (roborev triage + codex coverage epic):** Shipped a long multi-track session across 2 days. **Roborev cleanup:** 50+21+3 reviews closed against the latest 6-merge fix series; 13 duplicate GitHub issues closed (#113, #114, #115, #117, #116, #118, #119–#128) with merge-commit references. **Privacy regression (#131):** Agent A sanitized `_all` JSON snapshots; the 12-hourly launchd cron at `~/Library/LaunchAgents/com.johngavin.llmtelemetry.refresh.plist → /Users/johngavin/docs_gh/proj/data/llm/telemetry/exec/refresh_and_preserve.sh` re-introduced 190+29 leaks 4 hours later. Fix landed in 2 commits: `c38f78d` added `inst/scripts/sanitize_ccusage_all.R` + 22 regression tests + restored snapshots; `6b6f814` (in the external repo, same `JohnGavin/llmtelemetry.git` remote) added a fail-closed Tier 3c sanitization step to the cron script. Verified clean against the 02:43 natural cron cycle (`1ddcbe4`); #131 closed. **Codex coverage (#72/#130):** Investigation found `~/.codex/log/codex-tui.log` is OTEL-structured with full per-turn token usage; thread_id joins with roborev DB for repo attribution. R (`feat/codex-telemetry-ingestion`, 43 tests) added `inst/scripts/refresh_codex_cache.R` + `inst/extdata/codex_pricing.json` + `codex_daily.json`/`codex_sessions.json` + hook into `export_dashboard_data.R`. T extended `send_daily_email.R` with codex row + "Codex by Automation Type" (roborev/interactive split). S extended `dashboard_shinylive.qmd` with `SOURCE_PALETTE` (claude `#d97757` / gemini `#4285f4` / codex `#10a37f`) across 6 chart sites; closed #72 and #130. **Stale branch cleanup:** 14 unmerged `fix/*` branches triaged — 4 superseded discarded, 2 already-applied (git cherry) deleted, 2 OT-investigation archived with comments on closed #58/#59, 1 ARIA no-op (already squash-merged via #64) discarded, 1 byte-order sort salvaged into commit fixing both #116 + #117. Remaining 7 branches are all `#83` epic phases (kept). **Process findings filed in llm repo:** #182 (sandbox over-restricts authorized external writes); #191 (sandbox under-restricts — agents D/F/S silently committed to main checkout); Tier 1 of #191 already shipped as `d7f58d9` (worktree-isolation prefix in `auto-delegation` rule). **#129 filed** with git-cherry methodology refinement (squash-merge limitation discovered when 4 of 6 "salvage candidates" turned out to be no-ops — patch-id matching can't catch squash-merge).
>
> **Failed approaches this session:**
> - Agent G blocked by sandbox on Bash for protected paths (`inst/extdata/`) — orchestrator took over read-only audit; turned out parquets were already clean (test-no-path-leak.R already covered them at lines 51-68).
> - Agent F's "ARIA rebase" produced an empty diff — content was already in main via squash-merge `469d709` (#64); `git cherry` patch-id check did NOT detect this — squash-merge breaks patch-id matching.
> - Agent P sandbox-blocked from writing to authorized external path `/Users/johngavin/docs_gh/proj/data/llm/telemetry/exec/refresh_and_preserve.sh` even with explicit prompt-level authorization — orchestrator took over.
> - Agent S committed directly to `main` branch in its worktree instead of the requested feature branch (filed as #191 in llm repo).
>
> **Accuracy / Metrics:**
> - Tests: 717 → 717 + 43 (codex ingestion) + 23 (codex email) + 9 (dashboard codex) + 22 (sanitizer regression) = 814 passing (1 pre-existing skip)
> - Open issues: 17 → 3 (#129 methodology, #83 epic, #133 closed stub for cross-link)
> - Open roborev reviews: 51 → 50 (each push triggers fresh review; current 50 reflect findings against the JUST-MERGED codex epic — see Known Limitations)
>
> **Known Limitations (open roborev findings against the codex epic):**
> - HIGH: `refresh_codex_cache.R:27-47` — standalone fallback canonicalizer returns container segments (`data`) for nested `proj/data/...` paths; reuse package canonicalizer or strip known container prefixes
> - MEDIUM: `sanitize_ccusage_all.R:109-110` — `digest::digest()` called unconditionally but `digest` not in DESCRIPTION; will fail in clean CI/Nix
> - MEDIUM: `refresh_codex_cache.R:459-462` — roborev attribution keyed on `repo_id` non-NA rather than join-success; matched rows with missing repo_id silently classified as "interactive"
> - MEDIUM: `send_daily_email.R:661-668` — "Codex by Automation Type" "Sessions" column uses `n()` over daily-aggregated `codex_d` rather than distinct `thread_id` in `codex_s` (over-counts)
> - MEDIUM: `dashboard_shinylive.qmd:1281-1289` — Sessions KPI counts only Claude+Gemini, misses codex
> - MEDIUM: `dashboard_shinylive.qmd:1171-1173,1602-1607` — Codex weekly chart uses `codex_d()` without project filter while Claude uses `cc_d_proj()` — filtered Claude compared against all-project Codex

> **Session 2026-05-18 (roborev triage + #112 hotfix):** Job 3002 found a High bug introduced in Phase 2B: DuckDB IN clause built with `JSON.stringify()` producing double-quoted identifiers instead of single-quoted SQL string literals — project filter was silently broken for any non-null selection. Fixed in 56c8018 (#112 closed). Filed #113 (resolve_sel/__ALL__ drops projects missing from projects_master), #114 (session_id hash collision risk, 1M space), #115 (ccusage_*_all.json exposes raw filesystem paths in public extdata). Jobs 3009/3015/3021/3027 triaged and closed against #113/#114/#115. Roborev resolution rate: 163/168 addressed.

> **Session 2026-05-18 (7-issue bug-fix blitz):** Closed #105, #107, #108, #111, #106, #110, #109 in parallel across two batches. #105: CI workflow removed guaranteed-404 validation of `_site/dashboard_shinylive.html` (dashboard is published as index.html only). #107: `output$cost_by_model` now uses `cc_d()` directly — `cc_d_proj()` routed through `tok_proj` which lacks `modelsUsed`/`totalCost`. #108: `aggregate()` step added after `canonicalize_session_project()` in cost and token exports — collapses duplicate (canonical_project, date) rows created when multiple raw aliases map to the same canonical name. #111: `unified_sessions` read split into two steps to emit `message()` count before the silent `pmax()` clamp of `ended_at < started_at`. #106: `"irishbuoys" = "irish_buoy_network"` added to `canonicalize_project()` overrides (dash-form was missing; only slash-form `"buoy/network"` was handled); mycare restored to `tracked_repos`. #110: session dedup now uses row-number tiebreaker — last-appended row wins when `started_at` ties, replacing the unreliable `distinct()` sort order. #109: `poll_github_events.R` parse-error handler sets `parse_failed <<- TRUE`; next line returns `load_previous()` immediately, discarding partial in-memory pages. All 7 commits pushed to main.

> **Session 2026-05-18 (Phase 3 hook deployment):** `~/.claude/hooks/llmtelemetry_emit.sh` deployed — two modes (start/stop), JSONL envelope to `~/.claude/logs/llmtelemetry-staging/events-HOST-DATE.jsonl`, duration computed via python3 (portable). Three projects opted in via flag files (llm, llmtelemetry, irishbuoys). `settings.json` wired to SessionStart + Stop events. End-to-end test: 224→231 rows in parquet from 7 staged events. 1-week observation window begins 2026-05-25 before Phase 4 backfill.

> **Session 2026-05-18 (Phase 2B + issue group blitz):** Phase 2B complete: Sessions by Project chart now fully reactive — DuckDB-WASM SQL carries `started_at >= cutoff` alongside project IN clause; `observe()` now reads `cutoff()` so date horizon changes re-query; chart colour corrected from Gemini red to Claude blue. v1 parquets refreshed: sessions 202→224, git_commits 860→1553. Phase 2 of Epic #83 is done.

> **Session 2026-05-18 (issue group blitz):** Closed 7 issues. #81: corrected `tracked_repos` paths in `export_dashboard_data.R` (irishbuoys/footbet pointed at non-existent dirs). #78: verified Gemini telemetry is current; CLI last used Feb 2026, closed as expected behaviour. #80: `output$cost_cumulative` now sources from `cost_by_project_estimated.json` for project-aware cumulative cost; new `proj_cost_cumulative` chart added to By Project tab. #74: Git tab restructured with `navset_tab()` into "Velocity" and "Code Health" sub-tabs. #75: `card_footer()` added to Overview Summary, Sessions Summary, Git Summary cards. #70: Glossary hyperlinks added (Cache, Bus Factor, Churn, TODO/FIXME, Brier Score, Calibration, API Conversation, Block); new Tools section. #79: `tokens_by_project.json` export added (72 rows, duration-weight approach matching cost estimates); `cc_d_proj()` reactive now project-aware; Token Analysis charts respect project filter.

> **Session 2026-05-17 evening (roborev fixes):** Fixed 2 roborev findings. #2181: moved "Check rendered HTML" step in `deploy-dashboard.yaml` to BEFORE `upload-pages-artifact` so a broken dashboard cannot be uploaded before the check fires. #2191: added `@k{n}` counter suffix to sanitized session_ids in `rollup_sessions.R` for collision resistance; fixed `distinct()` sort order to keep the most-recent row (sort DESC before distinct, not after).

> **Session 2026-05-17 (PR merge + cleanup):** Merged all 12 roborev-backlog PRs (#93–#104) into main via sequential local integration merge with 4 manual conflict resolutions. Cleaned up 3 agent worktrees (199MB), deleted 12 remote branches. Key conflict resolutions: (1) `poll_github_events.R` — combined #94 exit-status fallback with #100 per-repo `load_previous()` filter; (2) extdata JSONs — took PR #101 privacy-sanitized versions; (3) dashboard ECharts graph — kept `name=f` (full path) from #103 to match edge keys. CI triggered (run 25996149508). Epic #83 Phase 2B (DuckDB-WASM chart integration) is next.

> **Session 2026-05-16 evening (telemetry epic Phase 1 complete):** Phase 1A-F + Phase 2A landed across 6 merged PRs (#85, #86, #87, #88, #90, #91). Push pipeline closed for all 3 v1 tables (sessions, costs, git_commits). DuckDB-WASM pilot proved end-to-end browser query against real parquet in 539 ms cold (Q3 from Phase 0 verified). Privacy fix PR #91 closes roborev #936. Roborev #905 closed (verified PR #62 fix). Issue #58 closed with synthesis. Filed #92 (ccusage_daily.json contains stderr text instead of JSON — likely root cause of roborev #948's reported symptom). Tests: 28 → 543 (+515). DESCRIPTION: 0.1.0 → 0.5.1.

### Privacy fix — closes roborev #936 (PR #N)
- `sanitize_for_public()` helper added to `inst/scripts/export_dashboard_data.R`
- Replaces raw `project` column with `canonical_project` before every write to `inst/extdata/`
- Drops rows where `canonical_project IS NA` (orphan/agent-worktree rows that have no canonical project)
- Sanitizes path-style `session_id` values (e.g. `-Users-johngavin-docs-gh-llm`) with a deterministic synthetic id built from `canonical_project@started_at`
- Drops `project_slug` from `predictions.json` (raw filesystem path column)
- All 7 affected committed JSONs regenerated clean: unified_sessions (136 orphans dropped), cost_by_project_estimated (37 dropped), cost_per_commit (0 dropped), weekly_commits_by_project (0 dropped), git_commits_by_project (0 dropped), file_churn (0 dropped), change_coupling (0 dropped)
- v1 parquets regenerated clean (sessions 329→202 rows, costs 108→68 rows, git_commits 860 rows unchanged)
- New `tests/testthat/test-no-path-leak.R` — 145 assertions blocking regressions across all JSON and Parquet extdata files
- Version bumped to 0.5.1

### Phase 2A of epic #83 (DRAFT PR)
- `vignettes/dashboard_v1_pilot.qmd`: pilot demonstrating DuckDB-WASM querying real v1 sessions parquet (329 rows) rendered to ECharts horizontal bar chart — pure JS, no R reactivity
- `.gitignore`: `vignettes/data/` already excluded (served copies are not source-of-truth)
- `scripts/serve_pilot.sh`: convenience runner — copies parquet, starts python3 http.server on port 8900
- Phase 2B will integrate this pattern into the main dashboard with project-filter reactivity

### Phase 1F of epic #83 (PR #N)
- Refactor: `.canonicalize_project_local()` moved to `R/canonicalize.R` as shared internal helper (used by all 3 appenders)
- New function: `append_costs_from_staging()` — consumes `cost_emitted` events into v1 costs parquet; dedup by `cost_id = paste(canonical_project, date, source, sep="|")`
- New function: `append_git_commits_from_staging()` — consumes `git_commit` events into v1 git_commits parquet; dedup by `commit_pk = paste(canonical_project, hash, sep="|")`
- `inst/schema/v1/events.md` updated with full payload specs for `cost_emitted` and `git_commit`
- `inst/scripts/run_rollup.R` now drains staging for all 3 tables (sessions, costs, git_commits)
- 30 tests in `tests/testthat/test-append-costs-from-staging.R`; 35 tests in `tests/testthat/test-append-git-commits-from-staging.R`
- Version bumped to 0.5.0

### Phase 1E of epic #83 (PR #N)
- New function: `append_sessions_from_staging()` — consumes hook events into v1 sessions parquet
- Dedup by `session_id`; idempotent (running twice yields identical row count)
- Event envelope schema documented at `inst/schema/v1/events.md`
- `inst/scripts/run_rollup.R` now also drains the staging dir after backfills
- 31 tests in `tests/testthat/test-append-from-staging.R`
- Version bumped to 0.4.0

### Phase 1B/C/D of epic #83 (PR #86)

- New tables: costs (Phase 1B), git_commits (Phase 1C)
- New rollups: `rollup_costs()` from `cost_by_project_estimated.json` (72 rows)
- New rollups: `rollup_git_commits()` from `git_commits_by_project.json` (837 rows)
- Push hook: `inst/hooks/llmtelemetry_emit.sh` appends events to staging JSONL
- Staging reader: `R/read_staging.R` — `read_staging()` returns tibble of staged events
- `inst/scripts/run_rollup.R` now executes all 3 rollups
- Version bumped to 0.3.0

### Phase 1A of epic #83

- New schema `inst/schema/v1/sessions.sql` — PIT sessions table with `valid_from NOT NULL`, primary key, two indexes.
- `R/schema.R` — `schema_version()`, `apply_schema_v1()`, `open_telemetry_db()` exported helpers.
- `R/rollup_sessions.R` — backfills sessions from `unified_sessions.json` into v1 Parquet via duckplyr + DuckDB COPY TO (no arrow dependency).
- `inst/scripts/run_rollup.R` — CLI runner; produces 329-row initial parquet.
- `inst/extdata/telemetry/v1/sessions.parquet` — first PIT data committed (329 rows, zstd compression).
- Tests: 5 schema tests + 8 rollup tests (column set, valid_from, round-trip, idempotency, error on empty input).
- Version bumped to 0.2.0; `withr` added to Suggests.

---

## 2026-05-13 to 2026-05-16 (session: dashboard-refresh-and-telemetry-epic)

### Completed
- **Issue #26 / PR #56** — unified.duckdb migration via `DBI::dbReadTable()`; 20 new tests.
- **Issue #27 dashboard refresh** (CLOSED) — Phase 2 (#66) added 4 new exports (`weekly_commits_by_project`, `cost_per_commit`, `file_churn`, `change_coupling`); Phase 3 (#67) added granularity toggle, 90-day default `dataZoom`, replaced 10 value_boxes with 3 compact summary tables, added 4 new chart cards including coupling force-graph; hotfix in same PR for E1/E2 Git tab card heights, D1 block_table sort, B3 ec_line y-axis rescale.
- **Issue #68 project filter epic** (CLOSED) — Phase A (#76) added `canonicalize_project()` helper, `canonical_project` column on 7 exports, new `projects_master.json` stable list; Phase B (#77) wired multi-select `selectInput(multiple=TRUE, selectize=FALSE, size=10)` populated ONCE from master list, 7 outputs reactive to filter, fixes time-horizon stickiness.
- **PR #71** — QA grep error patterns (`no method`, `S3 class:`, `could not find function`, `object .* not found`) in both `inst/scripts/qa_dashboard_content.sh` and `.github/workflows/deploy-dashboard.yaml` verify-deployment step.
- **Epic #83 Phase 0** (PR #84 merged) — added `shinylive` R package to nix shell (default.R + regenerated default.nix); Q3 gate prototype at `prototypes/duckdb-wasm/` validates DuckDB-WASM + WebR coexistence in same browser tab. Phase 0 verdict: PASS. Architecture validated for the push-to-PIT-parquet telemetry DB design.

### Bug fixes within PR #77
- Server-crash `observe(..., once=TRUE)`: invalid arg, replaced with `local({...})`.
- `commits_by_proj` "arguments imply differing number of rows: 0, 1" — defensive: use `names(splits)[i]` for Project, coerce numeric columns, guard empty groups.
- `base_url` hardcoded to gh-pages: local renders were fetching production data. Now `""` (same-origin) — works for local + prod identically.
- Bus Factor / File Growth caption renames (6 Months → 1 Month, 6-Month → 1-Week) — label-only; server-side window unchanged.

### Issues filed (open at session end)
- #72 multi-source coverage (Claude + Gemini + codex)
- #73 Cost per Commit Trend "no data available"
- #74 Git tab layout — tabsets or split
- #75 caption on every chart per visualisation standard
- #78 Gemini data is stale
- #79 per-project token aggregation
- #80 cumulative cost (B+C epic)
- #81 Git charts only show llm/llmtelemetry — backfill needed
- #82 Calibration tab not reactive to time filters
- #83 **EPIC** centralised push-based telemetry DB — Phase 0 complete, Phase 1 pending

Of these, #72, #78, #79, #80 are downstream of #83's push architecture — likely dissolved when Phase 1 ships.

### Lessons learned (preserved in `.claude/CURRENT_WORK.md`)
- `quarto render` exit 0 ≠ render success: shinylive Lua filter errors are swallowed; always grep rendered HTML for expected content.
- WebR rlang 1.1.6 pin means dplyr/duckplyr cannot run client-side; dashboard query layer must use pre-built VIEWS executed via DuckDB-WASM minimal SQL (rule-compliant exemption).
- HF `hf://` row-group queries fail browser CORS preflight (huggingface/datasets#7931) — gh-pages must be the dashboard's live query target, HF stays as long-term archive.
- shinylive's `<pre class="shinylive-r">` "warning" text in rendered HTML is normal; the JS runtime replaces it with the widget at page load (10–30s WebR cold start).
- `default.R` `project_path` should be `"."` not a hardcoded sibling path — otherwise regens land in the wrong checkout.

### Build / infrastructure changes
- `default.R` adds `shinylive` to `r_pkgs`; `project_path = "."` (was hardcoded sibling).
- `default.nix` regenerated to include shinylive 0.3.0 + transitive deps.
- Prototypes at `prototypes/duckdb-wasm/` preserved on main as reference for Phase 1.

---

## 2026-05-12 (session: roborev-triage-and-maintenance)

### Completed
- **Backup infrastructure** (PR #61): created private `JohnGavin/llmtelemetry-data` repo as
  second-copy backup target; `scripts/backup_extdata.sh` syncs `inst/extdata/` and
  `vignettes/data/` there idempotently; filled all `<TODO>` placeholders in `RECOVERY.md`
  with actual paths, failure-domain note, and restore steps.
- **Version bump** (PR #61): `DESCRIPTION` `0.0.0.9000` → `0.1.0` (project is live/prod).
- **Roborev triage**: sampled all 40 failing verdicts; closed 2 false positives (jobs 1056,
  1052 — reviewer didn't see prior-commit otelsdk work); categorised remaining 38.
- **QA script bugs** (PR #62): `curl -sL` without `--fail` allowed HTTP 404 to pass silently
  as "OK" (HIGH, job #905); `$TODAY` used before initialization under `set -u` caused unbound-
  variable abort when sessions data was empty (MEDIUM, job #996).
- **Project filter + email NaN** (PR #63): "None" button showed all projects instead of none —
  `length > 0` guard on 9 filter sites bypassed empty selection (jobs #982, #989); replaced
  `is.na(cost_per_mtok) <- 0` with `is.nan()` so genuine missing rows render "-" not "$0.00"
  (job #897).
- **ARIA + fullscreen sweep** (PR #64): `ec()` `role="img"` → `role="button"` + `tabindex="0"`
  + keyboard handler (Enter/Space); `ec_scatter()` and bespoke `block_timeline` chart were
  entirely missing ARIA and fullscreen — both now have same pattern (jobs #946, #940).
- **Timestamp normalization** (PR #65): `ended_at < started_at` in exported sessions (ccusage
  UTC/local timezone mismatch); `pmax(ended_at, started_at)` before `as.character()` conversion
  keeps fields monotonic (job #1055).

### Failed Approaches
- `rsync` via `command -v rsync` in backup script: `openrsync` wrapper resolves but can't exec
  the underlying `rsync` binary in the Nix shell context — exit 14. Switched to `cp -R` with
  pre-deletion of destination dirs.
- `/usr/bin/rsync` directly: same openrsync wrapper issue. `cp -R` with clear-then-copy is the
  reliable macOS/Nix workaround.

### Accuracy / Metrics
- Roborev: 51 total, 8 passed, 43 failed, **2 addressed** (up from 0) this session.
  5 PRs in review address jobs #905, #996, #982, #989, #897, #946, #940, #1055 (8 more verdicts).
  Projected pass rate after merges: ~16/51 ≈ 31% (up from 13%).

### Known Limitations
- 5 PRs (#61–#65) open but not yet merged; session work lands on merge.
- Remaining ~9 open roborev verdicts: JS formatter serialized as JSON string in `ec()` tooltip
  (job #982), missing regression tests (#905, #897), `investigate_58.R` overstatement (#1033),
  and investigation-branch items.
- Backup repo in same GitHub account — not protected against account-level loss (noted in
  `RECOVERY.md`; higher-assurance option is rclone to Backblaze B2).
- `scripts/backup_extdata.sh` not yet on a daily schedule (manual run only).

---

## 2026-05-12 (session: fix-issue-59-nix-otelsdk)

### Completed
- Built `otelsdk` (OpenTelemetry C++ SDK wrapper, GitHub-only) as a manual Nix
  derivation in `default.nix` (issue #59, PR #60). `library(otelsdk)` now loads
  cleanly from `nix-shell default.nix`.
- Added `default.post.sh`: idempotent script that re-applies the `otelsdk` derivation
  block after any future `rix::rix()` regeneration of `default.nix`.
- Created `CHANGELOG.md` (this file) with full history back to 2026-02-21.
- Created `NIX.md`: Nix environment reference — adding packages, updating the `otelsdk`
  rev, reverting `default.nix`, known issues and workarounds.

### Failed Approaches
- `postPatch` on `src/CMakeLists.txt` — file does not exist in the package root;
  it lives inside `src/vendor/opentelemetry-cpp.tgz` (extracted at build time, not
  at patch time). The patch hook runs before the tarball is unpacked.
- `postPatch` on `CMakeLists.txt` (no path prefix) — same result, wrong location.
- Setting `CMAKE_PREFIX_PATH` with semicolons in `preBuild` — semicolons are bash
  statement separators inside Makefile recipes; the remaining path segments after the
  first semicolon were executed as shell commands, producing "Is a directory" errors.
- `postPatch` with `sed` to comment out `include(cmake/googletest.cmake)` in
  `src/CMakeLists.txt` — same "no such file" problem as above.

### Root cause
`-DBUILD_TESTING=OFF` is already present in `src/Makevars.in`'s cmake invocation,
and `CMakeLists.txt` does guard the googletest include inside `if(BUILD_TESTING)`.
However cmake 4.1.2 evaluates `FetchContent_MakeAvailable` at configure time before
the `BUILD_TESTING` guard fully suppresses it. Adding
`-DFETCHCONTENT_FULLY_DISCONNECTED=ON` to the cmake invocation (via a `sed` patch on
`src/Makevars.in`) prevents all FetchContent network downloads unconditionally.

### Accuracy / Metrics
- `library(otelsdk)` loads from `nix-shell default.nix --run "Rscript -e '...'"`: ✓
- Build time: ~45 seconds (cmake + C++ compile)

### Known Limitations
- `RECOVERY.md` backup destinations are still TODO (backup infra not yet configured).
- 37 pre-existing roborev verdicts unaddressed (pre-date this session).
- `DESCRIPTION` still at `0.0.0.9000` — needs bump before first public deploy.

---

## 2026-05-12

### Added
- `otelsdk` R package (GitHub-only, wraps OpenTelemetry C++ SDK 1.22.0) built as a
  manual Nix derivation in `default.nix` (issue #59, PR #60)
  - `nativeBuildInputs`: `which`, `cmake`, `protobuf_21`, `pkg-config`, `curl`, `curl.dev`,
    `zlib`, `zlib.dev`
  - `preBuild`: exports `$CMAKE` so configure skips `which cmake` (not in Nix sandbox PATH)
  - `postPatch`: patches `src/Makevars.in` to add `-DFETCHCONTENT_FULLY_DISCONNECTED=ON`
    (prevents googletest git-clone, which is blocked in the Nix derivation sandbox)
  - `default.post.sh`: idempotent script that re-applies the otelsdk block after any
    future `Rscript default.R` / `rix::rix()` regeneration (see `NIX.md`)
- `.gitignore` entries: `.Renviron`, common IDE and OS files
- OTEL env-var stubs in `.Renviron.example` for issue #59 context

### Fixed
- QA staleness threshold tightened to 1 day; additional data-presence checks added (#55)

---

## 2026-05-11

### Added
- Multi-project Commits tab with project filter (PR #54)
- Commits tab clarification: single-repo scope note added (PR #54 follow-up)
- Comprehensive Glossary definitions (PR #53)

### Fixed
- ARIA labels added to Repo Health charts (PR #52)
- Session duration display, negative durations, and axis label fixes
- Calibration export NULL handling; QA script added

---

## 2026-05-10

### Added
- Click-to-fullscreen for all ECharts panels
- Zoom controls; remaining barplots converted to dotcharts (Cleveland dot plots)
- File growth visualization on dashboard (#45)
- Calibration by project chart (#46 Phase 3)
- Session-weighted cost estimation by project (Phase 2 of #46)
- QA tests for empty data and stale sessions
- GitHub issue events polling (Layers 3–4 ingestion)
- Project filter buttons for Sessions tab (#46)
- Glossary page and ARIA labels for accessibility
- Tooltips on Overview value boxes (#33)
- Overview value boxes clickable to drill-down (#32)

### Fixed
- `persist local-only data to inst/extdata` for CI deployment
- Dashboard sessions display, model names, chart zoom

---

## 2026-05-09

### Added
- "By Project" hybrid drilldown tab (Option D)
- `pulse` scripts migrated to llmtelemetry (Option C)
- `feat/scope-change-ingestion` merged: multi-project scope change tracking

### Fixed
- `fb9aadf` Guard NaN values in daily email `cost_per_mtok` formatting

---

## 2026-03-10

### Added
- Lines-per-commit distribution chart on dashboard (#18, PR #20)
- Data API with documented schema and source references (#19, PR #21)

### Fixed
- `bf162bf` Quote git log format string to prevent shell pipe interpretation

---

## 2026-03-08

### Fixed
- Dashboard UX: captions, zoom, legends, value boxes, footer (PR #17)

---

## 2026-03-05

### Fixed
- Dashboard all-zeros bug: data loading and CI deploy fixed (#13, PR #13)
- Dashboard NA crash, pie chart replaced with dotchart, deploy health check (PR #15)

---

## 2026-02-26

### Added
- Shinylive interactive telemetry dashboard (#9, PR #10)
- Adversarial QA tests for all exported functions (Step 4.4/4.5)
- `shinylive` R package added to CI dependencies

### Fixed
- R CMD check clean pass + CI deploy (#11)
- Backfill historical data from llm repo (#7, PR #8)

---

## 2026-02-25

### Added
- Gemini usage data in daily email (#5, PR #6)

### Fixed
- Two-tier refresh removes `nix-shell` dependency; pins `ccusage@18.0.5` (PR #4)
- Prominent vignette link in email header (PR #2)
- Stale data warning in email script
- Error handling improvements in email script

---

## 2026-02-21

### Added
- `ci: Add daily report workflow`
- `feat: Initial commit of llmtelemetry package`
- `DBI` added to DESCRIPTION and `setup-pandoc` to CI workflow
- `purrr` dependency added to CI and DESCRIPTION
