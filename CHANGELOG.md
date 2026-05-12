# Changelog

All notable changes to this project are documented here.
Format follows [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).
Versions use date stamps (package pre-1.0; semver applies from first public release).

Auto-refresh cache commits (`chore: Auto-refresh ccusage cache …`) are omitted
— they are routine CI noise. Check `git log --oneline --grep "Auto-refresh"` for the
full refresh history.

---

## [Unreleased]

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
