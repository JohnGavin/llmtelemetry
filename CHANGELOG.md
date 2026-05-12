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
