# Migration Plan: `llmtelemetry`

**Goal:** Create a standalone, independent R package `llmtelemetry` to handle cost tracking, visualization, and automated reporting.

**Destination:** `/Users/johngavin/docs_gh/proj/data/llm/telemetry/`
**Package Name:** `llmtelemetry`

## 1. Directory Structure

```
llmtelemetry/
├── DESCRIPTION          # Imports: duckdb, dplyr, ggplot2, jsonlite, blastula, here
├── NAMESPACE
├── default.nix          # Nix environment WITH ggplot2 (needs Cairo)
├── default.sh           # Shell entry point
├── R/
│   ├── ccusage.R        # Core logic
│   ├── email_report.R   # Logic to generate and send email
│   └── utils.R
├── inst/
│   ├── extdata/         # Data storage (ccusage_*.json)
│   ├── logs/            # Log files
│   └── templates/       # Email HTML templates
```

## 2. Dependency Strategy

*   **`llmtelemetry`** generates plots (e.g., cost trends).
*   Therefore, it **MUST** depend on `ggplot2`.
*   Therefore, its Nix environment **MUST** include `Cairo`, `libpng`, etc.
*   **Correction:** Unlike `llmcontent` (which is just a registry), `llmtelemetry` is an *application* that runs code, so it needs the full runtime environment.

## 3. Context Management
*   `llmtelemetry` has `ggplot2` in its `DESCRIPTION`.
*   To get API context for `ggplot2`, it relies on `llmcontent`.
*   `llmcontent` stores `inst/ctx/external/ggplot2.ctx.yaml`.
*   `llmtelemetry` does **not** need to generate this file itself; it consumes it from the registry.

## 4. Implementation Steps

### A. Initialize
1.  Run `usethis::create_package(".")`.
2.  **Undo previous errors:** Ensure no context generation logic resides here.
3.  Setup Nix environment (`duckdb`, `dplyr`, `ggplot2`...).

### B. Code Migration
1.  Move `R/ccusage.R` from `llm` repo.
2.  Ensure paths use `system.file(..., package="llmtelemetry")`.

## 5. What NOT To Do
*   **Do NOT** copy context generation scripts here.
*   **Do NOT** include contexts for transitive dependencies (e.g., `rlang`) unless directly imported.
