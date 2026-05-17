# DuckDB-WASM Phase 0 Feasibility Prototype

Issue [#83](https://github.com/JohnGavin/llmtelemetry/issues/83) — centralised PIT telemetry DB on Parquet via DuckDB-WASM.

## Files

| File | Purpose |
|------|---------|
| `generate_test_data.R` | Writes `test.parquet` — 20 rows, schema matching the planned `costs`/`sessions` table |
| `test.parquet` | 20-row Parquet (1.7 KB, ZSTD-compressed) |
| `step3_js_only.qmd` | Step 3: DuckDB-WASM loads and queries `test.parquet`, JS only, no WebR |
| `step4_with_webr.qmd` | Step 4: same DuckDB-WASM block + Shinylive iframe to test coexistence |
| `_extensions/` | Shinylive Quarto extension (copied from main checkout — not used by step 4 due to missing R package, kept for reference) |

> **Note:** Rendered HTML files (`*.html`, `*_files/`) are excluded by `.gitignore` and must be built locally with `quarto render`.

## Local Test Commands

Serve the prototype directory over HTTP (DuckDB-WASM requires HTTP, not `file://`):

```bash
# From the repo root:
quarto render prototypes/duckdb-wasm/step3_js_only.qmd
quarto render prototypes/duckdb-wasm/step4_with_webr.qmd
python3 -m http.server 8888 --bind 127.0.0.1 --directory prototypes/duckdb-wasm
```

Then open in Chrome:

- **Step 3 (JS only):** http://127.0.0.1:8888/step3_js_only.html
- **Step 4 (WebR + DuckDB-WASM):** http://127.0.0.1:8888/step4_with_webr.html

## What Success Looks Like

### Step 3 — DuckDB-WASM queries same-origin Parquet

The `<pre id="output">` block should show green text with all these lines:

```
[Xms] DuckDB-WASM module imported
[Xms] Bundle selected: <URL to .wasm file>
[Xms] DuckDB instantiated
[Xms] Connection established
[Xms] Query returned 5 rows

SAMPLE ROWS (project='llm', LIMIT 5):
[{"project":"llm","date":"2026-05-01","session_id":"sess-0001",...}, ...]

COUNTS BY PROJECT:
[{"project":"llm","n":"7"},{"project":"llmtelemetry","n":"6"},...]

COST BY PROJECT:
[...]

✅ ALL PASSED in Xms total
```

**Q1 verdict signal:** If this works, the gh-pages same-origin storage design is viable.
The key difference from HuggingFace (which fails CORS on Range requests) is that
`python3 -m http.server` serves from localhost — no CORS preflight. A gh-pages deployment
of the dashboard would have the same advantage (same origin).

### Step 4 — WebR + DuckDB-WASM coexistence

The page should show:
1. The green DuckDB-WASM `<pre>` block completing (same as step 3)
2. Below it, a Shinylive iframe loading and showing "WebR alive"

Both should be visible without page crash or OOM.

## What to Check in Chrome DevTools

### Network tab

- `test.parquet`: should appear as a single GET request (small file, no Range requests
  needed for 1.7 KB). For a production-sized Parquet (>1 MB), DuckDB-WASM would issue
  HTTP Range requests for row-group pushdown — watch for `206 Partial Content` responses.
  No CORS preflight errors should appear.
- `.wasm` files: the DuckDB-WASM bundle (~5 MB) fetches from `cdn.jsdelivr.net`.
- Step 4 only: Shinylive assets fetch from `shinylive.io` CDN.

### Console tab

- No `SharedArrayBuffer not available` errors (these indicate missing COOP/COEP headers,
  which force DuckDB-WASM to fall back to the single-threaded bundle).
- No Worker registration errors.
- DuckDB-WASM logs worker messages to console — these are expected.

### Memory tab (Performance > Memory)

Take a heap snapshot after both runtimes have loaded. Expected usage:
- Step 3: 50–300 MB (DuckDB-WASM only)
- Step 4: 200–800 MB (DuckDB-WASM + WebR/Shinylive iframe)
- Alarm threshold: 2 GB (Shinylive docs recommend 4 GB tab limit)

## Timing Notes (approximate cold-load)

- `generate_test_data.R` ran in < 2 seconds (duckdb COPY to Parquet)
- `step3_js_only.qmd` rendered in ~3 seconds (no R chunks)
- `step4_with_webr.qmd` rendered in ~3 seconds (iframe fallback, no shinylive filter)

Browser-side timing will appear in the `<pre>` output once loaded.

## Known Limitations / Notes for Phase 1

1. **Shinylive R package missing from nix shell.** The `{shinylive-r}` Quarto filter
   requires the `shinylive` R package at render time. It is not in the llmtelemetry
   `default.nix`. Step 4 uses an `<iframe>` pointing to `shinylive.io` as a fallback.
   To use the filter: add `shinylive` to `default.R` and regenerate `default.nix`.
   See `nix-agent-shell-protocol` rule for the regeneration workflow.

2. **No SharedArrayBuffer headers.** `python3 -m http.server` does not set
   `Cross-Origin-Opener-Policy: same-origin` or `Cross-Origin-Embedder-Policy: require-corp`,
   which are required for `SharedArrayBuffer`. DuckDB-WASM detects this and silently falls
   back to the `eh` (exception-handling) single-threaded bundle. This is acceptable for
   prototyping; gh-pages or Cloudflare can be configured to serve these headers.

3. **test.parquet schema:** `date` and `valid_from` columns are stored as strings (DuckDB
   writes them as VARCHAR when the R input is character). The production schema should use
   `DATE` and `TIMESTAMPTZ`. Verify with:
   ```sql
   DESCRIBE SELECT * FROM 'test.parquet'
   ```
   Run this in the browser DevTools console after connecting:
   ```js
   (await (await db.connect()).query("DESCRIBE SELECT * FROM 'test.parquet'")).toArray()
   ```

4. **HF CORS confirmed broken.** Phase 0 comment on issue #83 documents that
   `huggingface.co` → `cas-bridge.xethub.hf.co` redirect breaks Range requests.
   The amended design (same-origin gh-pages parquet) sidesteps this. This prototype
   tests that amended assumption directly.
