Review for job 9001 (by codex)
------------------------------------------------------------
## Review Findings
- **Severity**: High
- **Location**: `R/scrape_data.R:287-307`
- **Problem**: `read_data_log()` rereads every rotated `data.log.*` sibling from byte `0` on every refresh. Once any rotated log exists, historical turns are replayed on all later runs, which can duplicate and corrupt output files.
- **Fix**: Only ingest rotated siblings when rotation is actually detected, or persist per-file offsets so previously consumed rotated logs are not replayed.

---
- **Severity**: High
- **Location**: `R/process_data.R:100-120`
- **Problem**: The incremental cache merge still treats the newest log slice as a full replacement for existing rows. Because the reader resumes from a saved byte offset, records spanning multiple refresh runs lose previously persisted values and become undercounted.
- **Fix**: Merge matching rows additively from existing and new data, or rebuild affected buckets from full history before rewriting the caches.

---
- **Severity**: Medium
- **Location**: `R/canonicalize.R:44-67`; `inst/scripts/export_data.R:71-99`
- **Problem**: Canonicalization still misses the underscore-form path now used in tracked repo paths. Those inputs fall through to a generic bucket instead of the expected project, so telemetry can be attributed to the wrong project.
- **Fix**: Add explicit overrides for the underscore-form path, or normalize underscore path segments before container-prefix stripping.

---
- **Severity**: Low
- **Location**: `tests/testthat/test-export.R:383-405`
- **Problem**: The CI fallback regression test is effectively disabled. It still searches for a helper that no longer exists in the export script, so the test skips unconditionally and cannot catch a future fallback regression.
- **Fix**: Rewrite the test against the current fallback logic or assert the emitted fallback output directly.

## Summary
The range adds substantial work, but the final code still has correctness gaps in incremental aggregation, fallback publishing, identifier normalization, and canonicalization.
