# LLM Telemetry Data API

Machine-readable JSON endpoints for LLM usage telemetry across Claude and Gemini.

## Base URL

```
https://johngavin.github.io/llmtelemetry/data
```

## Discovery

The API index is available at:

```
GET /data/index.json
```

It contains the current version, list of all endpoints, their schemas, and data source references.

## Endpoints

### ccusage_daily.json

Daily Claude API usage aggregated by project.

| Column | Type | Description |
|--------|------|-------------|
| project | string | Shortened project path |
| date | string | Date in YYYY-MM-DD format |
| inputTokens | integer | Input tokens consumed |
| outputTokens | integer | Output tokens produced |
| cacheCreation | integer | Cache creation tokens |
| cacheRead | integer | Cache read tokens |
| totalTokens | integer | Total tokens (all types) |
| totalCost | number | Cost in USD (rounded to 4 decimal places) |
| modelsUsed | string | Comma-separated list of model names |

**Source:** `npx ccusage daily --json --instances`
**Tool:** [ccusage](https://www.npmjs.com/package/ccusage)

---

### ccusage_sessions.json

Claude API sessions with token and cost totals.

| Column | Type | Description |
|--------|------|-------------|
| sessionId | string | Shortened session identifier |
| inputTokens | integer | Input tokens consumed |
| outputTokens | integer | Output tokens produced |
| cacheCreation | integer | Cache creation tokens |
| cacheRead | integer | Cache read tokens |
| totalTokens | integer | Total tokens (all types) |
| totalCost | number | Cost in USD (rounded to 4 decimal places) |
| lastActivity | string | Last activity timestamp |
| modelsUsed | string | Comma-separated list of model names |

**Source:** `npx ccusage session --json --instances`
**Tool:** [ccusage](https://www.npmjs.com/package/ccusage)

---

### ccusage_blocks.json

Claude API usage grouped into contiguous time blocks (gaps and zero-cost blocks excluded).

| Column | Type | Description |
|--------|------|-------------|
| startTime | string | Block start time (ISO 8601) |
| endTime | string | Block scheduled end time (ISO 8601) |
| actualEndTime | string | Block actual end time (ISO 8601), may be null |
| entries | integer | Number of log entries in the block |
| inputTokens | integer | Input tokens consumed |
| outputTokens | integer | Output tokens produced |
| cacheCreation | integer | Cache creation input tokens |
| cacheRead | integer | Cache read input tokens |
| totalTokens | integer | Total tokens (all types) |
| costUSD | number | Cost in USD (rounded to 4 decimal places) |
| models | string | Comma-separated list of model names (excludes synthetic) |

**Source:** `npx ccusage blocks --json --instances`
**Tool:** [ccusage](https://www.npmjs.com/package/ccusage)

---

### gemini_daily.json

Daily Gemini API usage with token and cost totals.

| Column | Type | Description |
|--------|------|-------------|
| date | string | Date in YYYY-MM-DD format |
| total_tokens | integer | Total tokens consumed |
| total_cost | number | Cost in USD (rounded to 4 decimal places) |
| message_count | integer | Number of messages sent |

**Source:** Local Gemini session logs (`~/.gemini/tmp/`), parsed via DuckDB
**Tool:** [Google AI Studio](https://ai.google.dev/)

---

### gemini_sessions.json

Gemini API sessions with token and cost totals.

| Column | Type | Description |
|--------|------|-------------|
| sessionId | string | Session identifier |
| project | string | Project name |
| total_tokens | integer | Total tokens consumed |
| total_cost | number | Cost in USD (rounded to 4 decimal places) |
| start_time | string | Session start timestamp |
| last_updated | string | Last update timestamp |

**Source:** Local Gemini session logs (`~/.gemini/tmp/`), parsed via DuckDB
**Tool:** [Google AI Studio](https://ai.google.dev/)

---

### cmonitor_summary.json

Aggregated Anthropic cmonitor usage summary (single object, not an array).

| Column | Type | Description |
|--------|------|-------------|
| date_range | string | Date range covered (e.g. "2025-01-01 to 2025-03-09") |
| total_tokens | integer | Total tokens consumed |
| total_cost | number | Total cost in USD |
| entries | integer | Number of log entries |

**Source:** Anthropic cmonitor CLI text output (ANSI stripped, regex-parsed)
**Tool:** [Anthropic Docs](https://docs.anthropic.com/)

---

## Data Sources and Provenance

| Endpoint | Collection Tool | Method | Raw Data Location |
|----------|----------------|--------|-------------------|
| ccusage_daily | [ccusage](https://www.npmjs.com/package/ccusage) | `npx ccusage daily --json --instances` | `inst/extdata/ccusage_daily_all.json` |
| ccusage_sessions | [ccusage](https://www.npmjs.com/package/ccusage) | `npx ccusage session --json --instances` | `inst/extdata/ccusage_session_all.json` |
| ccusage_blocks | [ccusage](https://www.npmjs.com/package/ccusage) | `npx ccusage blocks --json --instances` | `inst/extdata/ccusage_blocks_all.json` |
| gemini_daily | DuckDB | Parse `~/.gemini/tmp/` session logs | `inst/extdata/gemini_usage.duckdb` |
| gemini_sessions | DuckDB | Parse `~/.gemini/tmp/` session logs | `inst/extdata/gemini_usage.duckdb` |
| cmonitor_summary | cmonitor CLI | Parse ANSI text output | `inst/extdata/cmonitor_daily.txt` |

## Refresh Cadence

Data is refreshed automatically via GitHub Actions:

- **Trigger:** Push to `main` affecting `vignettes/**`, `inst/extdata/**`, `inst/scripts/export_dashboard_data.R`, or `.github/workflows/deploy-dashboard.yaml`
- **Manual:** Workflow can be triggered via `workflow_dispatch`
- **Process:** Raw data in `inst/extdata/` is collected by a scheduled ccusage cache refresh job, then `export_dashboard_data.R` flattens/trims it to browser-friendly JSON in `vignettes/data/`, and the dashboard is rendered and deployed to GitHub Pages
- **Frequency:** Typically multiple times per day as new usage data arrives

## Usage Example

```r
library(jsonlite)

base_url <- "https://johngavin.github.io/llmtelemetry/data"

# Discover all endpoints
index <- fromJSON(paste0(base_url, "/index.json"))
index$endpoints$path

# Load daily Claude usage
daily <- fromJSON(paste0(base_url, "/ccusage_daily.json"))
head(daily)
```

```python
import requests

base_url = "https://johngavin.github.io/llmtelemetry/data"

# Discover all endpoints
index = requests.get(f"{base_url}/index.json").json()
for ep in index["endpoints"]:
    print(ep["path"], "-", ep["description"])

# Load daily Claude usage
daily = requests.get(f"{base_url}/ccusage_daily.json").json()
```
