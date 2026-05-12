# Investigation log for issue #58
# Title: investigate: leverage Posit OpenTelemetry integration for LLM telemetry gaps
# URL: https://github.com/JohnGavin/llmtelemetry/issues/58
# Blog post: https://opensource.posit.co/blog/2026-05-07_opentelemetry/

# Session setup
library(devtools)

# --- Step 1: Read ellmer OTel integration ---
# Does it capture tokens per call or just spans?
# browseURL("https://github.com/r-lib/otel")
# browseURL("https://github.com/r-lib/otelsdk")

# --- Step 2: Evaluate backend options ---
# Logfire (Pydantic), Grafana Cloud, Jaeger (self-hosted)
# For a single-developer OSS project, Jaeger local is lowest friction to start.
# docker run -d --name jaeger -p 16686:16686 -p 4318:4318 jaegertracing/all-in-one:latest

# --- Step 3: Prototype instrumentation ---
# Add otelsdk to default.R:
# r_pkgs = c(...existing..., "otelsdk")
#
# Env vars needed:
# OTEL_TRACES_EXPORTER=otlp
# OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
# OTEL_SERVICE_NAME=llmtelemetry
#
# Then run the Shinylive render pipeline and inspect spans at http://localhost:16686

# --- Step 4: Gap analysis questions ---
# Q1: Does ellmer OTel report prompt/completion token counts?
#     ccusage reports these from ~/.claude/... — OTel may duplicate or extend this.
# Q2: Does OTel capture cost (USD) or only latency/token counts?
#     Hypothesis: OTel = latency + structure; ccusage = cost + billing accuracy.
# Q3: What spans appear for a tar_make() run using crew/mirai workers?
#     Expected: task dispatch spans from mirai ≥2.5.0 instrumentation.
# Q4: What DBI spans appear for unified.duckdb reads?
#     Expected: query text, duration, row count.

# --- Commands to run during investigation ---

# Check if otelsdk is available
# nix-shell ~/docs_gh/llmtelemetry/default.nix --run "Rscript -e 'packageVersion(\"otelsdk\")'"

# Run a traced Rscript to inspect what otel captures for httr2
# Sys.setenv(OTEL_TRACES_EXPORTER = "console")  # dump to stdout for quick check
# library(otelsdk)
# library(httr2)
# httr2::request("https://example.com") |> httr2::req_perform()

# --- Outcome tracking ---
# [ ] ellmer token capture: confirmed / not available / partial
# [ ] Backend selected: ___
# [ ] Prototype spans collected: yes / no
# [ ] Gap analysis written: yes / no
# [ ] Decision: OTel alongside ccusage / replace subset / defer
