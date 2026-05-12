# Investigation log for issue #58
# Title: investigate: leverage Posit OpenTelemetry integration for LLM telemetry gaps
# URL: https://github.com/JohnGavin/llmtelemetry/issues/58
# Blog post: https://opensource.posit.co/blog/2026-05-07_opentelemetry/

# --- What Jaeger is (no R package needed) ------------------------------------
#
# Jaeger = distributed tracing BACKEND + UI.
# It receives spans over OTLP (OpenTelemetry Protocol) and visualises the full
# call tree: which functions ran, in what order, how long, where errors occurred.
#
# There is NO separate R package for Jaeger.
# otelsdk (R) speaks OTLP natively → point at Jaeger's OTLP endpoint → done.
# Jaeger web UI: http://localhost:16686
# OTLP HTTP ingest: http://localhost:4318

# --- Run Jaeger all-in-one via OrbStack (not Docker Desktop) -----------------
#
# OrbStack is Docker-compatible but far lighter on Mac.
# Same docker commands work unchanged.
#
# docker run -d --name jaeger \
#   -p 16686:16686 \   # UI
#   -p 4318:4318 \     # OTLP HTTP
#   jaegertracing/all-in-one:latest

# --- Install otelsdk (GitHub only for now, not yet on CRAN) ------------------
# pak::pak("r-lib/otel")
# pak::pak("r-lib/otelsdk")

# --- Env vars to activate tracing -------------------------------------------
# Sys.setenv(
#   OTEL_TRACES_EXPORTER    = "otlp",
#   OTEL_EXPORTER_OTLP_ENDPOINT = "http://localhost:4318",
#   OTEL_SERVICE_NAME       = "llmtelemetry"
# )
# Or set in ~/.Renviron / project .Renviron

# --- Priority 1: mirai/crew worker spans ------------------------------------
#
# mirai >= 2.5.0 is OTel-instrumented.
# With otelsdk active, a tar_make() run using crew workers should produce spans
# for each worker task dispatch and execution.
#
# Test:
# library(otelsdk)
# library(targets)
# library(crew)
# tar_make()  # with OTEL env vars set
# Then inspect http://localhost:16686 — look for "mirai" service spans.
#
# Questions to answer:
# - Does each tar_target() appear as a span?
# - Do worker-side spans link back to the orchestrator (trace context propagation)?
# - Are failed targets captured as error spans?

# --- Priority 2: DBI query timing -------------------------------------------
#
# DBI >= 1.3.0 is OTel-instrumented.
# With otelsdk active, DBI calls to unified.duckdb should produce spans
# including query text and duration.
#
# Test:
# library(otelsdk)
# library(DBI)
# library(duckdb)
# con <- DBI::dbConnect(duckdb::duckdb(), "inst/extdata/unified.duckdb")
# DBI::dbGetQuery(con, "SELECT COUNT(*) FROM sessions")
# DBI::dbDisconnect(con)
# Then inspect http://localhost:16686 — look for "dbi" spans with SQL text.
#
# Questions to answer:
# - Is the SQL query text captured in span attributes?
# - Is row count captured?
# - Is this additive to ccusage or redundant?

# --- Priority 3: Backend selection ------------------------------------------
#
# Dev/local:    Jaeger all-in-one via OrbStack (no cost, disposable)
# Cloud option: Grafana Cloud free tier (10k spans/day), or Logfire
# Decision:     Start with Jaeger local; evaluate cloud if we want persistent traces

# --- ellmer: de-prioritised -------------------------------------------------
#
# We don't use ellmer meaningfully. ccusage already captures Claude Code
# token/cost data at the session level with billing accuracy.
# ellmer OTel would give span-level latency per LLM call — not a current gap.
# Revisit if we start building R code that calls LLM APIs directly via ellmer.

# --- Gap analysis summary (to be filled in after prototype) -----------------
#
# Signal                    | ccusage | OTel (if added) | Gap?
# --------------------------|---------|-----------------|------
# Token count per session   | YES     | via ellmer only | no (we don't use ellmer)
# Cost (USD) per session    | YES     | NO              | OTel can't replace
# Block-level cost/duration | YES     | NO              | OTel can't replace
# mirai worker task timing  | NO      | YES             | FILL IN AFTER TEST
# DBI query timing          | NO      | YES             | FILL IN AFTER TEST
# Shiny reactive latency    | NO      | YES (shiny>=1.12)| new capability
# targets pipeline trace    | NO      | UNKNOWN         | investigate

# --- Acceptance criteria (closes issue) -------------------------------------
# [ ] Jaeger running locally via OrbStack
# [ ] otelsdk installed and traces flowing to Jaeger
# [ ] mirai/crew spans confirmed (or documented as absent)
# [ ] DBI spans confirmed with SQL text (or documented as absent)
# [ ] Written gap analysis table completed above
# [ ] Decision on whether to add otelsdk to default.R permanently
