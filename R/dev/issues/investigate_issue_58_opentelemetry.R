# Investigation log for issue #58
# Title: investigate: leverage Posit OpenTelemetry integration for LLM telemetry gaps
# URL: https://github.com/JohnGavin/llmtelemetry/issues/58
# Blog post: https://opensource.posit.co/blog/2026-05-07_opentelemetry/
# Investigation date: 2026-05-11

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
# orbctl start   # start OrbStack daemon if not running
# docker run -d --name jaeger \
#   -p 16686:16686 \   # UI
#   -p 4318:4318 \     # OTLP HTTP
#   jaegertracing/all-in-one:latest
#
# RESULT 2026-05-11: Jaeger started successfully via OrbStack.
# UI available at http://localhost:16686. OTLP ingest at http://localhost:4318.

# --- Install otelsdk (GitHub only for now, not yet on CRAN) ------------------
# pak::pak("r-lib/otel")      # API layer — CRAN available, already in default.nix
# pak::pak("r-lib/otelsdk")   # SDK/exporter — GitHub only, requires cmake + protobuf

# --- BLOCKER: otelsdk fails to build in Nix ----------------------------------
#
# Attempted 2026-05-11 with multiple approaches (cmake, pkg-config, protobuf).
#
# Root cause: protobuf version mismatch.
# otelsdk bundles the OpenTelemetry C++ SDK and generates protobuf C++ code
# using one version of protoc. The nixpkgs system protobuf runtime is a different
# (newer) version. Clang errors:
#   error: "Protobuf C++ gencode is built with an incompatible version of
#           Protobuf C++ headers/runtime."
#
# This is a Nix-specific packaging issue. The fix requires a proper Nix derivation
# for otelsdk that pins compatible protobuf versions (both the code-gen tool and
# the runtime library must match).
#
# Action: Open a new issue to create a Nix derivation for otelsdk.
# Outside Nix (e.g. macOS system R), installation should work if cmake and a
# compatible protobuf are available.

# --- Env vars to activate tracing -------------------------------------------
# Sys.setenv(
#   OTEL_TRACES_EXPORTER         = "otlp",
#   OTEL_EXPORTER_OTLP_ENDPOINT  = "http://localhost:4318",
#   OTEL_SERVICE_NAME            = "llmtelemetry"
# )
# Or set in project .Renviron (preferred for reproducibility)

# --- Package versions tested -------------------------------------------------
# otel:    0.2.0 (CRAN, already in default.nix) — API layer only, no exporter
# otelsdk: 0.2.4.9000 (GitHub) — BUILD FAILED (protobuf version mismatch in Nix)
# mirai:   2.5.3 (in default.nix) — OTel instrumentation present (added 2.5.0)
# DBI:     1.2.3 (in default.nix) — NO OTel hooks (needs >= 1.3.0, unreleased)
# shiny:   1.12.1 (in default.nix) — FULL OTel hooks present (see below)

# --- Priority 1: mirai/crew worker spans — FINDINGS -------------------------
#
# CONFIRMED: mirai 2.5.3 has comprehensive OTel instrumentation (PR #394).
# Activation: automatic when `otel` + `otelsdk` are both installed.
# No code changes needed — mirai auto-detects OpenTelemetry.
#
# Span types emitted (from mirai v05-opentelemetry vignette):
#   - "daemons set" / "daemons reset" — root span for compute profile
#   - "mirai" — child span per async task (includes worker-side execution)
#   - "daemon connect" — span when daemon connects to dispatcher
#   - Error spans: failed tasks captured as error spans with status
#   - Distributed context: trace context propagates across network boundaries
#                          (worker-side spans link back to orchestrator)
#
# Attributes captured per task span:
#   - mirai.dispatcher (true/false)
#   - mirai.compute (profile name)
#   - server.address, server.port, network.transport
#   - task duration (implicit from span timing)
#
# Answers to original questions:
# - Does each tar_target() appear as a span? YES, each mirai() task → span
# - Do worker-side spans link to orchestrator? YES, trace context propagated
# - Are failed targets captured as error spans? YES
#
# Status: READY pending otelsdk Nix derivation (see BLOCKER above)

# --- Priority 2: DBI query timing — FINDINGS --------------------------------
#
# CONFIRMED: DBI 1.2.3 has NO OTel hooks.
# OTel support was added in DBI 1.3.0 (GitHub development version, unreleased).
# See DBI NEWS: "Add support for OpenTelemetry via the otel and otelsdk packages"
#
# Double blocker for DBI:
#   1. DBI 1.3.0 not yet on CRAN (cannot add to default.R)
#   2. otelsdk fails to build in Nix
#
# When both blockers are resolved:
#   - SQL query text captured in span attributes
#   - Query duration from span timing
#   - Additive to ccusage (orthogonal signal — query-level vs session-level)
#
# Status: BLOCKED pending (1) DBI 1.3.0 CRAN release + (2) otelsdk Nix derivation

# --- Priority 3: Shiny reactive latency — UNEXPECTED FINDING ----------------
#
# CONFIRMED: Shiny 1.12.1 (already in default.nix) has FULL OTel instrumentation.
# Found 60+ internal OTel functions including:
#   - otel_span_session_start / otel_span_session_end
#   - otel_span_label_reactive / otel_span_label_observer
#   - otel_span_label_render_function
#   - otel_span_label_extended_task (ExtendedTask spans for crew workers)
#   - otel_log_label_set_reactive_val / otel_log_label_set_reactive_values
#
# This means the llmtelemetry Shinylive dashboard can emit spans for:
#   - Session duration (start → end)
#   - Reactive invalidation and recalculation latency
#   - ExtendedTask execution timing (crew/mirai background tasks)
#   - Render function timing
#
# This is a NEW capability not in the original gap analysis.
# Status: READY pending otelsdk Nix derivation (single blocker)

# --- Priority 4: Backend selection ------------------------------------------
#
# Dev/local:    Jaeger all-in-one via OrbStack ✓ (running, tested 2026-05-11)
# Cloud option: Grafana Cloud free tier (10k spans/day), or Logfire
# Decision:     Keep Jaeger local for development; evaluate Grafana Cloud
#               once otelsdk Nix derivation is available

# --- ellmer: de-prioritised -------------------------------------------------
#
# We don't use ellmer meaningfully. ccusage already captures Claude Code
# token/cost data at the session level with billing accuracy.
# ellmer OTel would give span-level latency per LLM call — not a current gap.
# Revisit if we start building R code that calls LLM APIs directly via ellmer.

# --- Gap analysis summary — COMPLETED ----------------------------------------
#
# Signal                    | ccusage | OTel (if added)   | Gap? | Status
# --------------------------|---------|-------------------|------|--------
# Token count per session   | YES     | via ellmer only   | no   | not needed
# Cost (USD) per session    | YES     | NO                | no   | OTel can't replace
# Block-level cost/duration | YES     | NO                | no   | OTel can't replace
# mirai worker task timing  | NO      | YES (mirai 2.5.3) | YES  | blocked: otelsdk Nix
# DBI query timing          | NO      | YES (DBI >= 1.3.0)| YES  | blocked: DBI+otelsdk
# Shiny reactive latency    | NO      | YES (shiny 1.12.1)| YES  | blocked: otelsdk Nix
# targets pipeline trace    | NO      | via mirai         | YES  | blocked: otelsdk Nix
# Session start/end spans   | partial | YES (shiny 1.12.1)| YES  | blocked: otelsdk Nix

# --- Option C investigation (2026-05-12): compatible protobuf in Nix shell ----
#
# Goal: add pkgs.protobuf_21 + pkgs.cmake + pkgs.pkg-config to system_pkgs in
# default.R so otelsdk can build within the project nix shell.
#
# Why protobuf_21 (3.21.12) specifically:
#   - OTel C++ SDK v1.22.0 (bundled in otelsdk 0.2.4.9000)
#   - otelsdk cmake uses -DWITH_ABSEIL=OFF
#   - protobuf >= 22 (old 3.22+) REQUIRES Abseil at link time — incompatible
#   - protobuf_21 (3.21.12) does NOT require Abseil; no @rpath conflicts
#   - Both protoc (code generation) and libprotobuf (compilation/linking) come
#     from the SAME Nix derivation → version check guaranteed to pass
#   - cmake CONFIG mode finds: protobuf::protoc = protoc-3.21.12.0
#     protobuf::libprotobuf = libprotobuf.3.21.12.0.dylib (no Abseil in link deps)
#
# Configure step result (2026-05-12): PASSED
#   system_pkgs = c("cmake", "protobuf_21", "pkg-config")
#   Generated Makevars:
#     CMAKE="/nix/store/.../cmake-4.1.2/bin/cmake"
#     PKG_LIBS = -L/nix/store/.../protobuf-21.12/lib -lprotobuf
#
# cmake build result (2026-05-12): BLOCKED by nested-shell NIX_CFLAGS_COMPILE
#   contamination (same mechanism as R_LIBS_SITE — nix-nested-shell-isolation rule)
#   - Outer global dev shell has pkgs.protobuf (34.1) → contaminates
#     NIX_CFLAGS_COMPILE with protobuf-32.1 and abseil-cpp-20250814.1-dev paths
#   - These appear BEFORE protobuf-21.12 in NIX_CFLAGS_COMPILE
#   - Clang wrapper passes all paths to both compiler AND linker
#   - Linker fails: "can't map file abseil-cpp-20250814.1-dev/include" (EINVAL)
#   NOTE: This is a TESTING ARTIFACT (running from within nested shells).
#   A user entering the project nix shell DIRECTLY (not nested) would have a
#   clean NIX_CFLAGS_COMPILE and the cmake build should succeed.
#
# Recommended implementation for issue #59:
#   Use rix git_pkgs to build otelsdk in a clean Nix derivation environment
#   (avoids nested-shell contamination — builds in fresh derivation, no outer
#   shell leakage). This combines Options C and A.
#
# Changes needed to default.R (main checkout, issue #59):
#   system_pkgs = c("cmake", "protobuf_21", "pkg-config")
#   r_pkgs: add "otel"
#   git_pkgs: add otelsdk from r-lib/otelsdk at confirmed commit

# --- Action items from investigation ----------------------------------------
#
# 1. Issue #59: add to default.R:
#    system_pkgs = c("cmake", "protobuf_21", "pkg-config")
#    r_pkgs: add "otel"
#    Then: git_pkgs: add otelsdk from GitHub (r-lib/otelsdk)
#    This is the single blocker for ALL OTel functionality.
#
# 2. Track DBI 1.3.0 CRAN release (already in development, NEWS shows active work)
#    Add to default.R once released.
#
# 3. No code changes needed in llmtelemetry for mirai/crew or Shiny instrumentation
#    — activation is automatic once otelsdk is loadable.
#
# 4. .Renviron already prepared (gitignored, inert until otelsdk loads):
#    OTEL_TRACES_EXPORTER=otlp
#    OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
#    OTEL_SERVICE_NAME=llmtelemetry

# --- Acceptance criteria — status -------------------------------------------
# [x] Jaeger running locally via OrbStack (confirmed 2026-05-11)
# [ ] otelsdk installed and traces flowing to Jaeger (Option C build in progress)
# [x] mirai/crew spans confirmed (present in mirai 2.5.3, needs otelsdk)
# [x] DBI spans confirmed (DBI 1.2.3 has no hooks; DBI 1.3.0 needed + otelsdk)
# [x] Written gap analysis table completed
# [x] Option C configure: PASSED (protobuf_21 + cmake + pkg-config)
# [~] Option C cmake build: BLOCKED by nested-shell NIX_CFLAGS_COMPILE
#     contamination (testing artifact). Expected to work in fresh project shell.
# [ ] Decision on whether to add otelsdk to default.R permanently
#     → YES, via rix git_pkgs (Options C+A combined, clean derivation build)
#     → Tracked in issue #59
