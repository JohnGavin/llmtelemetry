#!/usr/bin/env bash
# refresh_codexbar.sh — Capture CodexBar usage+cost JSON and sanitise.
#
# PURPOSE
# -------
# 1. Capture `codexbar usage --provider all --json` (may exit non-zero on
#    single-provider auth failure; stdout JSON is still valid for other providers).
# 2. Capture `codexbar cost --provider all --json`.
# 3. Call sanitize_codexbar.R to strip PII and write the clean output files
#    to inst/extdata/ (codexbar_usage.json and codexbar_cost_daily.json).
#
# DESIGN CONSTRAINTS
# ------------------
# - No git commit/push step here — handled separately downstream.
# - Graceful skip when codexbar is not installed (CI / headless machines).
# - `set -euo pipefail` is active but `codexbar usage` non-zero exit is
#   handled explicitly (we tolerate it and still use stdout JSON).
# - Log file: inst/logs/codexbar_refresh.log
#
# See also: inst/scripts/sanitize_codexbar.R
#           R/codexbar.R (parsers)

set -uo pipefail
# NOTE: we do NOT use `set -e` unconditionally because `codexbar usage` can
# legitimately exit non-zero (single-provider 401) while emitting valid JSON.
# We handle the exit code explicitly below.

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
export PATH="/nix/var/nix/profiles/default/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:${PATH:-}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

LOG_DIR="${PKG_ROOT}/inst/logs"
LOG_FILE="${LOG_DIR}/codexbar_refresh.log"
LOCK_FILE="/tmp/codexbar_refresh.lock"

mkdir -p "${LOG_DIR}"

log() {
  printf '%s: %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$1" >> "${LOG_FILE}"
}

# ---------------------------------------------------------------------------
# Lock (prevent concurrent runs)
# ---------------------------------------------------------------------------
if [ -f "${LOCK_FILE}" ]; then
  existing_pid="$(cat "${LOCK_FILE}" 2>/dev/null || true)"
  if [ -n "${existing_pid}" ] && kill -0 "${existing_pid}" 2>/dev/null; then
    log "SKIP: another instance running (PID ${existing_pid})"
    exit 0
  fi
fi
printf '%d' "$$" > "${LOCK_FILE}"
trap 'rm -f "${LOCK_FILE}"' EXIT

# ---------------------------------------------------------------------------
# Graceful skip when codexbar is unavailable (CI / headless)
# ---------------------------------------------------------------------------
if ! command -v codexbar > /dev/null 2>&1; then
  log "SKIP: codexbar not found on PATH — skipping capture (CI/headless environment)"
  exit 0
fi

log "Starting CodexBar refresh (codexbar $(codexbar --version 2>/dev/null || echo unknown))"

# ---------------------------------------------------------------------------
# Capture to temp files
# ---------------------------------------------------------------------------
TMP_USAGE="$(mktemp /tmp/codexbar_usage_raw.XXXXXX.json)"
TMP_COST="$(mktemp  /tmp/codexbar_cost_raw.XXXXXX.json)"
trap 'rm -f "${TMP_USAGE}" "${TMP_COST}"; rm -f "${LOCK_FILE}"' EXIT

# --- usage (tolerate non-zero exit: single-provider 401 is expected) ---
log "Capturing: codexbar usage --provider all --json"
usage_exit=0
codexbar usage --provider all --json > "${TMP_USAGE}" 2>> "${LOG_FILE}" || usage_exit=$?
if [ "${usage_exit}" -ne 0 ]; then
  log "WARNING: codexbar usage exited ${usage_exit} — proceeding with stdout JSON (partial providers OK)"
fi

# Verify we got at least some JSON (non-empty output)
if [ ! -s "${TMP_USAGE}" ]; then
  log "ERROR: codexbar usage produced no output — aborting"
  exit 1
fi

# --- cost (should always succeed; abort on non-zero) ---
log "Capturing: codexbar cost --provider all --json"
if ! codexbar cost --provider all --json > "${TMP_COST}" 2>> "${LOG_FILE}"; then
  log "ERROR: codexbar cost exited non-zero — aborting"
  exit 1
fi

if [ ! -s "${TMP_COST}" ]; then
  log "ERROR: codexbar cost produced no output — aborting"
  exit 1
fi

log "Raw JSON captured (usage: $(wc -c < "${TMP_USAGE}") bytes, cost: $(wc -c < "${TMP_COST}") bytes)"

# ---------------------------------------------------------------------------
# Sanitise via R script (strips PII, writes inst/extdata/ outputs)
# ---------------------------------------------------------------------------
log "Running sanitize_codexbar.R"

SANITIZE_SCRIPT="${PKG_ROOT}/inst/scripts/sanitize_codexbar.R"
if [ ! -f "${SANITIZE_SCRIPT}" ]; then
  log "ERROR: sanitize_codexbar.R not found at ${SANITIZE_SCRIPT}"
  exit 1
fi

# Always prefer the project's nix-shell so R resolves to the correct binary
# and here::here() cannot write to a different repo.  Pass the explicit
# output dir as arg 3 so the script never falls back to a cwd-relative path.
# Bare Rscript is a last resort; it also receives the explicit out-dir + cwd
# so it still writes to the right place.
if command -v nix-shell > /dev/null 2>&1 && [ -f "${PKG_ROOT}/default.nix" ]; then
  if (cd "${PKG_ROOT}" && nix-shell "${PKG_ROOT}/default.nix" --run "Rscript inst/scripts/sanitize_codexbar.R '${TMP_USAGE}' '${TMP_COST}' '${PKG_ROOT}/inst/extdata'") >> "${LOG_FILE}" 2>&1; then
    log "Sanitization complete (nix-shell)"
  else
    log "ERROR: sanitize_codexbar.R failed via nix-shell — check ${LOG_FILE}"
    exit 1
  fi
elif command -v Rscript > /dev/null 2>&1; then
  # Fallback: bare Rscript, still cwd=PKG_ROOT + explicit out-dir so it writes to the right place
  if (cd "${PKG_ROOT}" && Rscript inst/scripts/sanitize_codexbar.R "${TMP_USAGE}" "${TMP_COST}" "${PKG_ROOT}/inst/extdata") >> "${LOG_FILE}" 2>&1; then
    log "Sanitization complete (bare Rscript fallback)"
  else
    log "ERROR: sanitize_codexbar.R failed — check ${LOG_FILE}"
    exit 1
  fi
else
  log "ERROR: neither nix-shell nor Rscript available"
  exit 1
fi

log "Done — inst/extdata/codexbar_usage.json and codexbar_cost_daily.json updated"
