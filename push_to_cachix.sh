#!/usr/bin/env bash
#
# push_to_cachix.sh - Push ONLY this project's R package to johngavin cachix
#
# CRITICAL: Only the single project derivation is pushed. Standard R packages
# (dplyr, duckdb, jsonlite, etc.) are ALREADY on rstats-on-nix and must NEVER
# be pushed to johngavin. Uses `cachix watch-exec` which pushes ONLY paths
# that are actually built (not substituted from rstats-on-nix).
#
# Step 5 of 9-step workflow. Builds from package.nix, pushes ONE derivation.
#
# Usage:
#   ./push_to_cachix.sh
#
# Exit codes:
#   0 - Success
#   1 - General error
#   2 - Validation failed (missing files, not authenticated)
#   3 - Build failed (nix-build error)
#   4 - Push failed (cachix push error)
#   5 - Pin failed (cachix pin error)

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Error handling
trap 'handle_error $? $LINENO' ERR

handle_error() {
  local exit_code=$1
  local line_number=$2
  echo -e "${RED}ERROR: Script failed at line ${line_number} with exit code ${exit_code}${NC}"
  echo -e "${YELLOW}Check the error message above for details${NC}"
  exit $exit_code
}

log_step() { echo -e "${BLUE}$1${NC}"; }
log_success() { echo -e "${GREEN}$1${NC}"; }
log_error() { echo -e "${RED}ERROR: $1${NC}"; }
log_warning() { echo -e "${YELLOW}WARNING: $1${NC}"; }
log_info() { echo -e "${NC}$1${NC}"; }

# Retry function with exponential backoff
retry_command() {
  local max_attempts="${1:-3}"
  local timeout="${2:-5}"
  local command="${@:3}"
  local attempt=1

  while [ $attempt -le $max_attempts ]; do
    if eval "$command"; then
      return 0
    else
      if [ $attempt -lt $max_attempts ]; then
        local wait_time=$((timeout * attempt))
        log_warning "Attempt $attempt/$max_attempts failed. Retrying in ${wait_time}s..."
        sleep $wait_time
        ((attempt++))
      else
        log_error "All $max_attempts attempts failed"
        return 1
      fi
    fi
  done
}

main() {
  echo "==========================================================="
  echo "   Push llmtelemetry to johngavin cachix"
  echo "==========================================================="
  echo ""

  # STEP 1: Validate environment
  log_step "Step 1/4: Validating environment..."

  if [ ! -f "DESCRIPTION" ]; then
    log_error "DESCRIPTION not found. Run from llmtelemetry package root."
    exit 2
  fi

  if [ ! -f "package.nix" ]; then
    log_error "package.nix not found. Create it first."
    exit 2
  fi

  if ! command -v nix-build &> /dev/null; then
    log_error "nix-build not found. Install Nix first."
    exit 2
  fi

  if ! command -v cachix &> /dev/null; then
    log_error "cachix not found."
    log_info "Install: nix-env -iA cachix -f https://cachix.org/api/v1/install"
    exit 2
  fi

  log_success "Environment validated"
  echo ""

  # STEP 2: Get package info
  log_step "Step 2/4: Reading package information..."

  PKG_NAME=$(grep "^Package:" DESCRIPTION | awk '{print $2}' | tr -d '\r' || echo "")
  PKG_VERSION=$(grep "^Version:" DESCRIPTION | awk '{print $2}' | tr -d '\r' || echo "")

  if [ -z "$PKG_NAME" ] || [ -z "$PKG_VERSION" ]; then
    log_error "Could not read package name/version from DESCRIPTION"
    exit 2
  fi

  log_success "Package: $PKG_NAME v$PKG_VERSION"
  echo ""

  # STEP 3: Build and push package with cachix watch-exec
  #
  # CRITICAL: `echo $PATH | cachix push` and `cachix push $PATH` BOTH
  # push the ENTIRE closure (all dependencies). This wastes quota pushing
  # dplyr, duckdb, etc. that are already on rstats-on-nix.
  #
  # `cachix watch-exec` watches the nix-build and pushes ONLY paths that
  # are actually BUILT (not substituted from rstats-on-nix). This means
  # only the llmtelemetry derivation itself gets pushed.
  #
  log_step "Step 3/4: Building and pushing $PKG_NAME via cachix watch-exec..."
  log_info "Only locally-built paths are pushed (deps substituted from rstats-on-nix are skipped)"

  PUSH_LOG="/tmp/cachix-push-${PKG_NAME}.log"
  # Note: --watch-mode auto is the default, no need to specify explicitly
  if ! cachix watch-exec johngavin -- \
       nix-build package.nix --no-out-link 2>&1 | tee "$PUSH_LOG"; then
    log_error "Build or push failed"
    log_info "Log: $PUSH_LOG"
    log_info "Check syntax: nix-instantiate --parse package.nix"
    exit 3
  fi

  RESULT=$(nix-build package.nix --no-out-link 2>/dev/null)
  log_success "Built and pushed: $RESULT"

  # Verify push count
  PUSH_COUNT=$(grep -c "^Pushing " "$PUSH_LOG" 2>/dev/null || echo "0")
  if [ "$PUSH_COUNT" -gt 1 ]; then
    log_warning "Pushed $PUSH_COUNT paths (expected 0-1). Dependencies may have been built locally."
    log_info "Ensure 'cachix use rstats-on-nix' is configured for substitution."
  else
    log_success "Push count: $PUSH_COUNT (expected: 0 if cached, 1 if new)"
  fi
  echo ""

  # STEP 4: Pin package (only for release versions)
  log_step "Step 4/4: Pinning $PKG_NAME v$PKG_VERSION..."

  if [[ "$PKG_VERSION" == *.9000 ]]; then
    log_warning "Development version detected (.9000 suffix)"
    log_info "Skipping pin - dev versions subject to garbage collection"
    echo ""
  else
    PIN_NAME="${PKG_NAME}-v${PKG_VERSION}"
    log_info "Release version - pinning forever"
    log_info "Pin name: $PIN_NAME"

    if ! retry_command 3 5 "cachix pin johngavin '$PIN_NAME' '$RESULT' --keep-forever"; then
      log_error "Failed to pin package after 3 attempts"
      log_warning "Package pushed but not pinned - may be garbage collected"
      log_info "Manually pin: cachix pin johngavin $PIN_NAME $RESULT --keep-forever"
      exit 5
    fi

    log_success "Pinned as $PIN_NAME (protected from GC forever)"
    echo ""
  fi

  # SUCCESS
  echo "==========================================================="
  if [[ "$PKG_VERSION" == *.9000 ]]; then
    log_success "SUCCESS! $PKG_NAME pushed to cachix (dev version, not pinned)"
  else
    log_success "SUCCESS! $PKG_NAME pushed and pinned to cachix"
  fi
  echo "==========================================================="
  echo ""
  echo "Pushed to johngavin cache:"
  if [[ "$PKG_VERSION" == *.9000 ]]; then
    echo "   $PKG_NAME v$PKG_VERSION ONLY (dev version - subject to GC)"
  else
    echo "   $PKG_NAME v$PKG_VERSION ONLY (pinned forever)"
  fi
  echo "   Dependencies NOT pushed (they are on rstats-on-nix)"
  echo ""
  echo "Links:"
  echo "   Monitor cache: https://app.cachix.org/cache/johngavin"
  echo "   Package: $RESULT"
  echo ""
}

main "$@"
