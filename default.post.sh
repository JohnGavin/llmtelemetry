#!/usr/bin/env bash
# Re-apply otelsdk derivation block to default.nix after rix regeneration.
# Must be idempotent: safe to run multiple times.
# Usage: called automatically after `Rscript default.R` regenerates default.nix.
set -euo pipefail

NIX_FILE="${1:-$(dirname "$0")/default.nix}"

if grep -q "otelsdk = pkgs.rPackages.buildRPackage" "$NIX_FILE"; then
  echo "default.post.sh: otelsdk block already present in $NIX_FILE — nothing to do."
  exit 0
fi

# Inject otelsdk derivation block after the system_packages closing brace.
# Uses awk to locate the marker and insert the block exactly once.
MARKER='  system_packages = builtins.attrValues {'

awk -v marker="$MARKER" '
  /  shell = pkgs\.mkShell \{/ && !inserted {
    print ""
    print "  # otelsdk: GitHub-only (not on CRAN). Requires cmake + protobuf_21 at build time."
    print "  # nativeBuildInputs supplies the cmake/protobuf toolchain to the nix derivation build env."
    print "  # rev pinned to otelsdk 0.2.4.9000 (2026-05-11). Update when new release available."
    print "  otelsdk = pkgs.rPackages.buildRPackage {"
    print "    name = \"otelsdk\";"
    print "    src = pkgs.fetchgit {"
    print "      url = \"https://github.com/r-lib/otelsdk\";"
    print "      rev = \"0cf2ad93631944d28142e941ae9ac5cab107f478\";"
    print "      sha256 = \"sha256-xYKmrniWGBx9TaoMOxqGpgd2FEG4fU8DP279APmk2sU=\";"
    print "    };"
    print "    nativeBuildInputs = [ pkgs.which pkgs.cmake pkgs.protobuf_21 pkgs.pkg-config ];"
    print "    propagatedBuildInputs = builtins.attrValues {"
    print "      inherit (pkgs.rPackages) otel;"
    print "    };"
    print "  };"
    inserted = 1
  }
  { print }
' "$NIX_FILE" > "${NIX_FILE}.tmp"

mv "${NIX_FILE}.tmp" "$NIX_FILE"

# Patch buildInputs to include otelsdk (idempotent: only if not already present).
if ! grep -q "otelsdk" "$NIX_FILE" | grep -q "buildInputs"; then
  sed -i.bak 's/buildInputs = \[ rpkgs system_packages \];/buildInputs = [ rpkgs system_packages otelsdk ];/' "$NIX_FILE"
  rm -f "${NIX_FILE}.bak"
fi

echo "default.post.sh: otelsdk block injected into $NIX_FILE."
