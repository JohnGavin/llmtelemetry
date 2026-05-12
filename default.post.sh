#!/usr/bin/env bash
# Re-apply otelsdk derivation block to default.nix after rix regeneration.
# Must be idempotent: safe to run multiple times.
# Usage: ./default.post.sh [path/to/default.nix]
set -euo pipefail

NIX_FILE="${1:-$(dirname "$0")/default.nix}"

if grep -q "otelsdk = pkgs.rPackages.buildRPackage" "$NIX_FILE"; then
  echo "default.post.sh: otelsdk block already present in $NIX_FILE — nothing to do."
  exit 0
fi

# Insert the otelsdk derivation before the `shell = pkgs.mkShell` line.
# awk finds that marker and inserts the block once, then continues printing.
awk '
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
    print "    # curl and zlib: both out (lib) and dev (headers) needed for cmake MODULE-mode FindCURL/FindZLIB"
    print "    nativeBuildInputs = [ pkgs.which pkgs.cmake pkgs.protobuf_21 pkgs.pkg-config pkgs.curl pkgs.curl.dev pkgs.zlib pkgs.zlib.dev ];"
    print "    # Set CMAKE directly (avoids \`which cmake\` in configure) and explicit CMAKE_PREFIX_PATH"
    print "    # so cmake MODULE-mode FindCURL/FindZLIB resolve to Nix store paths in the derivation sandbox."
    print "    preBuild = \047\047"
    print "      export CMAKE=\"${pkgs.cmake}/bin/cmake\""
    print "      export CMAKE_PREFIX_PATH=\"${pkgs.curl.dev};${pkgs.curl};${pkgs.zlib.dev};${pkgs.zlib};\047\047${CMAKE_PREFIX_PATH:-}\""
    print "    \047\047;"
    print "    propagatedBuildInputs = builtins.attrValues {"
    print "      inherit (pkgs.rPackages) otel;"
    print "    };"
    print "  };"
    inserted = 1
  }
  { print }
' "$NIX_FILE" > "${NIX_FILE}.tmp"

mv "${NIX_FILE}.tmp" "$NIX_FILE"

# Patch buildInputs to include otelsdk if not already there.
if ! grep -q "otelsdk" "$NIX_FILE"; then
  sed -i.bak 's/buildInputs = \[ rpkgs system_packages \];/buildInputs = [ rpkgs system_packages otelsdk ];/' "$NIX_FILE"
  rm -f "${NIX_FILE}.bak"
fi

echo "default.post.sh: otelsdk block injected into $NIX_FILE."
