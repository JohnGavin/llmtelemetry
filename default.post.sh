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
    print "    # CMAKE pre-set so configure skips \`which cmake\` (not in Nix sandbox PATH)."
    print "    # CMAKE_PREFIX_PATH intentionally NOT set: cmake finds CURL/ZLIB via macOS system paths"
    print "    # accessible in the Nix Darwin sandbox. Semicolons in CMAKE_PREFIX_PATH are bash statement"
    print "    # separators — embedding them in Makevars recipes causes the remaining path segments to be"
    print "    # executed as shell commands (\"Is a directory\" errors)."
    print "    preBuild = \047\047"
    print "      export CMAKE=\"${pkgs.cmake}/bin/cmake\""
    print "    \047\047;"
    print "    # src/CMakeLists.txt lives inside src/vendor/opentelemetry-cpp.tgz (extracted at build time,"
    print "    # not at patch time). Patch src/Makevars.in instead — it controls the cmake flags passed to"
    print "    # the configure step. Adding FETCHCONTENT_FULLY_DISCONNECTED=ON prevents ALL FetchContent"
    print "    # network downloads (including googletest git-clone) regardless of BUILD_TESTING value."
    print "    postPatch = \047\047"
    print "      sed -i \047s|-DWITH_ETW=OFF)|-DWITH_ETW=OFF -DFETCHCONTENT_FULLY_DISCONNECTED=ON)|\047 src/Makevars.in"
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
