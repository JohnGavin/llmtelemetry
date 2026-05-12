# Fix for issue #59: add otelsdk to Nix environment
# Branch: fix-issue-59-nix-otelsdk
# PR: https://github.com/JohnGavin/llmtelemetry/pull/60
# Resolved: 2026-05-12

# ============================================================
# PROBLEM
# ============================================================
# otelsdk is GitHub-only (not on CRAN). It wraps the OpenTelemetry C++ SDK
# and requires cmake + protobuf_21 at build time. rix::rix() git_pkgs does
# not expose nativeBuildInputs, so the derivation must be written manually
# in default.nix.

# ============================================================
# SOLUTION: manual buildRPackage derivation in default.nix
# ============================================================
# Key decisions:
#
# 1. nativeBuildInputs: pkgs.which + pkgs.cmake + pkgs.protobuf_21 +
#    pkgs.pkg-config + pkgs.curl + pkgs.curl.dev + pkgs.zlib + pkgs.zlib.dev
#    - pkgs.which: configure script uses `which cmake` (not in Nix sandbox PATH)
#    - curl/zlib + .dev: cmake MODULE-mode FindCURL/FindZLIB needs both lib + headers
#
# 2. preBuild: export CMAKE="${pkgs.cmake}/bin/cmake"
#    - configure script reads $CMAKE env var; avoids `which cmake` lookup
#
# 3. postPatch: sed on src/Makevars.in to add -DFETCHCONTENT_FULLY_DISCONNECTED=ON
#    - cmake/googletest.cmake uses FetchContent which git-clones — blocked in Nix sandbox
#    - src/CMakeLists.txt is INSIDE src/vendor/opentelemetry-cpp.tgz (extracted at build
#      time, not patch time) — cannot be patched via postPatch
#    - src/Makevars.in IS available at patch time and controls all cmake flags
#    - FETCHCONTENT_FULLY_DISCONNECTED=ON prevents ALL FetchContent network downloads
#      regardless of BUILD_TESTING value (cmake 4.x evaluates FetchContent at configure
#      time before BUILD_TESTING guard takes effect)
#
# 4. CMAKE_PREFIX_PATH NOT set:
#    - macOS system /usr/lib/libcurl.dylib is accessible in Nix Darwin sandbox
#    - Semicolons in CMAKE_PREFIX_PATH are bash statement separators — embedding
#      semicolon-separated paths in Makevars recipes causes "Is a directory" errors
#
# 5. propagatedBuildInputs: otel (CRAN package providing the R interface)
#
# 6. default.post.sh: idempotent script re-applying the otelsdk block after any
#    future `Rscript default.R` regeneration

# ============================================================
# FAILED APPROACHES
# ============================================================
# - postPatch on src/CMakeLists.txt — file doesn't exist in package root
# - postPatch on CMakeLists.txt — same, it's inside the vendored tarball
# - Setting CMAKE_PREFIX_PATH with semicolons in preBuild — semicolons act as
#   bash statement separators in Makevars recipes, causing make to try to execute
#   the remaining path segments as shell commands

# ============================================================
# VERIFICATION
# ============================================================
# nix-shell /Users/johngavin/docs_gh/llmtelemetry/default.nix \
#   --run "Rscript -e 'library(otelsdk); cat(\"otelsdk OK\n\")'"
# => otelsdk OK
