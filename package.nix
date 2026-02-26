# package.nix - Build llmtelemetry as an installable R package derivation
#
# Used by push_to_cachix.sh to build and push to johngavin cachix.
# Uses same rstats-on-nix pin as default.nix (2026-01-05).
#
# Usage:
#   nix-build package.nix --no-out-link
#   # Push ONLY this package (not deps!) via watch-exec:
#   cachix watch-exec johngavin --watch-mode auto -- nix-build package.nix --no-out-link

let
  pkgs = import (fetchTarball "https://github.com/rstats-on-nix/nixpkgs/archive/2026-01-05.tar.gz") {};

  llmtelemetry = pkgs.rPackages.buildRPackage {
    name = "llmtelemetry";
    src = ./.;

    # Imports from DESCRIPTION
    propagatedBuildInputs = with pkgs.rPackages; [
      checkmate
      cli
      DBI
      dplyr
      duckdb
      here
      jsonlite
      purrr
      scales
      tibble
    ];
  };

in llmtelemetry
