#!/usr/bin/env bash
# .nix-shellhook.sh — Fix R_LIBS_SITE for nested nix-shell
# Prevents segfault from ABI mismatch when entering this shell
# from inside another nix-shell (e.g., global dev shell).
# See nix-nested-shell-isolation rule.

R_LIBS_SITE=""
for pkg in $buildInputs; do
  for dep in $(nix-store -qR "$pkg" 2>/dev/null); do
    if [ -d "$dep/library" ]; then
      case ":$R_LIBS_SITE:" in
        *":$dep/library:"*) ;;
        *) R_LIBS_SITE="${R_LIBS_SITE:+$R_LIBS_SITE:}$dep/library" ;;
      esac
    fi
  done
done
export R_LIBS_SITE
unset R_LIBS_USER
unset R_LIBS
