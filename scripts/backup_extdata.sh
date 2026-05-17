#!/usr/bin/env bash
# Backup llmtelemetry non-reproducible data to a separate GitHub repo.
# Run daily (or after any significant data update). Idempotent — exits 0 with no-op when nothing changed.
set -euo pipefail

BACKUP_DIR="$HOME/.llmtelemetry-data-backup"
DATA_REMOTE="https://github.com/JohnGavin/llmtelemetry-data.git"
SRC_ROOT="/Users/johngavin/docs_gh/llmtelemetry"
LOG="$HOME/.claude/logs/llmtelemetry_backup.log"

log() { echo "$(date '+%Y-%m-%d %H:%M:%S') $*" >> "$LOG"; }
log "backup_extdata.sh: start"

# ------------------------------------------------------------------
# Initialise local backup clone (once)
# ------------------------------------------------------------------
if [ ! -d "$BACKUP_DIR/.git" ]; then
    mkdir -p "$BACKUP_DIR"
    git -C "$BACKUP_DIR" init -b main
    git -C "$BACKUP_DIR" remote add origin "$DATA_REMOTE"
    # Fetch existing history from remote (may be empty on first ever use)
    git -C "$BACKUP_DIR" fetch origin 2>/dev/null || true
    # If origin/main already exists, reset local branch to it before adding
    # new content — avoids an unrelated root commit that would be rejected as
    # a non-fast-forward push (#877).
    if git -C "$BACKUP_DIR" rev-parse --verify origin/main >/dev/null 2>&1; then
        git -C "$BACKUP_DIR" checkout -B main origin/main
    fi
fi

# ------------------------------------------------------------------
# Sync data directories (clear-then-copy to handle deletions)
# ------------------------------------------------------------------
rm -rf "$BACKUP_DIR/extdata" "$BACKUP_DIR/vignettes_data"
cp -R "$SRC_ROOT/inst/extdata" "$BACKUP_DIR/extdata"

if [ -d "$SRC_ROOT/vignettes/data" ]; then
    cp -R "$SRC_ROOT/vignettes/data" "$BACKUP_DIR/vignettes_data"
fi

# ------------------------------------------------------------------
# Commit and push only when something changed
# ------------------------------------------------------------------
git -C "$BACKUP_DIR" add -A
if git -C "$BACKUP_DIR" diff --cached --quiet; then
    log "backup_extdata.sh: no changes — skipping push"
    exit 0
fi

git -C "$BACKUP_DIR" commit -m "chore: backup $(date '+%Y-%m-%d %H:%M:%S')"
git -C "$BACKUP_DIR" push origin main
log "backup_extdata.sh: pushed to $DATA_REMOTE"
