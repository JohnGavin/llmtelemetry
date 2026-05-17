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
    # Try to pull existing history; ignore error only if the remote repo is truly empty
    git -C "$BACKUP_DIR" fetch origin || { echo "ERROR: git fetch origin failed — aborting backup"; exit 1; }
    git -C "$BACKUP_DIR" checkout main 2>/dev/null || true
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
