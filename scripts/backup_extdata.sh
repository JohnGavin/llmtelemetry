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
    # Probe remote: distinguish genuinely empty remote from transport/auth errors.
    # git ls-remote exit codes:
    #   0  — remote responded and returned refs (history exists; must fetch)
    #   2  — remote responded but has no matching refs (empty repo; safe to init)
    #   other (1, 128, …) — network/auth/config error; abort to avoid creating an
    #                        unrelated root commit that would fail as non-fast-forward.
    ls_remote_rc=0
    git -C "$BACKUP_DIR" ls-remote --exit-code --heads origin >/dev/null 2>&1 || ls_remote_rc=$?
    if [ "$ls_remote_rc" -eq 0 ]; then
        log "backup_extdata.sh: remote has refs — fetching history"
        if ! git -C "$BACKUP_DIR" fetch origin; then
            log "backup_extdata.sh: ERROR — fetch failed after remote reported refs; aborting"
            exit 1
        fi
        # Reset local branch to the fetched origin/main to avoid unrelated root commit (#877).
        git -C "$BACKUP_DIR" checkout -B main origin/main
    elif [ "$ls_remote_rc" -eq 2 ]; then
        log "backup_extdata.sh: remote is empty — proceeding with initial push"
    else
        log "backup_extdata.sh: ERROR — ls-remote exited $ls_remote_rc (network/auth failure); aborting"
        exit 1
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
