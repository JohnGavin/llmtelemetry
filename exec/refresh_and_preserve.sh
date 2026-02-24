#!/bin/bash
# Enhanced refresh script with proper branch management and error handling
# Preserves complete history and handles git operations correctly
# Runs via launchd every 12 hours

set -e

# Configuration
LLM_REPO="/Users/johngavin/docs_gh/proj/data/llm/telemetry"
LOG_FILE="$LLM_REPO/inst/logs/refresh_preserve.log"
ERROR_LOG="$LLM_REPO/inst/logs/refresh_preserve_error.log"
LOCK_FILE="/tmp/refresh_preserve.lock"
MAIN_BRANCH="main"

# Source Nix if available
if [ -e "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh" ]; then
    . "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
fi
if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi

# Add common paths
export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:$PATH"

# Ensure only one instance runs
if [ -f "$LOCK_FILE" ]; then
    pid=$(cat "$LOCK_FILE")
    if kill -0 "$pid" 2>/dev/null; then
        echo "$(date): Another instance running (PID $pid), exiting" >> "$LOG_FILE"
        exit 0
    fi
fi
echo $$ > "$LOCK_FILE"
trap "rm -f $LOCK_FILE" EXIT

# Create directories if needed
mkdir -p "$(dirname "$LOG_FILE")"
mkdir -p "$LLM_REPO/inst/extdata"
mkdir -p "$LLM_REPO/inst/extdata/archive"

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S'): $1" | tee -a "$LOG_FILE"
}

error_log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S'): ERROR: $1" | tee -a "$ERROR_LOG" >&2
}

# Log rotation function
rotate_logs() {
    local log_file=$1
    local max_size=5242880  # 5MB in bytes
    local keep_count=3      # Keep 3 old versions

    if [ -f "$log_file" ]; then
        local size=$(stat -f%z "$log_file" 2>/dev/null || stat -c%s "$log_file" 2>/dev/null || echo 0)

        if [ "$size" -gt "$max_size" ]; then
            # Rotate existing logs
            for i in $(seq $((keep_count-1)) -1 1); do
                if [ -f "${log_file}.${i}" ]; then
                    mv "${log_file}.${i}" "${log_file}.$((i+1))"
                fi
            done

            # Move current log to .1
            mv "$log_file" "${log_file}.1"
            touch "$log_file"
            log "Log rotated (was $((size/1024/1024))MB)"
        fi
    fi
}

# Rotate logs at start if needed
rotate_logs "$LOG_FILE"
rotate_logs "$ERROR_LOG"

log "===== Starting history-preserving refresh ====="

cd "$LLM_REPO"

# CRITICAL: Save current branch and ensure we're on main
ORIGINAL_BRANCH=$(git rev-parse --abbrev-ref HEAD)
log "Current branch: $ORIGINAL_BRANCH"

# Stash any uncommitted changes
if ! git diff --quiet || ! git diff --staged --quiet; then
    log "Stashing uncommitted changes..."
    git stash push -m "Auto-stash before ccusage refresh $(date +%Y%m%d_%H%M%S)"
fi

# Switch to main branch
if [ "$ORIGINAL_BRANCH" != "$MAIN_BRANCH" ]; then
    log "Switching to $MAIN_BRANCH branch..."
    git checkout "$MAIN_BRANCH" 2>&1 | tee -a "$LOG_FILE"
fi

# Pull latest changes
log "Pulling latest changes from origin..."
if ! git pull origin "$MAIN_BRANCH" 2>&1 | tee -a "$LOG_FILE"; then
    error_log "Failed to pull from origin"
    # Continue anyway - we can still update locally
fi

# 1. Archive current JSON files (in case something goes wrong)
ARCHIVE_DIR="$LLM_REPO/inst/extdata/archive/$(date +%Y%m%d_%H%M%S)"
mkdir -p "$ARCHIVE_DIR"
cp inst/extdata/*.json "$ARCHIVE_DIR/" 2>/dev/null || true
log "Archived existing JSON files to $ARCHIVE_DIR"

# ===== TIER 1: Direct JSON refresh via npx (no R/nix-shell needed) =====
# Pin ccusage version to avoid npm protocol breakage (v18.0.6 uses unsupported runtime: URLs)
CCUSAGE_VERSION="18.0.5"

log "--- Tier 1: Fetching fresh JSON data via npx ccusage@${CCUSAGE_VERSION} ---"

TIER1_OK=true

# Fetch daily data
log "Fetching daily data..."
if npx "ccusage@${CCUSAGE_VERSION}" daily --json --instances > "$LLM_REPO/inst/extdata/ccusage_daily_all.json.tmp" 2>/dev/null; then
    if [ -s "$LLM_REPO/inst/extdata/ccusage_daily_all.json.tmp" ] && \
       python3 -c "import json; json.load(open('$LLM_REPO/inst/extdata/ccusage_daily_all.json.tmp'))" 2>/dev/null; then
        mv "$LLM_REPO/inst/extdata/ccusage_daily_all.json.tmp" "$LLM_REPO/inst/extdata/ccusage_daily_all.json"
        log "✓ Daily data fetched"
    else
        error_log "Daily JSON invalid or empty, keeping previous version"
        rm -f "$LLM_REPO/inst/extdata/ccusage_daily_all.json.tmp"
        TIER1_OK=false
    fi
else
    error_log "npx ccusage daily failed"
    rm -f "$LLM_REPO/inst/extdata/ccusage_daily_all.json.tmp"
    TIER1_OK=false
fi

# Fetch session data
log "Fetching session data..."
if npx "ccusage@${CCUSAGE_VERSION}" session --json --instances > "$LLM_REPO/inst/extdata/ccusage_session_all.json.tmp" 2>/dev/null; then
    if [ -s "$LLM_REPO/inst/extdata/ccusage_session_all.json.tmp" ] && \
       python3 -c "import json; json.load(open('$LLM_REPO/inst/extdata/ccusage_session_all.json.tmp'))" 2>/dev/null; then
        mv "$LLM_REPO/inst/extdata/ccusage_session_all.json.tmp" "$LLM_REPO/inst/extdata/ccusage_session_all.json"
        log "✓ Session data fetched"
    else
        error_log "Session JSON invalid, keeping previous version"
        rm -f "$LLM_REPO/inst/extdata/ccusage_session_all.json.tmp"
    fi
else
    error_log "npx ccusage session failed"
    rm -f "$LLM_REPO/inst/extdata/ccusage_session_all.json.tmp"
fi

# Fetch blocks data
log "Fetching blocks data..."
if npx "ccusage@${CCUSAGE_VERSION}" blocks --json --instances --breakdown > "$LLM_REPO/inst/extdata/ccusage_blocks_all.json.tmp" 2>/dev/null; then
    if [ -s "$LLM_REPO/inst/extdata/ccusage_blocks_all.json.tmp" ] && \
       python3 -c "import json; json.load(open('$LLM_REPO/inst/extdata/ccusage_blocks_all.json.tmp'))" 2>/dev/null; then
        mv "$LLM_REPO/inst/extdata/ccusage_blocks_all.json.tmp" "$LLM_REPO/inst/extdata/ccusage_blocks_all.json"
        log "✓ Blocks data fetched"
    else
        error_log "Blocks JSON invalid, keeping previous version"
        rm -f "$LLM_REPO/inst/extdata/ccusage_blocks_all.json.tmp"
    fi
else
    error_log "npx ccusage blocks failed"
    rm -f "$LLM_REPO/inst/extdata/ccusage_blocks_all.json.tmp"
fi

if [ "$TIER1_OK" = true ]; then
    log "✓ Tier 1 complete: all JSON files refreshed"
else
    error_log "Tier 1 partial: some JSON files could not be refreshed"
fi

# ===== TIER 2: DuckDB history preservation (optional, requires R) =====
log "--- Tier 2: DuckDB history preservation (optional) ---"

if command -v Rscript &>/dev/null; then
    log "Rscript found at $(which Rscript), attempting DuckDB preservation..."

    cat > /tmp/refresh_preserve.R << 'REOF'
library(DBI)
library(duckdb)
library(jsonlite)
library(dplyr)

`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

DB_PATH <- "inst/extdata/llm_usage_history.duckdb"
con <- dbConnect(duckdb(), dbdir = DB_PATH)

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS daily_usage (
    date DATE NOT NULL, project VARCHAR NOT NULL,
    input_tokens BIGINT, output_tokens BIGINT,
    cache_creation_tokens BIGINT, cache_read_tokens BIGINT,
    total_tokens BIGINT, total_cost DOUBLE,
    models_used VARCHAR, data_source VARCHAR NOT NULL,
    collected_at TIMESTAMP NOT NULL,
    PRIMARY KEY (date, project, data_source)
  )
")

daily_fresh <- tryCatch(
  fromJSON("inst/extdata/ccusage_daily_all.json"),
  error = function(e) NULL
)

if (!is.null(daily_fresh$projects)) {
  collected_at <- Sys.time()
  records <- 0
  for (pname in names(daily_fresh$projects)) {
    pdata <- daily_fresh$projects[[pname]]
    if (is.data.frame(pdata) && nrow(pdata) > 0) {
      for (i in seq_len(nrow(pdata))) {
        row <- pdata[i, ]
        dbExecute(con, sprintf(
          "INSERT OR REPLACE INTO daily_usage VALUES ('%s','%s',%s,%s,%s,%s,%s,%f,'[]','ccusage','%s')",
          row$date, pname,
          as.integer(row$inputTokens %||% 0), as.integer(row$outputTokens %||% 0),
          as.integer(row$cacheCreationTokens %||% 0), as.integer(row$cacheReadTokens %||% 0),
          as.integer(row$totalTokens %||% 0), as.numeric(row$totalCost %||% 0),
          format(collected_at, "%Y-%m-%d %H:%M:%S")
        ))
        records <- records + 1
      }
    }
  }
  message(sprintf("DuckDB: added/updated %d records", records))
}

stats <- dbGetQuery(con, "
  SELECT COUNT(DISTINCT date) as days, MIN(date) as earliest, MAX(date) as latest,
         SUM(total_cost) as cost
  FROM (SELECT date, MAX(total_cost) as total_cost FROM daily_usage GROUP BY date, project)
")
message(sprintf("DuckDB: %d days, %s to %s, $%.2f total",
                stats$days, stats$earliest, stats$latest, stats$cost))

dbDisconnect(con)
REOF

    if timeout 120 Rscript /tmp/refresh_preserve.R >> "$LOG_FILE" 2>&1; then
        log "✓ Tier 2 complete: DuckDB history preserved"
    else
        log "⚠ Tier 2 failed (non-blocking): DuckDB preservation skipped"
    fi
else
    log "⚠ Rscript not in PATH, skipping Tier 2 (DuckDB preservation)"
fi

# 3. Capture cmonitor data (try from PATH directly, no nix-shell)
log "Capturing cmonitor data..."
if command -v cmonitor &>/dev/null; then
    if timeout 60 cmonitor --view daily > "$LLM_REPO/inst/extdata/cmonitor_daily.txt" 2>/dev/null; then
        log "✓ cmonitor daily data captured"
        timeout 60 cmonitor --view monthly > "$LLM_REPO/inst/extdata/cmonitor_monthly.txt" 2>/dev/null || true
        CMONITOR_COST=$(grep "Total Cost:" "$LLM_REPO/inst/extdata/cmonitor_daily.txt" | sed 's/.*\$\([0-9.,]*\).*/\1/' | head -1)
        if [ -n "$CMONITOR_COST" ]; then
            log "cmonitor reports total cost: \$$CMONITOR_COST"
        fi
    else
        log "⚠ cmonitor command failed"
        touch "$LLM_REPO/inst/extdata/cmonitor_daily.txt"
    fi
else
    log "⚠ cmonitor not in PATH, skipping"
    touch "$LLM_REPO/inst/extdata/cmonitor_daily.txt"
fi

# 4. ALWAYS stage and commit changes (don't check if files changed)
log "Staging changes..."
# Split git add to handle missing file types gracefully
git add inst/extdata/*.json 2>&1 | tee -a "$LOG_FILE" || true
git add inst/extdata/*.txt 2>&1 | tee -a "$LOG_FILE" || true
git add inst/extdata/*.duckdb* 2>&1 | tee -a "$LOG_FILE" || true
git add inst/extdata/gemini/*.duckdb* 2>&1 | tee -a "$LOG_FILE" || true

# Check if there are staged changes
if git diff --staged --quiet; then
    log "No changes to commit (files unchanged)"
else
    log "Committing changes..."

    # Get summary from latest data
    if [ -f "inst/extdata/ccusage_daily_all.json" ]; then
        TOTAL_COST=$(grep '"totalCost"' inst/extdata/ccusage_daily_all.json | head -1 | sed 's/.*: \(.*\),/\1/')
    fi

    commit_msg="chore: Auto-refresh ccusage cache $(date '+%Y-%m-%d %H:%M')

Updated usage data with complete history preservation.
Total cost: \$$TOTAL_COST

🤖 Automated via launchd (12-hourly)"

    if git commit -m "$commit_msg" 2>&1 | tee -a "$LOG_FILE"; then
        log "✓ Commit successful"

        # Push to remote
        log "Pushing to remote..."
        if git push origin "$MAIN_BRANCH" 2>&1 | tee -a "$LOG_FILE"; then
            log "✓ Push successful"
        else
            error_log "Push failed - may need manual intervention"
            # Don't exit with error - local commit still succeeded
        fi
    else
        error_log "Commit failed"
    fi
fi

# 5. Return to original branch if different
if [ "$ORIGINAL_BRANCH" != "$MAIN_BRANCH" ] && [ "$ORIGINAL_BRANCH" != "HEAD" ]; then
    log "Returning to original branch: $ORIGINAL_BRANCH"
    git checkout "$ORIGINAL_BRANCH" 2>&1 | tee -a "$LOG_FILE"

    # Restore stashed changes if any
    if git stash list | grep -q "Auto-stash before ccusage refresh"; then
        log "Restoring stashed changes..."
        git stash pop 2>&1 | tee -a "$LOG_FILE" || true
    fi
fi

# 6. Clean up old archive directories (keep last 10)
log "Cleaning old archives..."
ls -t "$LLM_REPO/inst/extdata/archive" 2>/dev/null | tail -n +11 | while read dir; do
    rm -rf "$LLM_REPO/inst/extdata/archive/$dir"
    log "Removed old archive: $dir"
done

log "===== Refresh complete ====="

# Print final statistics
if [ -f "$LLM_REPO/inst/extdata/llm_usage_history.duckdb" ]; then
    DB_SIZE=$(du -h "$LLM_REPO/inst/extdata/llm_usage_history.duckdb" | cut -f1)
    log "DuckDB size: $DB_SIZE"
fi

# Exit with success
exit 0