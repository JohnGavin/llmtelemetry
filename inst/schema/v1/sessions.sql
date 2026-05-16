-- Phase 1A schema v1 — sessions table
-- PIT: every row has valid_from; never UPDATE/DELETE.
-- Corrections go to sessions_amendments table (Phase 1B+).

CREATE TABLE IF NOT EXISTS sessions (
    session_id        VARCHAR PRIMARY KEY,
    project           VARCHAR,           -- raw project label (from source)
    canonical_project VARCHAR,           -- canonicalised via canonicalize_project() helper
    started_at        TIMESTAMP,
    ended_at          TIMESTAMP,
    duration_min      DOUBLE,
    agent             VARCHAR,           -- e.g. "claude-sonnet-4-6"; NULL if unknown
    source            VARCHAR,           -- e.g. "ccusage", "unified_duckdb"; required
    working_dir       VARCHAR,           -- absolute path; NULL if unknown
    valid_from        TIMESTAMP NOT NULL -- when this row was first written
);

CREATE INDEX IF NOT EXISTS idx_sessions_project ON sessions(canonical_project);
CREATE INDEX IF NOT EXISTS idx_sessions_started ON sessions(started_at);
