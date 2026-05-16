-- Phase 1C schema v1 — git_commits table
-- PIT: every row has valid_from; never UPDATE/DELETE.

CREATE TABLE IF NOT EXISTS git_commits (
    commit_pk         VARCHAR PRIMARY KEY,   -- project|hash composite
    project           VARCHAR,
    canonical_project VARCHAR,
    hash              VARCHAR NOT NULL,
    date              DATE NOT NULL,
    message           VARCHAR,
    lines_added       INTEGER,
    lines_deleted     INTEGER,
    files_changed     INTEGER,
    lines_changed     INTEGER,
    valid_from        TIMESTAMP NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_commits_project ON git_commits(canonical_project);
CREATE INDEX IF NOT EXISTS idx_commits_date ON git_commits(date);
CREATE INDEX IF NOT EXISTS idx_commits_hash ON git_commits(hash);
