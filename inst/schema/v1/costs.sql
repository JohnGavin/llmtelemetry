-- Phase 1B schema v1 — costs table
-- PIT: every row has valid_from; never UPDATE/DELETE.

CREATE TABLE IF NOT EXISTS costs (
    cost_id           VARCHAR PRIMARY KEY,   -- synthetic: canonical_project|date|source
    project           VARCHAR,
    canonical_project VARCHAR,
    date              DATE NOT NULL,
    source            VARCHAR,               -- "ccusage" | "gemini" | "estimated"
    daily_cost_usd    DOUBLE,
    n_sessions        INTEGER,               -- if available; else NULL
    duration_min      DOUBLE,               -- if available; else NULL
    valid_from        TIMESTAMP NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_costs_project ON costs(canonical_project);
CREATE INDEX IF NOT EXISTS idx_costs_date ON costs(date);
