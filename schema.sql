-- Expert Elicitation Platform Schema for Supabase (PostgreSQL)
-- Run this in the Supabase SQL Editor (https://supabase.com/dashboard/project/_/sql)

-- 1. Organizations
CREATE TABLE IF NOT EXISTS organizations (
    id TEXT PRIMARY KEY,
    data JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_organizations_data ON organizations USING GIN (data);

-- 2. Users
CREATE TABLE IF NOT EXISTS users (
    id TEXT PRIMARY KEY,
    data JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_users_data ON users USING GIN (data);

-- 3. People (Invited Experts and Contacts)
CREATE TABLE IF NOT EXISTS people (
    id TEXT PRIMARY KEY,
    data JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_people_data ON people USING GIN (data);

-- 4. Case Studies
CREATE TABLE IF NOT EXISTS studies (
    id TEXT PRIMARY KEY,
    data JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_studies_data ON studies USING GIN (data);

-- 5. Quantities of Interest / Questions
CREATE TABLE IF NOT EXISTS questions (
    id TEXT PRIMARY KEY,
    data JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_questions_data ON questions USING GIN (data);

-- 6. Expert Judgments
CREATE TABLE IF NOT EXISTS judgments (
    id TEXT PRIMARY KEY,
    data JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_judgments_data ON judgments USING GIN (data);

-- 7. Study Access
CREATE TABLE IF NOT EXISTS study_access (
    id TEXT PRIMARY KEY,
    data JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_study_access_data ON study_access USING GIN (data);

-- 8. Invite Tokens
CREATE TABLE IF NOT EXISTS invite_tokens (
    id TEXT PRIMARY KEY,
    data JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_invite_tokens_data ON invite_tokens USING GIN (data);

-- 9. Onboarding Records
CREATE TABLE IF NOT EXISTS onboarding (
    id TEXT PRIMARY KEY,
    data JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_onboarding_data ON onboarding USING GIN (data);

-- 10. Elicitation Bounds
CREATE TABLE IF NOT EXISTS elicitation_bounds (
    id TEXT PRIMARY KEY,
    data JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_elicitation_bounds_data ON elicitation_bounds USING GIN (data);

-- 11. Peer Comments
CREATE TABLE IF NOT EXISTS peer_comments (
    id TEXT PRIMARY KEY,
    data JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_peer_comments_data ON peer_comments USING GIN (data);

-- 12. Model Aggregations
CREATE TABLE IF NOT EXISTS aggregations (
    id TEXT PRIMARY KEY,
    data JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_aggregations_data ON aggregations USING GIN (data);

-- Enable Row Level Security (RLS) across all tables to satisfy Supabase security linter
-- and block unauthorized external HTTP/PostgREST access.
-- The Shiny app connects as the database owner (`postgres`), which bypasses RLS by default.
ALTER TABLE organizations ENABLE ROW LEVEL SECURITY;
ALTER TABLE users ENABLE ROW LEVEL SECURITY;
ALTER TABLE people ENABLE ROW LEVEL SECURITY;
ALTER TABLE studies ENABLE ROW LEVEL SECURITY;
ALTER TABLE questions ENABLE ROW LEVEL SECURITY;
ALTER TABLE judgments ENABLE ROW LEVEL SECURITY;
ALTER TABLE study_access ENABLE ROW LEVEL SECURITY;
ALTER TABLE invite_tokens ENABLE ROW LEVEL SECURITY;
ALTER TABLE onboarding ENABLE ROW LEVEL SECURITY;
ALTER TABLE elicitation_bounds ENABLE ROW LEVEL SECURITY;
ALTER TABLE peer_comments ENABLE ROW LEVEL SECURITY;
ALTER TABLE aggregations ENABLE ROW LEVEL SECURITY;
