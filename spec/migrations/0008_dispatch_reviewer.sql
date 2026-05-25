ALTER TABLE dispatches ADD COLUMN review_verdict TEXT
    CHECK (review_verdict IS NULL OR review_verdict IN ('pass', 'warn', 'fail'));
ALTER TABLE dispatches ADD COLUMN reviewer_log_path TEXT;
