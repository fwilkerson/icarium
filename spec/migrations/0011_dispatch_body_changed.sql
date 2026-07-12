-- Reviewer tamper signal: did the task body change during the run (other
-- than newly-added ## Proof / ## Notes sections)? NULL = no review ran, so
-- no comparison was made. Durable audit record — reviewer logs rotate.
ALTER TABLE dispatches ADD COLUMN body_changed INTEGER
    CHECK (body_changed IS NULL OR body_changed IN (0, 1));
