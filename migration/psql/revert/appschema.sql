-- Revert subtube:appschema from pg

BEGIN;

DROP SCHEMA subtube;

COMMIT;
