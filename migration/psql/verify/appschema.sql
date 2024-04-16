-- Verify subtube:appschema on pg

BEGIN;

SELECT pg_catalog.has_schema_privilege('subtube', 'usage');

ROLLBACK;
