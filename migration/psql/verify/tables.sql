-- Verify subtube:tables on pg

BEGIN;

SELECT gid,tag_name,introduction,updated_at
  FROM subtube.tag
 WHERE FALSE;

ROLLBACK;

