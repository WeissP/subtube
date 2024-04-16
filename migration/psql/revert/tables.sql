-- Revert subtube:tables from pg

BEGIN;

DROP TABLE subtube.ytb_channel;
DROP TABLE subtube.ytb_video;
DROP TABLE subtube.media;
DROP TABLE subtube.tag_member;
DROP TABLE subtube.tag;

COMMIT;
