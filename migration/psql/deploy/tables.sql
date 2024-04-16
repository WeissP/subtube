-- Deploy subtube:tables to pg
-- requires: appschema

BEGIN;

CREATE OR REPLACE FUNCTION set_updated_at()
RETURNS trigger AS
$$
    begin
      NEW.updated_at = now();
      return NEW;
    end;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION trigger_updated_at(tablename regclass)
RETURNS void AS
$$
    begin
      execute format('CREATE TRIGGER set_updated_at
                     BEFORE UPDATE
                     ON %s
                     FOR EACH ROW
                     WHEN (OLD is distinct from NEW)
                     EXECUTE FUNCTION set_updated_at();', tablename);
    end;
$$ LANGUAGE plpgsql;

CREATE TABLE subtube.tag (
    gid uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    tag_name bytea UNIQUE NOT NULL,
    introduction bytea,
    updated_at timestamptz NOT NULL DEFAULT now()
);
SELECT trigger_updated_at('subtube.tag');

CREATE TABLE subtube.ytb_channel (
    gid uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    ytb_channel_id text UNIQUE NOT NULL,
    channel_name text NOT NULL,
    description text NOT NULL,
    sub_count int4 NOT NULL,
    introduction bytea,
    thumbnail text NOT NULL,
    info_cached_at int8 NOT NULL,  -- unix timestamp
    videos_cached_at int8 NOT NULL,  -- unix timestamp
    updated_at timestamptz NOT NULL DEFAULT now()
);
SELECT trigger_updated_at('subtube.ytb_channel');

CREATE TABLE subtube.media (
    gid uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    title bytea NOT NULL,
    duration int8 NOT NULL,
    thumbnail text NOT NULL,
    published_at int8 NOT NULL,  -- unix timestamp
    cached_at int8,
    introduction bytea,
    updated_at timestamptz NOT NULL DEFAULT now()
);
SELECT trigger_updated_at('subtube.media');

CREATE TABLE subtube.ytb_video (
    ytb_video_id text PRIMARY KEY NOT NULL,
    gid uuid  REFERENCES subtube.media (gid),
    ytb_channel_id text NOT NULL,
    description bytea NOT NULL,
    updated_at timestamptz NOT NULL DEFAULT now()
);
SELECT trigger_updated_at('subtube.ytb_video');

CREATE TABLE subtube.tag_member (
    gid uuid REFERENCES subtube.tag (gid),
    member_gid uuid NOT NULL,
    tagged_by text NOT NULL,
    updated_at timestamptz NOT NULL DEFAULT now(),
    PRIMARY KEY (gid,member_gid,tagged_by)
);
SELECT trigger_updated_at('subtube.tag_member');

COMMIT;
