CREATE OR REPLACE FUNCTION uniq(ary anyarray) RETURNS anyarray AS $$
DECLARE
	i integer;
	ret ary%TYPE := '{}';
BEGIN
	IF ary IS NULL THEN
		return NULL;
	END IF;

  FOR i IN ARRAY_LOWER(ary, 1)..ARRAY_UPPER(ary, 1) LOOP
    IF NOT ary[i] = any(ret) THEN
      ret = array_append(ret, ary[i]);
    END IF;
  END LOOP;

  RETURN ret;
END;
$$ LANGUAGE plpgsql;

CREATE TABLE users (
  id bigserial PRIMARY KEY NOT NULL,
  username text NOT NULL,
  display_name text NOT NULL DEFAULT 'New User'::text,
  password_hash text NOT NULL DEFAULT ''::text
);

CREATE TABLE blogposts (
  identifier BIGSERIAL PRIMARY KEY NOT NULL,
  title TEXT NOT NULL,
  bodyText TEXT NOT NULL,
  timestamp TIMESTAMPTZ DEFAULT clock_timestamp(),
  tags text[] DEFAULT ARRAY[]::text[],
  author_id bigint REFERENCES users(id) NOT NULL,
  is_draft bool DEFAULT false
);

CREATE TABLE comments (
  id BIGSERIAL PRIMARY KEY NOT NULL,
  parentId BIGINT DEFAULT NULL,
  postId BIGINT NOT NULL,
  email TEXT NOT NULL,
  displayName TEXT DEFAULT 'Commenter',
  timestamp TIMESTAMPTZ DEFAULT clock_timestamp(),
  body TEXT NOT NULL
);

CREATE TABLE auth (
  token text PRIMARY KEY NOT NULL,
  user_id bigint NOT NULL
);

CREATE VIEW v_posts_all AS SELECT b.identifier,b.title,b.bodytext,b.timestamp,b.tags,b.is_draft,u as user FROM blogposts b, users u WHERE u.id=b.author_id;
CREATE VIEW v_posts AS SELECT * FROM v_posts_all WHERE is_draft='f'::bool;
CREATE VIEW v_drafts AS SELECT * FROM v_posts_all WHERE is_draft='t'::bool;