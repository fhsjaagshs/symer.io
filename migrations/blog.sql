CREATE FUNCTION array_diff(anyarray,anyarray) RETURNS anyarray AS $BODY$
BEGIN
  SELECT array_agg(unnest) FROM unnest($1) WHERE unnest != ALL($2) INTO $1;
  return $1;
END;
$BODY$ LANGUAGE 'plpgsql' VOLATILE;

-- uniq(myarray)
-- returns unique copy of myarray
CREATE FUNCTION duniq(anyarray) RETURNS anyarray AS $BODY$
BEGIN
  SELECT ARRAY(SELECT DISTINCT unnest FROM unnest($1)) INTO $1;
  return $1;
END;
$BODY$ LANGUAGE 'plpgsql' VOLATILE;

-- uniq_append(array,elem)
-- appends elem onto array if it doesn't exist in array.
CREATE FUNCTION uniq_append(anyarray, anyelement) RETURNS anyarray AS $BODY$
BEGIN
	if ($2 = ANY($1)) THEN
		return $1;
	ELSE
		return array_append($1, $2);
	END IF;
END;
$BODY$ LANGUAGE 'plpgsql' VOLATILE;

-- uniq_prepend(array,elem)
-- prepends elem onto array if it doesn't exist in array.
CREATE FUNCTION uniq_prepend(anyarray, anyelement) RETURNS anyarray AS $BODY$
BEGIN
	if ($2 = ANY($1)) THEN
		return $1;
	ELSE
		return array_prepend($1, $2);
	END IF;
END;
$BODY$ LANGUAGE 'plpgsql' VOLATILE;

-- uniq_cat(one, two)
-- Concatenates one and an array consisting of values in two that don't exist in one
CREATE FUNCTION uniq_cat(anyarray, anyarray) RETURNS anyarray AS $BODY$
BEGIN
  return array_cat($1,array_diff($2,$1));
END;
$BODY$ LANGUAGE 'plpgsql' VOLATILE;

-- uniq_cat2(one, two)
-- Concatenates one and two and returns a copy without duplicates.
CREATE FUNCTION uniq_cat2(anyarray, anyarray) RETURNS anyarray AS $BODY$
BEGIN
  return uniq(array_cat($1,$2));
END;
$BODY$ LANGUAGE 'plpgsql' VOLATILE;

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