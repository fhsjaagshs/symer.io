-- BlogPost
CREATE TABLE blogposts (
  identifier BIGSERIAL PRIMARY KEY NOT NULL,
  title TEXT NOT NULL,
  bodyText TEXT NOT NULL,
  timestamp TIMESTAMPTZ DEFAULT clock_timestamp(),
  tags text[] DEFAULT ARRAY[]::text[]
);

-- PostTag
CREATE TABLE tags (
  postIdentifier BIGINT NOT NULL,
  tag TEXT
);
