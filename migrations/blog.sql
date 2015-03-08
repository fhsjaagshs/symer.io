-- BlogPost
CREATE TABLE blogposts (
  identifier BIGSERIAL PRIMARY KEY NOT NULL,
  title TEXT NOT NULL,
  bodyText TEXT NOT NULL,
  timestamp TIMESTAMPTZ DEFAULT clock_timestamp(),
  tags text[] DEFAULT ARRAY[]::text[]
);

CREATE TABLE users (
  id bigserial PRIMARY KEY NOT NULL,
  username text NOT NULL,
  display_name text NOT NULL DEFAULT 'New User'::text,
  password_hash text NOT NULL DEFAULT ''::text
);

INSERT INTO users (username, display_name, password_hash) VALUES ('foobar','Foo Bar', '$2a$10$RupeVQID0sn1IRyaY6IRruCWqbyUt6/PGHYhRbhKFFHtg.4GlzA1m');

 -- foobar