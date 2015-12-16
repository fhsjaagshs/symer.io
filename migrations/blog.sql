CREATE TABLE users (
  id bigserial PRIMARY KEY NOT NULL,
  username text NOT NULL,
  display_name text NOT NULL DEFAULT 'New User'::text,
  password_hash text NOT NULL DEFAULT ''::text
);

CREATE TABLE posts (
  id bigserial PRIMARY KEY NOT NULL,
  title text NOT NULL,
  body text NOT NULL,
  timestamp timestamptz DEFAULT clock_timestamp(),
  tags text[] DEFAULT ARRAY[]::text[],
  author_id bigint REFERENCES users(id) NOT NULL,
  draft bool DEFAULT false
);

CREATE TABLE comments (
  id bigserial PRIMARY KEY NOT NULL,
  parentId bigint DEFAULT NULL,
  postId bigint REFERENCES posts(id) NOT NULL,
  email text NOT NULL,
  displayName text DEFAULT 'Commenter',
  timestamp timestamptz DEFAULT clock_timestamp(),
  body text NOT NULL
);

CREATE TABLE auth (
  token text PRIMARY KEY NOT NULL,
  user_id bigint REFERENCES users(id) NOT NULL
);

CREATE VIEW v_posts_all AS SELECT p.id,p.title,p.body,p.timestamp,p.tags,p.draft,u as user FROM posts p,users u WHERE u.id=p.author_id;
CREATE VIEW v_posts AS SELECT * FROM v_posts_all WHERE draft='f'::bool;
CREATE VIEW v_drafts AS SELECT * FROM v_posts_all WHERE draft='t'::bool;

CREATE INDEX i_posts_not_drafts ON posts(draft) WHERE draft IS FALSE;
CREATE INDEX i_posts_drafts ON posts(draft) WHERE draft IS TRUE;
CREATE INDEX i_posts_author_id ON posts(author_id);
CREATE INDEX i_posts_tags ON posts USING GIN (tags);

CREATE INDEX i_comments_post_id ON comments(postId);