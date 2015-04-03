CREATE TABLE comments (
  id BIGSERIAL PRIMARY KEY NOT NULL,
  parentId BIGINT DEFAULT NULL,
  postId BIGINT NOT NULL,
  email TEXT NOT NULL,
  commentDisplayName TEXT DEFAULT 'Commenter',
  timestamp TIMESTAMPTZ DEFAULT clock_timestamp(),
  body TEXT NOT NULL
);