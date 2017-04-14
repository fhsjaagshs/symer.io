CREATE TABLE IF NOT EXISTS user_t (
  UserID BIGSERIAL PRIMARY KEY,
  UserName TEXT NOT NULL,
  UserPasswordHash TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS user_t_username ON user_t(UserName);

CREATE TABLE IF NOT EXISTS post_t (
  PostID BIGSERIAL PRIMARY KEY,
  PostTitle TEXT NOT NULL,
  PostBody TEXT NOT NULL,
  PostCreatedAt TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  PostIsDraft BOOL DEFAULT TRUE,
  PostAuthorID BIGINT REFERENCES user_t(UserID) NOT NULL
);

CREATE INDEX IF NOT EXISTS posts_t_not_drafts ON post_t(PostIsDraft) WHERE PostIsDraft IS FALSE;
CREATE INDEX IF NOT EXISTS posts_t_drafts     ON post_t(PostIsDraft) WHERE PostIsDraft IS TRUE;

CREATE TABLE IF NOT EXISTS tag_t (
  TagID BIGSERIAL PRIMARY KEY,
  TagValue TEXT NOT NULL UNIQUE
);

CREATE INDEX IF NOT EXISTS tag_t_tagvalue ON tag_t USING spgist (TagValue);

CREATE TABLE IF NOT EXISTS post_tag_t (
  PostTagID BIGSERIAL PRIMARY KEY,
  PostID BIGSERIAL NOT NULL REFERENCES post_t(PostID) ON DELETE CASCADE,
  TagID BIGSERIAL NOT NULL REFERENCES tag_t(TagID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS comment_t (
  CommentID BIGSERIAL PRIMARY KEY,
  CommentParentID BIGINT REFERENCES comment_t(CommentID) ON DELETE CASCADE,
  PostID BIGINT NOT NULL REFERENCES post_t(PostID) ON DELETE CASCADE,
  CommentBody TEXT NOT NULL,
  CommentCreatedAt TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp()
);

CREATE TABLE IF NOT EXISTS session_t (
  SessionID BIGSERIAL PRIMARY KEY,
  UserID BIGINT NOT NULL REFERENCES user_t(UserID),
  SessionCreatedAt TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp()
);

CREATE TABLE IF NOT EXISTS visit_t (
  VisitID BIGSERIAL PRIMARY KEY,
  VisitDate TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  VisitLanguage TEXT, -- Accept-Language
  VisitForwarded TEXT, -- Forwarded
  VisitReferrer TEXT, -- Referrer
  VisitUserAgent TEXT -- User-Agent
  -- TODO: IP address?
);

CREATE INDEX IF NOT EXISTS posts_id         ON post_t(PostID);
CREATE INDEX IF NOT EXISTS tags_id          ON tag_t(TagID);
CREATE INDEX IF NOT EXISTS post_tags_tagid  ON post_tag_t(TagID);
CREATE INDEX IF NOT EXISTS post_tags_postid ON post_tag_t(PostID);

CREATE OR REPLACE VIEW v_weighted_tag AS
  SELECT t.TagID,t.TagValue,count(pt.PostID) as TagCount
  FROM tag_t t
  INNER JOIN post_tag_t pt ON t.TagID=pt.TagID
  GROUP BY t.TagID,t.TagValue
  HAVING count(pt.PostID) > 0;
  
CREATE OR REPLACE VIEW v_posts_all AS
  SELECT p.PostID,p.PostTitle,p.PostBody,p.PostCreatedAt,
         coalesce((
           SELECT array_agg(t.tagvalue)
           FROM tag_t t
           WHERE EXISTS (
             SELECT *
             FROM post_tag_t pt
             where pt.TagID=t.TagID AND pt.postid=p.PostId
           )
         ), '{}') as tags,
         p.PostIsDraft,u.UserID,u.UserName
  FROM post_t p
  INNER JOIN user_t u ON p.PostAuthorID=u.UserID
  ORDER BY p.PostCreatedAt DESC;

CREATE OR REPLACE VIEW v_posts AS SELECT * FROM v_posts_all WHERE PostIsDraft IS FALSE;
CREATE OR REPLACE VIEW v_drafts AS SELECT * FROM v_posts_all WHERE PostIsDraft IS TRUE;

-- Creates or updates a post. Behavior depends on _p_postid:
-- 1. if _p_postid is NULL, then it creates a post.
-- 2. otherwise, it updates a post with id _p_postid.
-- Only non-NULL values are updated if #2 applies.
-- Default values will replace NULLs if #1 applies.
CREATE OR REPLACE FUNCTION upsert_post(_p_postid BIGINT, _p_title TEXT, _p_body TEXT, _p_isdraft BOOL, _p_tags TEXT[], _p_author_id BIGINT) RETURNS BIGINT AS $$
DECLARE
  _postid BIGINT; -- this isn't == to _p_postid. It is set after the post is updated or inserted.
  _tag TEXT; -- iteration variable, see below
  _tagid BIGINT;
BEGIN
  IF _p_author_id IS NULL THEN
    RAISE WARNING 'unauthenticated upsert attempted on %', _p_postid;
    RETURN NULL;
  END IF;

  IF _p_postid IS NULL THEN
    -- Insert a new post into the database
    INSERT INTO post_t (PostTitle, PostBody, PostIsDraft, PostAuthorID)
    VALUES (coalesce(_p_title, ''), coalesce(_p_body, ''), coalesce(_p_isdraft, TRUE), _p_author_id)
    RETURNING PostID INTO _postid;
  ELSE
    _postid := _p_postid; -- we know the post id
    -- Update the existing post in the database
    UPDATE post_t
    SET PostTitle=coalesce(_p_title, PostTitle),
        PostBody=coalesce(_p_body, PostBody),
        PostIsDraft=coalesce(_p_isdraft, PostIsDraft)
    WHERE PostAuthorID=_p_author_id AND PostID=_postid;
  END IF;

  -- If there are tags passed in
  IF _p_tags IS NOT NULL THEN
    -- Then add each tag to the post
    FOREACH _tag IN ARRAY _p_tags LOOP
      _tagid := NULL; -- NULL out _tagid
      
      -- 1. Find the tag entry in the database
      SELECT t.TagID FROM tag_t t WHERE t.TagValue=_tag INTO _tagid;
  
      IF _tagid IS NULL THEN
        -- 2a. Create a tag_t and a post_tag_t.
        INSERT INTO tag_t (TagValue) VALUES (_tag) RETURNING TagID INTO _tagid;
        INSERT INTO post_tag_t (PostId, TagId) VALUES (_postid, _tagid);
      ELSIF NOT EXISTS (SELECT * FROM post_tag_t pt WHERE pt.PostID=_postid AND pt.TagID=_tagid) THEN
        -- 2b. Create a post_tag_t if it doesn't already exist.
        INSERT INTO post_tag_t (PostId, TagId) VALUES (_postid, _tagid);
      END IF;
    END LOOP;
  END IF;

  return _postid;
END;
$$ LANGUAGE plpgsql;
