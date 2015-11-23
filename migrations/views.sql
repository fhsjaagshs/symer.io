-- POSTS
CREATE VIEW v_posts_all AS SELECT b.identifier,b.title,b.bodytext,b.timestamp,b.tags,b.is_draft,u as user FROM blogposts b, users u WHERE u.id=b.author_id;
CREATE VIEW v_posts AS SELECT * FROM v_posts_all WHERE is_draft='f'::bool;
CREATE VIEW v_drafts AS SELECT * FROM v_posts_all WHERE is_draft='t'::bool;
-- @getPostsByTag@ SELECT * FROM v_posts v WHERE ?=any(v.tags) ORDER BY identifier DESC OFFSET ? LIMIT ?
-- @getPosts@ SELECT * FROM v_posts ORDER BY identifier DESC OFFSET ? LIMIT ?
-- @getPost@ SELECT * FROM v_posts_all WHERE identifier=? LIMIT 1
-- @getDrafts@ SELECT * FROM v_drafts ORDER BY identifier DESC OFFSET ? LIMIT ?