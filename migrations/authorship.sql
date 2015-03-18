UPDATE users SET id=1 WHERE id = (SELECT MIN(id) FROM users); -- set first user to id 1
ALTER TABLE blogposts ADD COLUMN author_id bigint REFERENCES users(id) DEFAULT 1; -- add author_id column to blogposts
UPDATE blogposts SET author_id=1; -- set the author id to the admin (id 1) user