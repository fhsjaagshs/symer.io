{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Migrations
import System.Environment
import Config

-- postgresql://[[USERNAME@PASSWORD]HOSTNAME[:PORT]]/[DBNAME]

main :: IO ()
main = do
  setEnv "DATABASE_URL" Config.postgresURL
  defaultMain up down
  
up = migrate $ do
    create_table "blogposts" [column "id" "bigserial PRIMARY KEY NOT NULL", column "title" "text NOT NULL DEFAULT 'New Blog Post'::text", column "body" "text NOT NULL DEFAULT ''::text", column "timestamp" "timestamptz DEFAULT clock_timestamp()", column "tags" "text[] DEFAULT ARRAY[]::text[]"]
    create_table "users" [column "id" "bigserial PRIMARY KEY NOT NULL", column "username" "text NOT NULL DEFAULT ''::text", column "display_name" "text NOT NULL DEFAULT ''::text", column "passwordHash" "text NOT NULL DEFAULT ''::text"]
 
down = migrate $ do
  drop_table "blogposts"
  drop_table "users"