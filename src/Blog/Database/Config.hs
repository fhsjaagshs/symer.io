{-# LANGUAGE OverloadedStrings #-}

-- to connect using psql (on blog.symer.io):
-- cd /deploy/ssl/
-- sudo psql "sslmode=verify-ca host=db.symer.io dbname=blog port=5432 sslrootcert=root.crt sslcert=server.crt sslkey=server.key user=symerdotio"

module Blog.Database.Config
(
  postgresConnStr,
  migrations,
  postsPerPage
) where

import Data.List

import System.IO.Unsafe
import System.Environment
import Data.Maybe

-- IMPORTANT: Do no remove entries from here once
-- the app has been run. It will mess up the DB
migrations :: [(String, String)]
migrations = [
  ("blog.sql", "./migrations/blog.sql"),
  ("array_funcs.sql", "./migrations/array_funcs.sql"),
  ("authorship.sql", "./migrations/authorship.sql"),
  ("drafts.sql", "./migrations/drafts.sql"),
  ("comments.sql", "./migrations/comments.sql"),
  ("auth.sql", "./migrations/auth.sql"),
  ("views.sql", "./migrations/views.sql")]

postgresConfigs :: FilePath -> FilePath -> FilePath -> String -> [(String, String)]
postgresConfigs _      _     _    "development" = [("user", "nathaniel"),
                                                   ("password", ""),
                                                   ("host", "localhost"),
                                                   ("port", "5432"),
                                                   ("dbname", "blog")]
postgresConfigs rootcrt ccrt ckey "production" = [("user", "symerdotio"),
                                                  ("host", "db.symer.io"),
                                                  ("port", "5432"),
                                                  ("dbname", "blog"),
                                                  ("sslmode", "verify-ca"), -- TODO configure verify-full
                                                  ("sslrootcert", rootcrt),
                                                  ("sslcert", ccrt),
                                                  ("sslkey", ckey)]
postgresConfigs _ _ _ _ = []

postgresConnStr :: FilePath -> FilePath -> FilePath -> String
postgresConnStr rootcrt ccrt ckey = intercalate " " $ map joinPair configs
  where
    joinPair :: (String, String) -> String
    joinPair (k, v) = k ++ "='" ++ v ++ "'"
    configs = postgresConfigs rootcrt ccrt ckey env
    env = fromMaybe "development" $ unsafePerformIO $ lookupEnv "ENV"

postsPerPage :: Int
postsPerPage = 10