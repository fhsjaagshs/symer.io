{-# LANGUAGE OverloadedStrings #-}

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
  ("comments.sql", "./migrations/comments.sql")]

postgresConfigs :: String -> FilePath -> FilePath -> FilePath -> String -> [(String, String)]
postgresConfigs _    _   _   _      "development" = [("user", "nathaniel"),
                                                     ("password", ""),
                                                     ("host", "localhost"),
                                                     ("port", "5432"),
                                                     ("dbname", "blog")]
postgresConfigs pass crt key rootcrt "production"  = [("user", "symerdotio"),
                                                      ("password", pass),
                                                      ("host", "db.symer.io"),
                                                      ("port", "5432"),
                                                      ("dbname", "blog"),
                                                      ("sslmode", "verify-full"),
                                                      ("sslcert", crt),
                                                      ("sslkey", key),
                                                      ("sslrootca", rootcrt)]
postgresConfigs _ _ _ _ _ = []

postgresConnStr :: String -> FilePath -> FilePath -> FilePath -> String
postgresConnStr pass crt key rootcrt = intercalate " " $ (map (\(k,v) -> k ++ "='" ++ v ++ "'") configs)
  where
    configs = (postgresConfigs pass crt key rootcrt env)
    env = fromMaybe "development" $ unsafePerformIO $ lookupEnv "ENV"

postsPerPage :: Int
postsPerPage = 10