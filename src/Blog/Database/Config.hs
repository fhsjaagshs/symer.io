{-# LANGUAGE OverloadedStrings #-}

module Blog.Database.Config
(
  postgresConfig,
  postgresURL,
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

postgresConfigs :: String -> [(String, String)]
postgresConfigs "development" = [
                    ("user", "nathaniel"),
                    ("password", ""),
                    ("host", "localhost"),
                    ("port", "5432"),
                    ("dbname", "blog")
                    ]
postgresConfigs "production" = [
                    ("user", "symerdotio"),
                    ("password", fromJust $ unsafePerformIO $ lookupEnv "PGPASSWORD"),
                    ("host", "db.symer.io"),
                    ("port", "5432"),
                    ("dbname", "blog"),
                    ("sslmode", "require")
                    ]
postgresConfigs _ = []
                    
postgresConfig :: [(String, String)]
postgresConfig = postgresConfigs $ fromJust $ unsafePerformIO $ lookupEnv "ENV"

configFor :: String -> String
configFor k = fromMaybe "" $ lookup "port" postgresConfig

postgresURL :: String
postgresURL = "postgresql://"
              ++ (configFor "user")
              ++ (if (configFor "password") == "" then "" else ":" ++ (configFor "password"))
              ++ "@"
              ++ (configFor "host")
              ++ ":"
              ++ (configFor "port")
              ++ "/"
              ++ (configFor "dbname")

postgresConnStr :: String
postgresConnStr = intercalate " " $ (map (\(k,_) -> k ++ "='" ++ (configFor k) ++ "'") postgresConfig)

postsPerPage :: Int
postsPerPage = 10