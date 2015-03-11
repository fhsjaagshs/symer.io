{-# LANGUAGE OverloadedStrings #-}

module Config (postgresConfig, postgresURL, postgresConnStr) where
  
import Control.Exception
import System.IO.Unsafe

import Data.List
  
import System.Environment

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

unsafeGetEnv :: String -> String -> String
unsafeGetEnv key defaultValue = unsafePerformIO $ catchAny (getEnv key) $ \_ -> return defaultValue

appEnv :: String
appEnv = unsafeGetEnv "ENVIRONMENT" "development"
  
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
                    ("password", unsafeGetEnv "PGPASSWORD" "development"),
                    ("host", "db.symer.io"),
                    ("port", "5432"),
                    ("dbname", "blog"),
                    ("sslmode", "require")
                    ]
postgresConfigs _ = []
                    
postgresConfig :: [(String, String)]
postgresConfig = postgresConfigs appEnv

configFor :: String -> String
configFor "port" = do
  case (lookup "port" postgresConfig) of
    (Just value) -> value
    _ -> "5432"
configFor key = do
  case (lookup key postgresConfig) of
    (Just value) -> value
    _ -> ""

postgresURL :: String
postgresURL = "postgresql://" ++ (configFor "user") ++ (if (configFor "password") == "" then "" else ":" ++ (configFor "password")) ++ "@" ++ (configFor "host") ++ ":" ++ (configFor "port") ++ "/" ++ (configFor "dbname")

postgresConnStr :: String
postgresConnStr = intercalate " " $ (map (\(k,v) -> k ++ "='" ++ v ++ "'") (map (\(k,v) -> (k, configFor k) ) postgresConfig))