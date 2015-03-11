{-# LANGUAGE OverloadedStrings #-}

module Config (postgresConfig, postgresURL, postgresConnStr) where
  
import Control.Exception
import System.IO.Unsafe

import Data.List
  
import System.Environment
  
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch
  
postgresConfig :: [(String, String)]
postgresConfig = [
                    ("user", "nathaniel"),
                    ("password", unsafePerformIO $ catchAny (getEnv "DB_PASSWORD") $ \_ -> return ""),
                    ("host", "localhost"),
                    ("port", "5432"),
                    ("dbname", "blog")
                    ]

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