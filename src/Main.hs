{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Blog.App
import Blog.AppState (postgresMigrate)
import Blog.Util.DumpDatabase
import Web.App
import Options.Applicative
import qualified Data.ByteString.Char8 as B (pack,unpack)
import Crypto.BCrypt (hashPasswordUsingPolicy,HashingPolicy(..))
import Database.PostgreSQL.Simple (connectPostgreSQL,close)

-- Setup:
-- ensure your executable is setuid & owned by root:
-- sudo chmod 4775 dist/build/blog/blog
-- sudo chown root dist/build/blog/blog

data Util = Password String
          | DumpDB FilePath String
          | LoadDB FilePath String
          | MigrateDB String

main :: IO ()
main = webappMainIO app (Just parseUtil) handleUtil

parseUtil :: Parser Util
parseUtil = subparser $ (mkcmd "password" "Hash a password" parsePassword) <>
                        (mkcmd "dump-db" "Dump database to JSON" parseDumpDB) <>
                        (mkcmd "load-db" "Load database from JSON" parseLoadDB) <>
                        (mkcmd "migrate-db" "Migrate database" parseMigrateDB)
  where
    parsePassword = Password <$> (strArgument $ metavar "PASSWORD" <> help "password to hash")
    parseDumpDB = DumpDB <$> mkOpt "output" 'o' "FILEPATH" "dump JSON into FILEPATH." <*> connstr
    parseLoadDB = LoadDB <$> mkOpt "input" 'i' "FILEPATH" "load JSON from FILEPATH." <*> connstr
    parseMigrateDB = MigrateDB <$> connstr
    connstr = strOption $ long "connstr" <> short 'c' <> value "" <> metavar "CONNSTR" <> help "connect to Postgres at CONNSTR."
    mkcmd cmd desc p = command cmd $ info (helper <*> p) $ progDesc desc
    mkOpt l s mv h = strOption $ long l <> short s <> metavar mv <> help h
    
handleUtil :: Util -> IO ()
handleUtil (Password s) = hsh s >>= maybe (return ()) putStrLn
  where
    hsh = fmap (fmap B.unpack) . hashPasswordUsingPolicy (HashingPolicy 12 "$2b$") . B.pack
handleUtil (DumpDB fp connstr) = dumpDB fp connstr
handleUtil (LoadDB fp connstr) = loadDB fp connstr
handleUtil (MigrateDB connstr) = do
  conn <- connectPostgreSQL $ B.pack connstr
  postgresMigrate conn
  close conn