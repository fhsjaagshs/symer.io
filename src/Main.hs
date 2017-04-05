{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Blog.App
import Blog.AppState (postgresMigrate)
import Web.App
import Options.Applicative
import qualified Data.ByteString.Char8 as B (pack,unpack)
import Crypto.BCrypt (hashPasswordUsingPolicy,HashingPolicy(..))
import Database.PostgreSQL.Simple (connectPostgreSQL,close)
import System.Environment

import Data.Monoid

import Network.Wai.Middleware.AddHeaders
import Network.Wai.Middleware.ForceSSL
import Network.Wai.Middleware.Gzip

-- Setup:
-- ensure your executable is setuid & owned by root:
-- sudo chmod 4775 dist/build/blog/blog
-- sudo chown root dist/build/blog/blog

data Util = Password String
          | MigrateDB String

main :: IO ()
main = do
  env <- lookupEnv "ENV"
  let middleware = [gzip def, addHeaders [("Cache-Control",ccontrol)], if env == (Just "production") then forceSSL else id]
  webappMainIO app middleware (Just parseUtil) handleUtil
  where ccontrol = "public,max-age=3600,s-max-age=3600,no-cache,must-revalidate,proxy-revalidate,no-transform"

parseUtil :: Parser Util
parseUtil = subparser $ (mkcmd "password" "Hash a password" parsePassword) <>
                        (mkcmd "migrate-db" "Migrate database" parseMigrateDB)
  where
    parsePassword = Password <$> (strArgument $ metavar "PASSWORD" <> help "password to hash")
    parseMigrateDB = MigrateDB <$> connstr
    connstr = strOption $ long "connstr" <> short 'c' <> value "" <> metavar "CONNSTR" <> help "connect to Postgres at CONNSTR."
    mkcmd cmd desc p = command cmd $ info (helper <*> p) $ progDesc desc
    
handleUtil :: Util -> IO ()
handleUtil (Password s) = hsh s >>= maybe (return ()) putStrLn
  where
    hsh = fmap (fmap B.unpack) . hashPasswordUsingPolicy (HashingPolicy 12 "$2b$") . B.pack
handleUtil (MigrateDB connstr) = do
  conn <- connectPostgreSQL $ B.pack connstr
  postgresMigrate conn
  close conn