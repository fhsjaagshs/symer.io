{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

-- to connect using psql (on blog.symer.io):
-- cd /deploy/ssl/
-- sudo psql "sslmode=verify-ca host=db.symer.io dbname=blog port=5432 sslrootcert=root.crt sslcert=server.crt sslkey=server.key user=symerdotio"

module Blog.Postgres 
(
  Postgres(..),
  PostgresScottyM,
  PostgresActionM,
  webMQuery,
  webMQuery_,
  postsPerPage
)
where

import System.WebApp
import Blog.Util.Env

import Control.Monad
import Control.Monad.IO.Class

import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ActionT, ScottyT)

-- import Database.PostgreSQL.Simple (Connection,ToRow,FromRow)
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration as PG.Migration

import           Blaze.ByteString.Builder (toByteString)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8

-- conveniences
type PostgresScottyM a = ScottyT Text (WebAppM Postgres) a
type PostgresActionM a = ActionT Text (WebAppM Postgres) a

data Postgres = Postgres {
  postgresConnection :: Connection
}

instance WebAppState Postgres where
  initState = do
    putStrLn "establishing database connections"
    pg <- appEnvIO >>= connectPostgreSQL . connString
    putStrLn "running database migrations"
    void $ withTransaction pg $ do
      void $ execute_ pg "SET client_min_messages=WARNING;"
      void $ runMigration $ MigrationContext MigrationInitialization True pg
      void $ execute_ pg "SET client_min_messages=NOTICE;"
      runMigration $ MigrationContext (MigrationFile "blog.sql" "migrations/blog.sql") True pg
    return $ Postgres pg
    where
    connString "production" = "" -- postgres config loaded *only* from env vars
    connString _            = "dbname='blog' host='localhost'"
  destroyState (Postgres pg) = do
    putStrLn "closing database connections"
    close pg

postsPerPage :: Int
postsPerPage = 10
    
webMQuery :: (ToRow q, FromRow r) => String -> q -> PostgresActionM [r]
webMQuery q ps = do
  pg <- fmap postgresConnection getState
  liftIO $ query pg (Query . toByteString . Utf8.fromString $ q) ps
  
webMQuery_ :: (ToRow q) => String -> q -> PostgresActionM ()
webMQuery_ q ps = void $ do
  pg <- fmap postgresConnection getState
  liftIO $ execute pg (stringToQuery q) ps

stringToQuery :: String -> Query
stringToQuery = Query . toByteString . Utf8.fromString