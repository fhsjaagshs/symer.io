{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

-- to connect using psql (on blog.symer.io):
-- cd /deploy/ssl/
-- sudo psql "sslmode=verify-ca host=db.symer.io dbname=blog port=5432 sslrootcert=root.crt sslcert=server.crt sslkey=server.key user=symerdotio"

module Blog.Postgres 
(
  -- * Types
  Postgres(..),
  PostgresScottyM,
  PostgresActionM,
  -- * Connecting and Disconnecting
  connectPostgres,
  disconnectPostgres,
  -- * Performing Queries
  postgresQuery,
  postgresExec,
  maybeQuery
)
where

import Web.App
import Blog.Util.Env

import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe

import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ActionT, ScottyT)

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
  initState = Postgres <$> connectPostgres
  destroyState (Postgres pg) = disconnectPostgres pg
    
-- |Connect to a PostgreSQL database. Configure via LibPQ
-- environment variables. In production, no configuration is
-- hardcoded. In development, the database is set to @blog@
-- and the host is set to @localhost@.
connectPostgres :: IO Connection
connectPostgres = do
  putStrLn "establishing database connections"
  pg <- appEnvIO >>= connectPostgreSQL . connString
  putStrLn "running database migrations"
  void $ withTransaction pg $ do
    void $ execute_ pg "SET client_min_messages=WARNING;"
    void $ runMigration $ MigrationContext MigrationInitialization True pg
    void $ execute_ pg "SET client_min_messages=NOTICE;"
    runMigration $ MigrationContext (MigrationFile "blog.sql" "migrations/blog.sql") True pg
  return pg
  where
  connString "production" = "" -- postgres config loaded *only* from env vars
  connString _            = "dbname='blog' host='localhost'"
  
-- |Disconnect from a PostgreSQL database.
disconnectPostgres :: Connection -> IO ()
disconnectPostgres pg = do
  putStrLn "closing database connections"
  close pg
    
-- |Make a PostgreSQL query & return parameters.
postgresQuery :: (ToRow q, FromRow r) => String -- query
                                      -> q -- parameters
                                      -> PostgresActionM [r]
postgresQuery q ps = do
  pg <- fmap postgresConnection getState
  liftIO $ query pg (Query . toByteString . Utf8.fromString $ q) ps
  
-- |Make a PostgreSQL query & don't return parameters.
postgresExec :: (ToRow q) => String -- query
                          -> q -- parameters
                          -> PostgresActionM ()
postgresExec q ps = void $ do
  pg <- fmap postgresConnection getState
  liftIO $ execute pg (stringToQuery q) ps

stringToQuery :: String -> Query
stringToQuery = Query . toByteString . Utf8.fromString

-- |Make a query return a maybe value.
maybeQuery :: PostgresActionM [Only a] -- ^ query action to maybe-ify
           -> PostgresActionM (Maybe a)
maybeQuery res = (listToMaybe . map fromOnly) <$> res