{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

-- to connect using psql (on blog.symer.io):
-- cd /deploy/ssl/
-- sudo psql "sslmode=verify-ca host=db.symer.io dbname=blog port=5432 sslrootcert=root.crt sslcert=server.crt sslkey=server.key user=symerdotio"

module Blog.AppState
(
  -- * Types
  AppState(..),
  -- * Connecting and Disconnecting
  makePostgresPool,
  destroyPostgresPool,
  withPostgres,
  postgresMigrate,
  -- * Performing Queries
  postgresQuery,
  postgresExec,
  onlyQuery
)
where

import Web.App
import Blog.FileCache

import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.State.Class as S

import Data.Maybe

import Data.Pool
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration as PG.Migration

import           Blaze.ByteString.Builder (toByteString)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8

import System.Environment

data AppState = AppState {
  appStatePool :: Pool Connection,
  appStateCache :: FileCache
}

instance WebAppState AppState where
  initState = AppState <$> makePostgresPool <*> newFileCache "assets/"
  destroyState (AppState pool cache) = do
    destroyPostgresPool pool
    teardownFileCache cache
  
-- |Create a pool that pools PostgreSQL connections.
-- Configure connections via LibPQ environment variables.
-- In production, no configuration is hardcoded. In
-- development, the database is set to @blog@ and
-- the host is set to @localhost@.
makePostgresPool :: IO (Pool Connection)
makePostgresPool = do
  putStrLn "Initializing PostgreSQL connection pool"
  env <- lookupEnv "ENV"
  pool <- createPool (connectPostgreSQL $ connString env) close 2 5.0 5
  withResource pool postgresMigrate
  return pool
  where
  connString (Just "production") = "" -- postgres config loaded *only* from env vars
  connString _                   = "dbname='blog' host='localhost'"
  
-- |Empty a postgres connection pool.
destroyPostgresPool :: Pool Connection -> IO ()
destroyPostgresPool = destroyAllResources

-- |Run DB migrations
postgresMigrate :: Connection -> IO ()
postgresMigrate pg = void $ withTransaction pg $ do
  void $ execute_ pg "SET client_min_messages=WARNING;"
  void $ runMigration $ MigrationContext MigrationInitialization True pg
  void $ execute_ pg "SET client_min_messages=NOTICE;"
  runMigration $ MigrationContext (MigrationFile "blog.sql" "migrations/blog.sql") True pg
  
-- |"lift" a function into the connection pool.
withPostgres :: (MonadIO m)
             => (Connection -> IO b) -- ^ function that given a 'Connection' creates an 'IO' action
             -> RouteT AppState m b
withPostgres f = S.get >>= \(AppState p _) -> liftIO $ withResource p f
    
-- |Make a PostgreSQL query & return parameters.
postgresQuery :: (FromRow a, ToRow b, MonadIO m)
              => String -- ^ query
              -> b -- ^ parameters
              -> RouteT AppState m [a] -- ^ returned rows
postgresQuery q ps = withPostgres $ \pg -> query pg (stringToQuery q) ps
  
-- |Make a PostgreSQL query & don't return parameters.
postgresExec :: (MonadIO m, ToRow a)
             => String -- query
             -> a -- parameters
             -> RouteT AppState m ()
postgresExec q ps = withPostgres $ \pg -> void $ execute pg (stringToQuery q) ps

stringToQuery :: String -> Query
stringToQuery = Query . toByteString . Utf8.fromString

-- |Make a query return a maybe value. Useful if you're updating a
-- single row and want to get a single column from the updated row
-- using @RETURNING@.
onlyQuery :: (MonadIO m)
          => RouteT AppState m [Only a] -- ^ query action
          -> RouteT AppState m (Maybe a)
onlyQuery res = fmap fromOnly . listToMaybe <$> res