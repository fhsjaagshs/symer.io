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
  makePostgresPool,
  destroyPostgresPool,
  withPostgres,
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

import Data.Pool
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration as PG.Migration

import           Blaze.ByteString.Builder (toByteString)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8

-- conveniences
type PostgresScottyM a = ScottyT Text (WebAppM Postgres) a
type PostgresActionM a = ActionT Text (WebAppM Postgres) a

data Postgres = Postgres {
  postgresPool :: Pool Connection
}

instance WebAppState Postgres where
  initState = Postgres <$> makePostgresPool
  destroyState (Postgres pool) = destroyPostgresPool pool
  
-- |Create a pool that pools PostgreSQL connections.
-- Configure connections via LibPQ environment variables.
-- In production, no configuration is hardcoded. In
-- development, the database is set to @blog@ and
-- the host is set to @localhost@.
makePostgresPool :: IO (Pool Connection)
makePostgresPool = do
  pool <- createPool (appEnvIO >>= connectPostgreSQL . connString) close 2 5.0 5
  withResource pool $ \pg -> void $ withTransaction pg $ do
    void $ execute_ pg "SET client_min_messages=WARNING;"
    void $ runMigration $ MigrationContext MigrationInitialization True pg
    void $ execute_ pg "SET client_min_messages=NOTICE;"
    runMigration $ MigrationContext (MigrationFile "blog.sql" "migrations/blog.sql") True pg
  return pool
  where
  connString "production" = "" -- postgres config loaded *only* from env vars
  connString _            = "dbname='blog' host='localhost'"
  
-- |Empty a postgres connection pool.
destroyPostgresPool :: Pool Connection -> IO ()
destroyPostgresPool = destroyAllResources
    
-- |"lift" a function into the connection pool.
withPostgres :: (Connection -> IO b) -> PostgresActionM b
withPostgres f = getState >>= \(Postgres p) -> liftIO $ withResource p f
    
-- |Make a PostgreSQL query & return parameters.
postgresQuery :: (FromRow a, ToRow b) => String -- ^ query
                                      -> b -- ^ parameters
                                      -> PostgresActionM [a] -- ^ returned rows
postgresQuery q ps = withPostgres $ \pg -> query pg (stringToQuery q) ps
  
-- |Make a PostgreSQL query & don't return parameters.
postgresExec :: (ToRow a) => String -- query
                          -> a -- parameters
                          -> PostgresActionM ()
postgresExec q ps = withPostgres $ \pg -> void $ execute pg (stringToQuery q) ps

stringToQuery :: String -> Query
stringToQuery = Query . toByteString . Utf8.fromString

-- |Make a query return a maybe value. Useful if you're updating a
-- single row and want to get a single column from the updated row
-- using @RETURNING@.
maybeQuery :: PostgresActionM [Only a] -- ^ query action
           -> PostgresActionM (Maybe a)
maybeQuery res = (listToMaybe . map fromOnly) <$> res