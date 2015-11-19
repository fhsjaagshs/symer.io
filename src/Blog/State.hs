{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Blog.State
(
  AppState(..),
  WebM(..),
  webM,
  gets,
  puts,
  modify,
  initState
) where
  
import Blog.Database.Config
import Blog.User
import Blog.System.FileCache
  
import           Control.Concurrent.STM
import           Control.Monad.Reader 

import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Migration as PG.Migration

import qualified Data.ByteString.Char8 as B

data AppState = AppState {
  statePostgres :: PG.Connection,
  stateCache :: FileCache
}

newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

-- Scotty's monads are layered on top of our custom monad.
-- We define this synonym for lift in order to be explicit
-- about when we are operating at the 'WebM' layer.
webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

-- Some helpers to make this feel more like a state monad.
gets :: (AppState -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

puts :: AppState -> WebM ()
puts v = ask >>= liftIO . atomically . flip writeTVar v

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

initState :: FilePath -> FilePath -> FilePath -> IO AppState
initState rootcrt crt key = do
  putStrLn "initializing cache"
  cache <- mkFileCache "assets/"
  putStrLn "establishing database connections"
  pg <- PG.connectPostgreSQL $ B.pack $ postgresConnStr rootcrt crt key
  putStrLn "running database migrations"
  runMigrations pg
  ((PG.query_ pg "SELECT * FROM users") :: IO [User]) >>= print
  return $ AppState pg cache
  where
    runMigrations pg = PG.withTransaction pg $ do
      PG.execute_ pg "SET client_min_messages=WARNING;"
      runMigration $ MigrationContext MigrationInitialization True pg
      PG.execute_ pg "SET client_min_messages=NOTICE;"
      forM_ migrations $ \(f, p) -> do
        runMigration $ MigrationContext (MigrationFile f p) True pg