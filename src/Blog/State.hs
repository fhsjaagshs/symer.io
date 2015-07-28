{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blog.State
(
  AppState(..),
  WebM(..),
  webM,
  gets,
  puts,
  modify
) where
  
import           Control.Concurrent.STM
import           Control.Monad.Reader 

import qualified Database.Redis as Redis
import qualified Database.PostgreSQL.Simple as PG

data AppState = AppState { 
  stateRedis :: Redis.Connection,
  statePostgres :: PG.Connection
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
