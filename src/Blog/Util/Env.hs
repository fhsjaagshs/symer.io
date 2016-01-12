module Blog.Util.Env
(
  appEnv,
  appEnvIO,
  production,
  development,
  nonProduction
)
where

import Data.Maybe

import System.Environment

import Control.Monad
import Control.Monad.IO.Class
  
appEnvIO :: IO String
appEnvIO = fromMaybe "development" <$> lookupEnv "ENV"

appEnv :: (MonadIO m) => m String
appEnv = liftIO $ appEnvIO

production :: (MonadIO m) => m () -> m ()
production = whenEnv "production"
  
development :: (MonadIO m) => m () -> m ()
development = whenEnv "development"

nonProduction :: (MonadIO m) => m () -> m ()
nonProduction = whenEnvNot "production"
  
whenEnv :: (MonadIO m) => String -> m () -> m ()
whenEnv env action = do
  e <- appEnv
  when (e == env) action
  
whenEnvNot :: (MonadIO m) => String -> m () -> m ()
whenEnvNot env action = do
  e <- appEnv
  when (e /= env) action