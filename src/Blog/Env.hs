module Blog.Env
(
  appEnv,
  appEnvIO,
  production,
  development,
  nonProduction
)
where

import Web.Scotty.Trans (ActionT, ScottyError)
import Data.Maybe

import System.Environment

import Control.Monad
import Control.Monad.IO.Class
  
appEnvIO :: IO String
appEnvIO = fromMaybe "development" <$> lookupEnv "ENV"
  
appEnv :: (ScottyError e, MonadIO m) => ActionT e m String
appEnv = liftIO $ appEnvIO

production :: (ScottyError e, MonadIO m) => ActionT e m () -> ActionT e m ()
production = whenEnv "production"
  
development :: (ScottyError e, MonadIO m) => ActionT e m () -> ActionT e m ()
development = whenEnv "development"

nonProduction :: (ScottyError e, MonadIO m) => ActionT e m () -> ActionT e m ()
nonProduction = whenEnvNot "production"
  
whenEnv :: (ScottyError e, MonadIO m) => String -> ActionT e m () -> ActionT e m ()
whenEnv env action = do
  e <- appEnv
  when (e == env) action
  
whenEnvNot :: (ScottyError e, MonadIO m) => String -> ActionT e m () -> ActionT e m ()
whenEnvNot env action = do
  e <- appEnv
  when (e /= env) action