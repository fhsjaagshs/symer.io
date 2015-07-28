{-# LANGUAGE OverloadedStrings #-}

module Blog.Caching
(
  setCacheControl,
  cachedBody,
  rawBodyCached
)
where
  
import Blog.State

import Control.Monad.IO.Class

import Web.Scotty.Trans as Scotty
import Network.HTTP.Types.Status
import qualified Database.Redis as Redis

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Lazy.Encoding as TL

import Data.Serialize
import qualified Data.Digest.Pure.MD5 as MD5

import Data.Maybe

import System.Environment

setCacheControl :: (Monad m, ScottyError e) => ActionT e m ()
setCacheControl = setHeader "Cache-Control" "public, max-age=3600, s-max-age=3600, no-cache, must-revalidate, proxy-revalidate" -- 1 hour

cachedBody :: (ScottyError e) => B.ByteString -> IO BL.ByteString -> ActionT e WebM ()
cachedBody key valueFunc = do
  redis <- webM $ gets stateRedis
  env <- liftIO $ fromMaybe "development" <$> lookupEnv "ENV"
  if env == "production"
    then do
      redisValue <- liftIO . Redis.runRedis redis . Redis.get $ key
      
      case redisValue of
        Right (Just cached) -> rawBodyCached . BL.fromStrict $ cached
        _ -> do
          v <- liftIO $ valueFunc
          _ <- liftIO $ Redis.runRedis redis $ do
            _ <- Redis.set key . BL.toStrict $ v
            Redis.expire key 3600
          rawBodyCached v
          
          
    else (liftIO $ valueFunc) >>= Scotty.raw
  
rawBodyCached :: (Monad m, ScottyError e) => BL.ByteString -> ActionT e m ()
rawBodyCached str = do
  let hashSum = encodeLazy $ MD5.md5 str
  setHeader "Vary" "Accept-Encoding"
  maybeinm <- Scotty.header "If-None-Match"
  case maybeinm of
    Just inm -> do
      if hashSum == (TL.encodeUtf8 inm)
        then status $ mkStatus 304 ""
        else do
          setHeader "ETag" $ TL.decodeUtf8 hashSum
          Scotty.raw str
    Nothing -> do
      setHeader "ETag" $ TL.decodeUtf8 hashSum
      Scotty.raw str
