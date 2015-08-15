{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Blog.Caching
(
  setCacheControl,
  cachedBody,
  rawBodyCached
)
where

import Blog.State
import Blog.Env

import Control.Monad
import Control.Monad.IO.Class

import Web.Scotty.Trans as Scotty (ScottyError(..), ActionT, raw, setHeader, header, status)
import Network.HTTP.Types.Status (Status(..))
import qualified Database.Redis as Redis (runRedis, get, set, expire)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Lazy.Encoding as TL

import Data.Serialize
import qualified Data.Digest.Pure.MD5 as MD5

setCacheControl :: (Monad m, ScottyError e) => ActionT e m ()
setCacheControl = Scotty.setHeader "Cache-Control" "public, max-age=3600, s-max-age=3600, no-cache, must-revalidate, proxy-revalidate, no-transform" -- 1 hour

cachedBody :: (ScottyError e) => B.ByteString -> ActionT e WebM BL.ByteString -> ActionT e WebM ()
cachedBody key valueFunc = do
  nonProduction $ valueFunc >>= Scotty.raw
  production $ do
    redis <- webM $ gets stateRedis
    (liftIO . Redis.runRedis redis . Redis.get $ key) >>= r redis
  where
    r _ (Right (Just cached)) = rawBodyCached . BL.fromStrict $ cached
    r redis _ = do
      v <- valueFunc
      rawBodyCached v
      void . liftIO . Redis.runRedis redis $ do
        Redis.set key . BL.toStrict $ v
        Redis.expire key 3600

rawBodyCached :: (Monad m, ScottyError e) => BL.ByteString -> ActionT e m ()
rawBodyCached str = do
  Scotty.setHeader "Vary" "Accept-Encoding"
  Scotty.header "If-None-Match" >>= f . maybe False (== hashSum)
  where
    hashSum = TL.decodeUtf8 . encodeLazy . MD5.md5 $ str
    f False = Scotty.status $ Status 304 ""
    f True = Scotty.setHeader "ETag" hashSum >> Scotty.raw str