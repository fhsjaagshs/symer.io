{-# LANGUAGE OverloadedStrings #-}

module Blog.Web.Caching
(
  setCacheControl,
  cachedBody
)
where

import Blog.State
import Blog.Util.Env
import Blog.System.FileCache

import Control.Monad.IO.Class

import Web.Scotty.Trans as Scotty (ScottyError(..), ActionT, raw, setHeader, header, status)
import Network.HTTP.Types.Status (Status(..))

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Lazy.Encoding as TL

import Crypto.Hash.MD5 as MD5 (hashlazy)

setCacheControl :: (Monad m, ScottyError e) => ActionT e m ()
setCacheControl = Scotty.setHeader "Cache-Control" "public, max-age=3600, s-max-age=3600, no-cache, must-revalidate, proxy-revalidate, no-transform" -- 1 hour

-- timeout -> The timeout if key is not a file path
cachedBody :: (ScottyError e) => Int -> B.ByteString -> ActionT e WebM BL.ByteString -> ActionT e WebM ()
cachedBody timeout key valueFunc = do
  nonProduction $ valueFunc >>= Scotty.raw
  production $ do
    cache <- webM $ gets stateCache
    (liftIO $ fclookup cache key) >>= f cache
  where
    f _     (Just cached) = rawBodyCached . BL.fromStrict $ cached
    f cache Nothing       = do
      v <- valueFunc
      rawBodyCached v
      liftIO $ fcinsert cache timeout key (BL.toStrict v)
    rawBodyCached str = do
      Scotty.setHeader "Vary" "Accept-Encoding"
      Scotty.header "If-None-Match" >>= g . maybe False (== hashSum)
      Scotty.raw str
      where
        md5sum = BL.fromStrict . B16.encode . MD5.hashlazy
        hashSum = TL.decodeUtf8 . md5sum $ str
        g True = Scotty.status $ Status 304 ""
        g False = Scotty.setHeader "ETag" hashSum >> Scotty.raw str
    
