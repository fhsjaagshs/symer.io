{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.App.Assets
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

General web app operations related to managing assets.
-}

module Blog.Assets
(
  loadAsset
)
where

import Web.App

import Blog.AppState
import Blog.FileCache as FC
import Blog.Util.Minification

import Network.HTTP.Types.Status
import Network.Mime

import System.FilePath
import System.Posix (fileAccess)

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception

import Data.Bool
import Data.Monoid

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL

-- | Loads an asset from the 'FileCache' associated with the 'WebAppM' monad.
loadAsset :: (MonadIO m)
          => FilePath -- ^ 'FilePath' relative to @assets/@ to load
          -> RouteT AppState m ()
loadAsset assetsPath = do
  exists <- liftIO $ canReadFile ("assets/" <> assetsPath)
  bool (status status404) (getState >>= loadFromCache . appStateCache) exists
  where
    mimetype = defaultMimeLookup $ T.pack $ takeFileName assetsPath
    loadFromCache cache = (liftIO $ FC.lookup cache assetsPath) >>= f cache
    f _     (Just (Left err)) = status status500 >> writeBody err
    f _     (Just (Right (cached, md5))) = do
      addHeader "Content-Type" $ mimetype <> "; charset=utf-8"
      addHeader "Vary" "Accept-Encoding"
      addHeader "Content-Encoding" "gzip" -- files in 'FileCache's are gzipped
      header "If-None-Match" >>= h . maybe False (== md5)
      where h = bool (addHeader "ETag" md5 >> writeBody cached) (status status304)
    f cache Nothing = do
      void $ liftIO $ FC.register' cache assetsPath $ g mimetype
      loadFromCache cache
    g "application/javascript" = fmap BL.toStrict . minifyJS . B.unpack
    g "text/css" = fmap (BL.toStrict . TL.encodeUtf8) . minifyCSS . T.decodeUtf8
    g _ = Right
    
canReadFile :: FilePath -> IO Bool
canReadFile fp = do
  v <- try $ fileAccess fp True False False
  case (v :: Either IOError Bool) of
      Left _ -> return False
      Right r -> return r