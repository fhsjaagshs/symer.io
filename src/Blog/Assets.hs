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

import Network.HTTP.Types.Status
import Network.Mime

import System.FilePath
import System.Directory

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class as S

import Data.Monoid

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TL

import qualified Text.CSS.Parse as CSS (parseNestedBlocks)
import qualified Text.CSS.Render as CSS (renderNestedBlocks)
import qualified Text.Jasmine as JS (minifym)

-- | Loads an asset from the 'FileCache' associated with the 'WebAppM' monad.
loadAsset :: (MonadIO m)
          => FilePath -- ^ 'FilePath' relative to @assets/@ to load
          -> RouteT AppState m ()
loadAsset assetsPath = do
  exists <- liftIO $ doesFileExist relPath
  if not exists
    then do
      status status404
      writeBodyBytes "File "
      writeBody relPath
      writeBodyBytes " does not exist."
    else S.get >>= loadFromCache . appStateCache
  where
    mimetype = defaultMimeLookup . T.pack . takeFileName $ assetsPath
    relPath = "assets/" <> assetsPath
    loadFromCache cache = (liftIO $ FC.lookup cache assetsPath) >>= f cache
    f _     (Just (Left err)) = do
      status status500
      writeBody err
    f _     (Just (Right (cached, md5))) = do
      addHeader "Content-Type" $ mimetype <> "; charset=utf-8"
      addHeader "Vary" "Accept-Encoding"
      addHeader "Content-Encoding" "gzip" -- files in FileCaches are gzipped
      header "If-None-Match" >>= h . maybe False (== md5)
      where
        h True = status status304
        h False = do
          addHeader "ETag" md5
          writeBody cached
    f cache Nothing = do
      void $ liftIO $ FC.register' cache assetsPath $ g mimetype
      loadFromCache cache
    g "application/javascript" = fmap BL.toStrict . JS.minifym . BL.fromStrict
    g "text/css" = fmap r . CSS.parseNestedBlocks . T.decodeUtf8
      where
        r = BL.toStrict . TL.encodeUtf8 . TL.toLazyText . CSS.renderNestedBlocks
    g _ = Right