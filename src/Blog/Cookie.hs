{-# LANGUAGE OverloadedStrings, TupleSections #-}

{-|
Module      : Web.App.Cookie
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : Cross-Platform

Parse HTTP cookies
-}

module Blog.Cookie 
(
  -- * Cookie Parsing
  parseCookie,
  getCookies
)
where

import Web.App

import Control.Applicative
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 as A

parseCookie :: B.ByteString -> Maybe [(String,String)]
parseCookie = maybeResult . flip feed "" . parse pairs
  where pairs = (:) <$> pair <*> (many' $ ";" *> skipSpace *> pair)
        pair = (,) <$> attr <*> val
        attr = map toLower <$> many' (satisfy nametoken)
        val = option mempty $ "=" *> q *> many' (satisfy token) <* q
        token v = (ord v >= 0x21) && (ord v <= 0x7E) && (v /= ' ') && (v /= ',') && (v /= ';')  
        nametoken v = token v && (v /= '=')
        q = "\"" <|> "'" <|> pure "" -- RFC 6265 states that cookies' values can be quoted

getCookies :: (WebAppState s, Monad m) => RouteT s m [(String,String)]
getCookies = mconcat . catMaybes . map parseCookie . foldl f [] <$> headers
  where f a ("Cookie",v) = a ++ [v]
        f a _ = a