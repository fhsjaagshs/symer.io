{-# LANGUAGE OverloadedStrings #-}

module Blog.Web.Cookie 
(
  Cookie(..),
  parseCookie
)
where

import Control.Applicative
import Data.Default
import Data.Char
import Data.Time.Format
import Data.Time.Clock (UTCTime)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 as A

data Cookie = Cookie {
  cookiePairs :: HashMap String String,
  cookiePath :: Maybe String,
  cookieDomain :: Maybe String,
  cookieExpires :: Maybe UTCTime,
  cookieSecure :: Bool,
  cookieHttpOnly :: Bool
} deriving (Show)

instance Default Cookie where
  def = Cookie H.empty Nothing Nothing Nothing False False

-- RFC 1123 date parser
parseCookieDate :: String -> Maybe UTCTime
parseCookieDate = parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"

parseCookie :: B.ByteString -> Maybe Cookie
parseCookie = fmap applyLex . maybeResult . endInput . parse pairs
  where
    -- lexer
    loop c [] = c
    loop c (("secure",_):xs) = loop (c { cookieSecure = True }) xs
    loop c (("httponly",_):xs) = loop (c { cookieHttpOnly = True }) xs
    loop c (("path",v):xs) = loop (c { cookiePath = (Just v) }) xs
    loop c (("domain",v):xs) = loop (c { cookieDomain = (Just v) }) xs
    loop c (("expires",v):xs) = loop (c { cookieExpires = (parseCookieDate v) }) xs
    loop c ((k,v):xs) = loop (c { cookiePairs = (H.insert k v $ cookiePairs c) }) xs
    -- grammar
    pairs = (:) <$> pair <*> (many $ ";" *> skipSpace *> pair)
    pair = do
      k <- token
      v <- option "" $ "=" *> token -- TODO: quoted string
      return (map toLower $ B.unpack k, B.unpack v)
    token = takeWhile1 takeFunc
    -- predicates
    takeFunc ' ' = False
    takeFunc '=' = False
    takeFunc ';' = False
    takeFunc _   = True
    -- helpers
    applyLex = loop def
    endInput = flip feed ""