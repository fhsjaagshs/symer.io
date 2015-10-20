{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Blog.Web.Cookie 
(
  Cookie(..),
  parseCookie
)
where

import qualified Data.ByteString.Char8 as B

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

import           Data.Attoparsec.ByteString.Char8 as A

import           Data.Time.Format
import           Data.Time.Clock (UTCTime)
import           Data.Default

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

-- TODO: fix this bitch (removing the feed func will cause an incomplete input error)
parseCookie :: B.ByteString -> Maybe Cookie
parseCookie = maybeResult . flip feed "" . parse cookieParser
  where
    cookieParser = (loop def) <$> pairs
    -- lexer
    loop c [] = c
    loop c (("secure",_):xs) = loop (c { cookieSecure = True }) xs
    loop c (("httponly",_):xs) = loop (c { cookieHttpOnly = True }) xs
    loop c (("path",v):xs) = loop (c { cookiePath = (Just v) }) xs
    loop c (("domain",v):xs) = loop (c { cookieDomain = (Just v) }) xs
    loop c (("expires",v):xs) = loop (c { cookieExpires = (parseCookieDate v) }) xs
    loop c ((k,v):xs) = loop (c { cookiePairs = (H.insert k v $ cookiePairs c) }) xs
    -- grammar
    pairs = many' entry
    entry = choice [pair, httpOnly, secure]
    httpOnly = skipSpace >> stringCI "httponly" *> pure ("httponly", "")
    secure = skipSpace >> stringCI "secure" *> pure ("secure", "")
    pair = do
      skipSpace
      k <- A.takeWhile takeFunc
      skipSpace
      char '='
      skipSpace
      v <- A.takeWhile takeFunc
      option ';' $ char ';'
      return (B.unpack k, B.unpack v)
    -- helpers
    takeFunc ' ' = False
    takeFunc '=' = False
    takeFunc ';' = False
    takeFunc _   = True