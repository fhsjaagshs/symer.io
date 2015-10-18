{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Blog.Cookie 
(
  Cookie(..),
  parseCookie,
  serializeCookie
)
where

import qualified Data.ByteString.Char8 as B

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

import           Data.Attoparsec.ByteString.Char8 as A

import           Data.Time.Format
import           Data.Time.Clock (UTCTime)
import           Data.Default

-------------------------------------------------------------------------------
-- | Types
  
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

-------------------------------------------------------------------------------
-- | Date Formatting

-- RFC 1123 date format
cookieDateFormat :: String
cookieDateFormat = "%a, %d %b %Y %H:%M:%S GMT"

parseCookieDate :: String -> Maybe UTCTime
parseCookieDate = parseTimeM True defaultTimeLocale cookieDateFormat

-- UNUSED
-- formatCookieDate :: UTCTime -> String
-- formatCookieDate = formatTime defaultTimeLocale cookieDateFormat

-------------------------------------------------------------------------------
-- | Cookie Parsing

-- Build list of Set-Cookie headers
-- TODO: should this set the Secure & HttpOnly flags
--       ^^^^^ do that
serializeCookie :: Cookie -> [String]
serializeCookie = f [] . H.toList . cookiePairs
  where
    f :: [String] -> [(String, String)] -> [String]
    f accum [] = accum
    f accum ((k,v):xs) = f ((k ++ "=" ++ v):accum) xs

parseCookie :: B.ByteString -> Maybe Cookie
parseCookie = maybeResult . flip feed "" . parse cookieParser

cookieParser :: Parser Cookie
cookieParser = (loop def) <$> pairs
  where
    -- lexer
    loop c [] = c
    loop c (("secure",_):xs) = loop (c { cookieSecure = True }) xs
    loop c (("httponly",_):xs) = loop (c { cookieHttpOnly = True }) xs
    loop c (("path",v):xs) = loop (c { cookiePath = (Just v) }) xs
    loop c (("domain",v):xs) = loop (c { cookieDomain = (Just v) }) xs
    loop c (("expires",v):xs) = loop (c { cookieExpires = (parseCookieDate v) }) xs
    loop c ((k,v):xs) = loop (c { cookiePairs = (H.insert k v $ cookiePairs c) })  xs
    -- grammar
    pairs = many' entry
    entry = choice [pair, httpOnly, secure]
    httpOnly = stringCI "httponly" >> return ("httponly", "")
    secure = stringCI "secure" >> return ("secure", "")
    pair = do
      skipSpace
      k <- A.takeWhile (not . isSpace)
      skipSpace
      char '='
      skipSpace
      v <- A.takeWhile (not . isSpace)
      char ';'
      return (B.unpack k, B.unpack v)