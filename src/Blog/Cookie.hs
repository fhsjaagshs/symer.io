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
import           Data.Char (toLower)

import           Data.Attoparsec.ByteString.Char8 as A

import           Data.Time.Format
import           Data.Time.Clock (UTCTime)
import           Data.Default

-------------------------------------------------------------------------------
-- | Types
  
data Cookie = Cookie {
  cookiePairs :: [(String, String)],
  cookiePath :: Maybe String,
  cookieDomain :: Maybe String,
  cookieExpires :: Maybe UTCTime,
  cookieSecure :: Bool,
  cookieHttpOnly :: Bool
} deriving (Show)

instance Default Cookie where
  def = Cookie [] Nothing Nothing Nothing False False

-------------------------------------------------------------------------------
-- | Date Formatting

-- RFC 1123 date format
cookieDateFormat :: String
cookieDateFormat = "%a, %d %b %Y %H:%M:%S GMT"

parseCookieDate :: String -> Maybe UTCTime
parseCookieDate = parseTimeM True defaultTimeLocale cookieDateFormat

formatCookieDate :: UTCTime -> String
formatCookieDate = formatTime defaultTimeLocale cookieDateFormat

-------------------------------------------------------------------------------
-- | Individual Cookie Parsing

-- Generates a list of Set-Cookie headers
serializeCookie :: Cookie -> [String]
serializeCookie (Cookie [] _ _ _ _ _) = []
serializeCookie (Cookie ((k,v):xs) path domain expires secure httponly) = cookie:(serializeCookie remaining)
  where
    cookie = 
      k ++ "=" ++ v
      ++ (maybe "" (((++) "; expires=") . formatCookieDate) expires)
      ++ (maybe "" ((++) "; domain=") domain)
      ++ (maybe "" ((++) "; path=") path)
      ++ (if secure then "; Secure" else "")
      ++ (if httponly then "; HttpOnly" else "")
    remaining = Cookie xs path domain expires secure httponly

parseCookie :: B.ByteString -> Maybe Cookie
parseCookie = maybeResult . flip feed "" . parse cookieParser

cookieParser :: Parser Cookie
cookieParser = do
  pairs <- parsePairs
  
  expires <- option Nothing $ do
    char ';'
    skipSpace
    string "expires="
    parseCookieDate . B.unpack <$> takeUntil ';'

  domain <- option Nothing $ do
    char ';'
    skipSpace
    string "domain="
    Just . B.unpack <$> takeUntil ';'
    
  path <- option Nothing $ do
    char ';'
    skipSpace
    string "path="
    Just . B.unpack <$> takeUntil ';'
    
  secure <- option False $ do
    char ';'
    skipSpace
    (==) "secure" . map toLower <$> many1 letter_iso8859_15
    
  httponly <- option False $ do
    char ';'
    skipSpace
    (==) "httponly" . map toLower <$> many1 letter_iso8859_15

  return $ Cookie pairs path domain expires secure httponly
  where
    takeUntil c = A.takeWhile (not . (==) c)
    parsePair = do
      skipSpace
      key <- B.unpack <$> takeUntil '='
      char '='
      value <- B.unpack <$> takeUntil ';'
      return (key, value)
    parsePairs = do
      mp <- option Nothing (Just <$> parsePair)
      case mp of
        Nothing -> return []
        Just p -> ((:) p) <$> parsePairs