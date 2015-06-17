{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Cookie 
(
  Cookie(..),
  parseCookie,
  parseCookies,
  serializeCookie,
  serializeCookies
)
where
  
import           Data.Char (toLower)
import           Data.List (intercalate)

import           Text.Parsec

import           Data.Time.Format
import           Data.Time.Clock (UTCTime)
import           Data.Default

-------------------------------------------------------------------------------
-- | Types
  
data Cookie = Cookie {
  cookieKey :: String,
  cookieValue :: String,
  cookiePath :: Maybe String,
  cookieDomain :: Maybe String,
  cookieExpires :: Maybe UTCTime,
  cookieSecure :: Bool,
  cookieHttpOnly :: Bool
}

instance Default Cookie where
  def = Cookie "" "" Nothing Nothing Nothing False False

instance Show Cookie where
  show = serializeCookie
  
instance Show [Cookie] where
  show = serializeCookies
  
instance Eq Cookie where
  (==) a b = (cookieKey a) == (cookieKey b)

-------------------------------------------------------------------------------
-- | Utilities

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe e = case e of
  Left _ -> Nothing
  Right r -> Just r

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
-- | These functions are designed for use with the Set-Cookie header

serializeCookie :: Cookie -> String
serializeCookie (Cookie k v path domain expires secure httponly) = 
  k
  ++ "="
  ++ v
  ++ (maybe "" (((++) "; expires=") . formatCookieDate) expires)
  ++ (maybe "" ((++) "; domain=") domain)
  ++ (maybe "" ((++) "; path=") path)
  ++ (if secure then "; Secure" else "")
  ++ (if httponly then "; HttpOnly" else "")
  
parseCookie :: String -> Maybe Cookie
parseCookie = eitherToMaybe . (parse cookieParser "")

-------------------------------------------------------------------------------
-- | Multiple Cookie Parsing
-- | These functions are designed for use with the Cookie header            
                                                   
serializeCookies :: [Cookie] -> String
serializeCookies cookies = intercalate "; " pairs
  where
    pairs = map (\(Cookie k v _ _ _ _ _) -> k ++ "=" ++ v) cookies

parseCookies :: String -> Maybe [Cookie]
parseCookies = eitherToMaybe . (parse cookiesParser "")

-------------------------------------------------------------------------------
-- | Parsers

type Parser a = Parsec String () a

cookiesParser :: Parser [Cookie]
-- cookiesParser = (:)
--                 <$> cookieParser
--                 <*> (maybe [] id (optional $ cookiesParser))
cookiesParser = do
  cookie <- optionMaybe cookieParser
  case cookie of
    Nothing -> return []
    Just c -> ((:) c) <$> cookiesParser

-- cookieParser :: Parser Cookie
cookieParser :: Parser Cookie
cookieParser = do
  key <- many1 alphaNum
  char '='
  value <- many alphaNum
  
  expires <- option Nothing $ do
    char ';'
    skipMany space
    string "expires="
    parseCookieDate <$> many1 anyToken

  domain <- optionMaybe $ do
    char ';'
    skipMany space
    string "domain="
    many alphaNum
    
  path <- optionMaybe $ do
    char ';'
    skipMany space
    string "path="
    many alphaNum
    
  secure <- option False $ do
    char ';'
    skipMany space
    (((==) "secure") . (map toLower)) <$> many1 letter
    
  httponly <- option False $ do
    char ';'
    skipMany space
    (((==) "httponly") . (map toLower)) <$> many1 letter
  
  return $ Cookie key value path domain expires secure httponly