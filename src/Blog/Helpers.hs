{-# LANGUAGE OverloadedStrings #-}

module Helpers where

-- standard
-- import           Control.Applicative
-- import           Data.Maybe
import           Data.Default
import           System.Environment
import           System.IO.Unsafe

-- String types
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TL

-- Date
import           Data.Time.Format

-- minification
import qualified Text.CSS.Parse as CSS
import qualified Text.CSS.Render as CSS
import qualified Text.Jasmine as Jasmine (minify)

-- HTML
import           Text.Blaze.Html5 (Html, input, customAttribute, (!), stringValue)
import           Text.Blaze.Html5.Attributes (class_, type_)

-- Scotty
import           Web.Scotty as Scotty

-- crypto
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Digest.Pure.MD5 as MD5

-- Cookie parsing
import Cookie


unsafeGetEnv :: String -> String -> String
unsafeGetEnv k d = unsafePerformIO $ safeGetEnv k d

safeGetEnv :: String -> String -> IO String
safeGetEnv key defaultValue = maybe defaultValue Prelude.id <$> lookupEnv key

-------------------------------------------------------------------------------
-- | Gravatar

gravatarUrl :: T.Text -> String
gravatarUrl email = "https://secure.gravatar.com/avatar/"
                    ++ (show $ MD5.md5 $ TL.encodeUtf8 $ TL.fromStrict email)
                    ++ ".png?s=120&r=x&d=mm"

-------------------------------------------------------------------------------
-- | General Helpers

splitList :: (Eq a) => a -> [a] -> [[a]]
splitList _ [] = []
splitList sep lst = part : (splitList sep xs)
  where
    part = takeWhile ((/=) sep) lst
    xs = drop (1 + (length part)) lst
    
emptyMaybe :: Maybe [a] -> [a]
emptyMaybe (Just v) = v
emptyMaybe Nothing = []
    
formatDate :: FormatTime t => t -> String
formatDate = formatTime defaultTimeLocale "%-m • %-e • %-y | %l:%M %p %Z" 
    
-------------------------------------------------------------------------------
-- | Hashing

md5Sum :: BL.ByteString -> BL.ByteString
md5Sum = BL.pack . show . MD5.md5

-------------------------------------------------------------------------------
--- | Authentication

checkPassword :: T.Text -> T.Text -> Bool
checkPassword hash pass = BCrypt.validatePassword (T.encodeUtf8 hash) (T.encodeUtf8 pass)

accessToken :: ActionM (Maybe T.Text)
accessToken = do
  mheader <- Scotty.header "Cookie"
  return $ lookupToken $ emptyMaybe $ parseCookies $ emptyMaybe $ TL.unpack <$> mheader
  where
    cookieToTuple (Cookie k v _ _ _ _ _) = (k, v)
    lookupToken  = ((<$>) T.pack) . (lookup "token") . (map cookieToTuple)

setAccessToken :: T.Text -> ActionM ()
setAccessToken token = addHeader "Set-Cookie" $ TL.pack $ serializeCookie cookie
  where
    cookie = def { cookieKey = "token", cookieValue = (T.unpack token) }

-------------------------------------------------------------------------------
--- | Scotty

emptyResponse :: ActionM ()
emptyResponse = Scotty.text ""

maybeParam :: TL.Text -> ActionM (Maybe TL.Text)
maybeParam key = Scotty.params >>= return . (lookup key)

-------------------------------------------------------------------------------
--- | HTML rendering

renderInput :: String -> Html
renderInput kind = input
                   ! class_ "blogtextfield"
                   ! customAttribute "autocorrect" "off"
                   ! customAttribute "autocapitalize" "off"
                   ! customAttribute "spellcheck" "false"
                   ! type_ (stringValue kind)