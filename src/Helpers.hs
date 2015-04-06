{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import           Control.Applicative
import           Control.Exception
  
import           Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TL
import           System.Locale
import           Data.Time.Format

import qualified Text.CSS.Parse as CSS
import qualified Text.CSS.Render as CSS

import qualified Text.Jasmine as Jasmine

import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Blaze.ByteString.Builder (toLazyByteString)

import           Web.Cookie as Cookie
import           Web.Scotty as Scotty

import qualified Crypto.BCrypt as BCrypt

import           System.Environment
import           System.IO.Unsafe

import qualified Data.Digest.Pure.MD5 as MD5

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

unsafeGetEnv :: String -> String -> String
unsafeGetEnv key defaultValue = unsafePerformIO $ catchAny (getEnv key) $ \_ -> return defaultValue

safeGetEnv :: String -> String -> IO String
safeGetEnv key defaultValue = catchAny (getEnv key) $ \_ -> return defaultValue

-------------------------------------------------------------------------------
-- | Gravatar

gravatarUrl :: T.Text -> String
gravatarUrl email = "https://secure.gravatar.com/avatar/" ++ (show $ MD5.md5 $ TL.encodeUtf8 $ TL.fromStrict email) ++ ".png?s=120&r=x&d=mm"

-------------------------------------------------------------------------------
-- | Lists

splitList :: (Eq a) => a -> [a] -> [[a]]
splitList _ [] = []
splitList sep lst = [(takeWhile ((/=) sep) lst)] ++ (splitList sep $ drop 1 $ dropWhile ((/=) sep) lst)

-------------------------------------------------------------------------------
-- | Hashing

md5Sum :: BL.ByteString -> BL.ByteString
md5Sum str = BL.pack $ show $ MD5.md5 str

-------------------------------------------------------------------------------
--- | Authentication

checkPassword :: T.Text -> T.Text -> Bool
checkPassword hash passwd = BCrypt.validatePassword (T.encodeUtf8 hash) (T.encodeUtf8 passwd)

accessToken :: ActionM (Maybe T.Text)
accessToken = (Scotty.header "Cookie") >>= \v -> return $ if isNothing v then Nothing else (lookup "token" $ parseCookiesText $ BL.toStrict $ TL.encodeUtf8 $ fromJust v)

setAccessToken :: T.Text -> ActionM ()
setAccessToken token = addHeader "Set-Cookie" (TL.decodeUtf8 . toLazyByteString $ renderSetCookie def { setCookieName  = "token", setCookieValue = T.encodeUtf8 token })

-------------------------------------------------------------------------------
--- | Minification

minifyJS :: BL.ByteString -> BL.ByteString
minifyJS = Jasmine.minify

minifyCSS :: B.ByteString -> BL.ByteString
minifyCSS css = case (CSS.renderNestedBlocks <$> (CSS.parseNestedBlocks $ T.decodeUtf8 css)) of
                  Left errmsg -> error errmsg
                  Right cssbuilder -> TL.encodeUtf8 $ TL.toLazyText cssbuilder
  

-------------------------------------------------------------------------------
--- | Helpers

emptyResponse :: ActionM ()
emptyResponse = Scotty.text ""

maybeParam :: TL.Text -> ActionM (Maybe TL.Text)
maybeParam key = params >>= \ps -> return (lookup key ps)
                                                   
formatDate :: FormatTime t => t -> String
formatDate d = formatTime defaultTimeLocale "%-m • %-e • %-y | %l:%M %p %Z" d  

-------------------------------------------------------------------------------
--- | HTML rendering

renderInput :: String -> Html
renderInput kind = input ! customAttribute "autocorrect" "off" ! customAttribute "autocapitalize" "off" ! customAttribute "spellcheck" "false" ! type_ (stringValue kind)