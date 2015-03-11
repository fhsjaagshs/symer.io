{-# LANGUAGE OverloadedStrings #-}

module Helpers where
  
import           Control.Applicative
  
import           Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TL
import           Data.Time.Clock
import           Data.Time.Lens as TLens

import qualified Text.CSS.Parse as CSS
import qualified Text.CSS.Render as CSS
import qualified Text.Jasmine as Jasmine

import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Blaze.ByteString.Builder (toLazyByteString)

import           Web.Cookie as Cookie
import           Web.Scotty as Scotty

import qualified Crypto.BCrypt as BCrypt

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
                                                                                                   
formatDate :: UTCTime -> String
formatDate d = (show $ getL month d) ++ " • " ++ (show $ getL day d) ++ " • " ++ (show $ getL year d) ++ " | " ++ (showInteger 2 $ getL hours d) ++ ":" ++ (showInteger 2 $ getL minutes d) ++  " UTC"

showInteger :: Int -> Int -> String
showInteger numPlaces integer = (replicate ((-) numPlaces $ length $ show integer) '0') ++ (show integer)

-------------------------------------------------------------------------------
--- | HTML rendering

renderTags :: [(T.Text, T.Text)] -> Html
renderTags [] = return ()
renderTags (x:xs) = do
  meta ! A.name (textValue $ fst x) ! content (textValue $ snd x)
  renderTags xs
  
renderCssLinks :: [T.Text] -> Html
renderCssLinks [] = return ()
renderCssLinks (x:xs) = do
  link ! href (textValue x) ! rel "stylesheet" ! type_ "text/css"
  renderCssLinks xs
  
renderInput :: String -> Html
renderInput kind = input ! customAttribute "autocorrect" "off" ! customAttribute "autocapitalize" "off" ! customAttribute "spellcheck" "false" ! type_ (stringValue kind)