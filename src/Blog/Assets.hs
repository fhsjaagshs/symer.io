module Blog.Assets
(
  js,
  css
)
where
  
import qualified Text.CSS.Parse as CSS
import qualified Text.CSS.Render as CSS
import qualified Text.Jasmine as Jasmine (minify)

import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TL

import qualified Data.ByteString.Lazy.Char8 as BL

import System.Environment
import Data.Maybe
  
-- TODO: Better error handling
  
js :: String -> IO (Maybe BL.ByteString)
js filename = do
  env <- fromMaybe "development" <$> lookupEnv "ENV"
  raw <- BL.readFile $ "assets/js/" ++ filename
  let f = if env == "production" then Jasmine.minify else id
  return $ Just $ f raw

css :: String -> IO (Maybe BL.ByteString)
css filename = do
  env <- fromMaybe "development" <$> lookupEnv "ENV"
  raw <- BL.readFile $ "assets/css/" ++ filename
  if env == "production" 
    then return . minify . BL.toStrict $ raw
    else return $ Just raw
  where
    minify cssdata = case CSS.renderNestedBlocks <$> (CSS.parseNestedBlocks $ T.decodeUtf8 cssdata) of
                  Left _ -> Nothing
                  Right cssbuilder -> Just $ TL.encodeUtf8 $ TL.toLazyText cssbuilder