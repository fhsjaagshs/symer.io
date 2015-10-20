module Blog.Web.Assets
(
  js,
  css
)
where
  
import Blog.Util.Env
  
import qualified Text.CSS.Parse as CSS (parseNestedBlocks)
import qualified Text.CSS.Render as CSS (renderNestedBlocks)
import qualified Text.Jasmine as JS (minifym)

import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TL

import qualified Data.ByteString.Lazy.Char8 as BL

js :: FilePath -> IO (Either String BL.ByteString)
js filename = f <$> appEnvIO <*> (BL.readFile filename)
  where 
    f "production" = JS.minifym
    f _ = Right

css :: FilePath -> IO (Either String BL.ByteString)
css filename = f <$> appEnvIO <*> (BL.readFile filename)
  where
    minify = fmap (TL.toLazyText . CSS.renderNestedBlocks) . CSS.parseNestedBlocks
    f "production" = fmap TL.encodeUtf8 . minify . T.decodeUtf8 . BL.toStrict
    f _ = Right