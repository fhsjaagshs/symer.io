{-# LANGUAGE OverloadedStrings, UnboxedTuples #-}

module Blog.Util.Minification
(
  minifyCSS,
  minifyJS
)
where

import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Blaze.ByteString.Builder as BB
import qualified Data.Text.Lazy.Builder as TB

import qualified Text.CSS.Parse as CSS (parseNestedBlocks)
import qualified Text.CSS.Render as CSS (renderNestedBlocks)
import qualified Language.JavaScript.Parser as JS (renderJS, parse)
import qualified Language.JavaScript.Process.Minify as JS

minifyCSS :: Text -> Either String TL.Text
minifyCSS = fmap (TB.toLazyText . CSS.renderNestedBlocks) . CSS.parseNestedBlocks

minifyJS :: String -> Either String BL.ByteString
minifyJS = either (Left . show) (Right . minify) . flip JS.parse ""
  where minify = BB.toLazyByteString . JS.renderJS . JS.minifyJS
