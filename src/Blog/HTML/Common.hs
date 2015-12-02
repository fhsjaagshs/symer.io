{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Blog.HTML.Common
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : Cross-Platform

Common HTML structures that are used throughout the blog.
-}

module Blog.HTML.Common
(
  -- * HTML Primitives
  renderMeta,
  renderStylesheet,
  renderScript,
  -- * Specific HTML Primitives
  renderKeywords,
  -- * HTML Controls
  renderButton,
  renderCheckbox,
  renderTextField,
  -- * Text
  renderTitle,
  renderSubtitle
)
where

import           Data.Maybe

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL

import           Text.Blaze.Html5 as H hiding (style, param, map)
import           Text.Blaze.Html5.Attributes as A
import           Prelude as P hiding (head, div, id)

-- |Render an HTML @meta@ tag.
renderMeta :: Text -- ^ The @name@ of the meta tag
           -> Text -- ^ The @content@ of the meta tag
           -> Html
{-# INLINE renderMeta #-}
renderMeta k v = meta
                 ! name (lazyTextValue k)
                 ! content (lazyTextValue v)

-- |Render an HTML @link@ tag representing a CSS stylesheet.
renderStylesheet :: Text -- ^ @href@ of the stylesheet
                 -> Html
{-# INLINE renderStylesheet #-}
renderStylesheet x = link
                     ! href (lazyTextValue x)
                     ! rel "stylesheet"
                     ! type_ "text/css"

-- |Render an HTML @script@ tag referencing a JavaScript script.
renderScript :: Text -- ^ @href@ of the script
             -> Html
{-# INLINE renderScript #-}
renderScript scriptSrc = script ! src (lazyTextValue scriptSrc) $ ""

-- |Render a meta tag containing page keywords.
renderKeywords :: [TL.Text] -- ^ keywords to render
               -> Html
{-# INLINE renderKeywords #-}
renderKeywords = renderMeta "keywords" . TL.intercalate ", "

-- |Render a text field.
renderTextField :: Bool -- ^ whether or not the text field is "secure"
                -> String -- ^ DOM @id@ and @name@ of text field
                -> Html
{-# INLINE renderTextField #-}
renderTextField isSecure n = f isSecure
  where
    f True  = base ! type_ "password"
    f False = base ! type_ "text"
    base = input
           ! class_ "blogtextfield"
           ! A.id (stringValue n)
           ! A.name (stringValue n)
           ! customAttribute "autocorrect" "off"
           ! customAttribute "autocapitalize" "off"
           ! customAttribute "spellcheck" "false"

-- |Render a checkbox.
renderCheckbox :: Text -- ^ DOM @id@ of the checkbox
               -> Text -- ^ text to be displayed to the right of the checkbox
               -> Bool -- ^ whether or not the checkbox is checked
               -> Html
{-# INLINE renderCheckbox #-}
renderCheckbox boxId txt isChecked = do
  H.label ! customAttribute "for" (lazyTextValue boxId) $ do
    checkbox isChecked
    toHtml txt
  where
    checkbox True = base ! A.checked ""
    checkbox False = base
    base = input
           ! type_ "checkbox"
           ! id (lazyTextValue boxId)

-- |Render a button.
renderButton :: Text -- ^ text to be displayed in the button
             -> Text -- ^ DOM @id@ of the button
             -> Maybe Text -- ^ DOM @href@ of the button
             -> Html
{-# INLINE renderButton #-}
renderButton btnTitle btnId btnHref = a
                                      ! class_ "blogbutton"
                                      ! rel "nofollow"
                                      ! id (lazyTextValue btnId)
                                      ! f btnHref
                                      $ toHtml btnTitle
  where
    f (Just anHref) = href (lazyTextValue anHref)
    f Nothing = mempty

-- |Render some title text.
renderTitle :: Text -- ^ text
            -> Html
{-# INLINE renderTitle #-}
renderTitle = (h2 ! class_ "title" ! id "blog-title") . toHtml

-- |Render some subtitle text.
renderSubtitle :: Text -- ^ text
               -> Html
{-# INLINE renderSubtitle #-}
renderSubtitle = (h3 ! id "subtitle") . toHtml