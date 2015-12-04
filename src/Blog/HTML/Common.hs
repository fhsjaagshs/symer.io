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
  -- * HTML Controls
  renderButton,
  renderCheckbox,
  renderTextField,
)
where

import Data.Maybe

import Text.Blaze.Html
import Text.Blaze.Html5 as H hiding (style, param, map)
import Text.Blaze.Html5.Attributes as A
import Prelude as P hiding (head, div, id)

-- |Render an HTML @meta@ tag.
renderMeta :: AttributeValue -- ^ The @name@ of the meta tag
           -> AttributeValue -- ^ The @content@ of the meta tag
           -> Html
{-# INLINE renderMeta #-}
renderMeta k v = meta ! name k ! content v

-- |Render an HTML @link@ tag representing a CSS stylesheet.
renderStylesheet :: AttributeValue -- ^ @href@ of the stylesheet
                 -> Html
{-# INLINE renderStylesheet #-}
renderStylesheet x = link
                     ! href x
                     ! rel "stylesheet"
                     ! type_ "text/css"

-- |Render an HTML @script@ tag referencing a JavaScript script.
renderScript :: AttributeValue -- ^ @href@ of the script
             -> Html
{-# INLINE renderScript #-}
renderScript scriptSrc = script ! src (toValue scriptSrc) $ ""

-- |Render a text field.
renderTextField :: Bool -- ^ whether or not the text field is "secure"
                -> AttributeValue -- ^ DOM @id@ and @name@ of text field
                -> Html
{-# INLINE renderTextField #-}
renderTextField isSecure n = f isSecure
  where
    f True  = el ! type_ "password"
    f False = el ! type_ "text"
    el = input
         ! class_ "blogtextfield"
         ! A.id n
         ! A.name n
         ! customAttribute "autocorrect" "off"
         ! customAttribute "autocapitalize" "off"
         ! customAttribute "spellcheck" "false"

-- |Render a checkbox.
renderCheckbox :: AttributeValue -- ^ DOM @id@ of the checkbox
               -> Html-- ^ content to be displayed to the right of the checkbox
               -> Bool -- ^ whether or not the checkbox is checked
               -> Html
{-# INLINE renderCheckbox #-}
renderCheckbox box h isChecked = do
  H.label ! customAttribute "for" box $ do
    checkbox isChecked
    h
  where
    checkbox True  = el ! A.checked ""
    checkbox False = el
    el = input ! type_ "checkbox" ! id box

-- |Render a button.
renderButton :: Html -- ^ text to be displayed in the button
             -> AttributeValue -- ^ DOM @id@ of the button
             -> Maybe AttributeValue -- ^ DOM @href@ of the button
             -> Html
{-# INLINE renderButton #-}
renderButton t bid h = a ! class_ "blogbutton"
                         ! rel "nofollow"
                         ! id bid
                         ! maybe mempty href h
                         $ t