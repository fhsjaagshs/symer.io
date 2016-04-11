{-# LANGUAGE OverloadedStrings #-}

module Blog.HTML.CSS
(
  blog,
  comments,
  editor,
  wordlist
)
where

import Data.Niagra
import Prelude hiding (not,div,span,lex)
import Data.Monoid

{- DRY -}

useFont :: (Monad m) => Builder -> Builder -> NiagraT m ()
useFont name pth = fontFace $ do
  fontFamily [name]
  -- fontWeight 700
  "src" .= "url('" <> pth <> ".eot') format('eot'),"
        <> "url('" <> pth <> ".woff2') format('woff2'),"
        <> "url('" <> pth <> ".woff') format('woff'),"
        <> "url('" <> pth <> ".ttf') format('truetype')"

{- Stylesheets -}

blog :: (Monad m) => NiagraT m ()
blog = do
  useFont "ostrich-bold" "/assets/fonts/ostrichproper/bold"
  useFont "ostrich-semibold" "/assets/fonts/ostrichproper/semibold"
  useFont "lato-regular" "/assets/fonts/lato/regular"
  
  body ? do
    textAlign  "center"
    color      (hex 0x333)
    background (hex 0xF0F0F0)
    maxWidth   (px 700)
    margin     [px 20, auto]
  
  cls "header" ? do
    not (cls "nopadding") <||> not lastChild ? do
      marginBottom (px 80)
      display      "block"
  
  a ? do
    hover  ? color (hex 0x9F8E90)
    active ? color (hex 0xBCADAF)
    color (hex 0x876E71)
    "text-decoration" .= "none"
    cls "taglink" ? do
      cls "selected-tag" ? background (v "white")
      hover ? do
        color      (hex 0x888)
        background (hex 0xddd)
      active ? do
        color      (hex 0x888)
        background (v "white")
      display      "inline-block"
      color        "gray"
      border       [em' 0.1, "solid", "white"]
      borderRadius (em 1)
      margin       (px 5)
      padding      [em' 0.4, em' 0.6]
      font         (px 16) ["lato-regular", "sans-serif"]
      
    -- cls "read-more" ? do
      -- TODO: colors
      
  code ? do
    fontFamily ["monospace"]
    
  not pre .>. code ? do
    background (hex 0xF2EDED)
    border     [px 1, "solid", hex 0xC9C6C6]
    padding    [em 0, em' 0.2]

  pre ? do
    "overflow" .= "auto"
    background (hex 0xF2EDED)
    border     [px 1, "solid", hex 0xC9C6C6]
    padding    [em' 0.1, em' 0.4]
    width      (perc 93)
    
  blockquote ? do
    p ? display "inline"
    borderLeft [px 5, "solid", hex 0xEAB05B]
    margin     [em' 1.5, px 10]
    padding    [em' 0.5, px 10]
      
  cls "title" ? do
    font   (px 50) ["ostrich-bold","sans-serif"]
    margin (px 5)
  
  cls "subtitle" ? do
    font   (px 30) ["ostrich-semibold","sans-serif"]
    margin (px 10)
    
  cls "tagline" ? do
    font   (px 20) ["ostrich-semibold","sans-serif"]
    margin (px 10)
    
  ident "name-title" ? do
    color (hex 0xb15d42)

  ident "prevbutton" ? do
    float  "left"
    margin (px 20)
    
  ident "nextbutton" ? do
    float  "right"
    margin (px 20)

  cls "post" ? do
    textAlign "left"
    margin    [px 0, px 10, px 25, px 10]
  
  cls "post-header" ? do
    marginBottom (px 10)
    position     "relative"

  cls "post-headerbox" ? do
    display       "inline-block"
    width         (perc 50)
    verticalAlign "top"

  cls "post-title" ? do
    hover ?  color (hex 0x94e1c8)
    active ? color "white"
    font   (px 40) ["ostrich-bold","sans-serif"]
    color  (hex 0x80B9B4)
    margin (px 0)
    
  cls "post-subtitle" ? do
    margin [px 10, px 0, px 0, px 0]
    font   (px 20) ["ostrich-bold","sans-serif"]
    color  (hex 0x717171)
    
  cls "post-content" ? do
    font       (pt 14) ["lato-regular","sans-serif"] -- TODO: use Lato
    lineHeight (perc 135)
    
  cls "post-edit-button" ? do
    marginTop (px 10)
    display   "block"

  cls "button" ? do
    hover ? boxShadow ["inset", px 0, px 3, "gray"]--  "box-shadow" .= "inset 0px 3px gray"
    active ? background [hex 0xC9BCA0]
    "text-decoration" .= "none"
    display    "inline-block"
    fontSize   (px 16)
    cursor     "pointer"
    padding    (px 10)
    background (hex 0xdaccae)
    color      (hex 0x876e71)
    border     (px 2)
    margin     (px 5)

  input ! "textfield" ? do
    "type" |=| "text" <> "type" |=| "password" ? do
      "transition" .= "border 0.3s"
      boxShadow    none
      font         (px 18) ["sans-serif"]
      textAlign    "center"
      display      "block"
      borderStyle  none
      borderBottom [px 2, "solid", hex 0xC9C9C9]
      padding      (px 10)
      margin       [px 20, auto]
      focus ? do
        outline      none
        borderBottom [px 2, "solid", hex 0x969696]
  
  textarea ! "textarea" ? do
    focus ? borderLeft ["solid", px 2, hex 0x969696]
    font          (pt 15) ["Helvetica","sans-serif"]
    width         (perc 95)
    height        (px 200)
    background    (v "white")
    border        none
    borderLeft    [px 2, "solid", hex 0xC9C9C9]
    padding       [px 8, px 8, px 8, px 10]
    verticalAlign "middle"
    display       "inline-block"
    outline       none
    "resize" .= "none"
    
comments :: (Monad m) => NiagraT m ()
comments = do
  cls "comment" ? do
    textAlign  "left"
    position   "relative"
    padding    (px 10)
    background (hex 0xEAEAEA)
    borderLeft [px 1, "solid", "gray"]
    after ? do -- partial bottom border
      position   "absolute"
      content    ""
      width      (px 50)
      height     (px 1)
      bottom     (px 0)
      left       (px 0)
      background (v "gray")

  div ? do
    cls "editor" ? textAlign "left"
    cls "gobutton" ? do
      active ? opacity 0.5
      display    "block"
      cursor     "pointer"
      width      (px 100)
      height     (px 40)
      background [url "/assets/images/gobutton.svg", "no-repeat"]
      margin     [px 10, auto]
      
  textarea ! "comment-textarea" ! "textarea" ? do
    focus ? borderLeft [px 2, "solid", hex 0x80B9B4]
    width  (perc 95)
    height (px 200)
    
editor :: (Monad m) => NiagraT m ()
editor = do
  useFont "lato-regular" "/assets/fonts/lato/regular"
  ident "editor" ? do
    "resize"  .= "none"
    outline    none
    border     none
    background (hex 0x3d3d3d)
    color      (hex 0xc8c8c8)
    font       (px 14) ["monospace"]
    lineHeight (perc 135)
    padding    (px 18)
    height     (px 500)
    width      (calc OpSub (perc 100) (px 36))
    
  ident "title-field" ? do
    outline      none
    textAlign    "center"
    border       none
    font         (pt 30) ["ostrich-semibold","sans-serif"]
    width        (calc OpSub (perc 100) (px 50))
    color        (hex 0x787878)
    background   (v "transparent")
    margin       [px 25, px 0]
    borderBottom [px 2, "dotted"]
    
  ident "save-button" ! "button" ? do
    color      "white"
    background (hex 0x64A58D)
    
  ident "delete-button" ! "button" ? do
    color      "white"
    background (hex 0xB15D42)

  label ? do
    display       "block"
    verticalAlign "middle"
    margin        (px 20)
    font          (px 20) ["lato-regular", "sans-serif"]
    
  input <||> "type" |=| "checkbox" ? do
    outline       none
    border        none
    borderRadius  (perc 15)
    cursor        "pointer"
    appearance    "button"
    background    (hex 0xCCC)
    width         (px 25)
    height        (px 25)
    margin        [px 0, px 5, px 0, px 0]
    verticalAlign "middle"
    checked <||> after ? do
      textAlign     "center"
      display       "block"
      verticalAlign "middle"
      content       "\\2713"
      borderRadius  (perc 15)
      background    (hex 0x65A38C)
      color         "white"
      width         (px 25)
      height        (px 25)
      fontSize      (px 20)
    after <||> hover <> hover ? background (hex 0x7DC9AC)
  
wordlist :: (Monad m) => NiagraT m ()
wordlist = do
  cls "wordlist-view" ? do
    "*" ? font (px 20) ["lato-regular","sans-serif"] -- font size dictates wordlist height
    margin   (px 30)
    maxWidth (px 700)
    width    auto
    input ? do
      verticalAlign "middle"
      width         (px 80)
      padding       (px 10)
      margin        [px 5, px 0]
      border        none
      color         (hex 0xF0F0F0)
      background    (hex 0x545454)
    
  cls "wordlist-item" ? do
    display       "inline-block"
    verticalAlign "middle"
    background    (hex 0x80B9B4)
    color         (hex 0xF0F0F0)
    padding       (px 10)
    margin        [px 5, px 10, px 5, px 0]
    span <||> after ? do
      cursor     "pointer"
      content    "\\00D7"
      marginLeft (px 5)