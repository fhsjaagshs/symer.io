{-# LANGUAGE OverloadedStrings #-}

module Blog.HTML.CSS
(
  blog,
  comments,
  editor
)
where

import Data.Niagra
import Data.Text.Lazy (Text)
import Prelude hiding (not,div)
import Data.Monoid

blog :: Text
blog = css' $ do
  body ? do
    "background"  .= "#F0F0F0"
    "font-family" .= "helvetica, sans-serif"
    "text-align"  .= "center"
    "color"       .= "#333"
    "max-width"   .= "700px"
    "margin"      .= "20px auto"
  
  cls "header" ? do
    not (cls "nopadding") ? do
      not lastChild ? do
        "margin-bottom" .= "80px"
        "display"       .= "block"
  
  a ? do
    hover ?  "color" .= "#9f8e90"
    active ? "color" .= "#bcadaf"
    "text-decoration" .= "none"
    "color"           .= "#876e71"
    cls "taglink" ? do
      cls "selected-tag" ? "background-color" .= "white"
      "color"         .= "gray"
      "display"       .= "inline-block"
      "margin"        .= "5px"
      "padding"       .= "0.4em 0.6em 0.4em 0.6em"
      "border-radius" .= "1em"
      "border"        .= "0.1em solid white"
      hover ? do
        "color"            .= "#888"
        "background-color" .= "#ddd"
      active ? do
        "color"            .= "#888"
        "background-color" .= "white"
    -- cls "read-more" ? do
      -- TODO: colors
      
  code ? do
    "font-family" .= "monospace"
    
  not pre .>. code ? do
    "border"     .= "1px solid #C9C6C6"
    "background" .= "#F2EDED"
    "padding"    .= "0 0.2em 0 0.2em"

  pre ? do
    "padding"    .= "0.1em 0.4em 0.1em 0.4em"
    "overflow"   .= "auto"
    "width"      .= "93%"
    "border"     .= "1px solid #C9C6C6"
    "background" .= "#F2EDED"

  blockquote ? do
    p ? "display" .= "inline"
    "border-left" .= "5px solid #eab05b"
    "margin"      .= "1.5em 10px"
    "padding"     .= "0.5em 10px"
      
  cls "title" ? do
    "font-size"   .= "50px"
    "margin"      .= "5px"
    "font-family" .= "'Oxygen',helvetica,sans-serif"
  
  cls "subtitle" ? do
    "font-size"   .= "20px"
    "margin"      .= "10px"
    "font-family" .= "'Oxygen Light',helvetica,sans-serif"
    
  cls "tagline" ? do
    "font-size"   .= "15px"
    "margin"      .= "10px"
    "font-family" .= "'Oxygen Light',helvetica,sans-serif"
    
  ident "name-title" ? do
    "color" .= "#b15d42"

  ident "prevbutton" ? do
    "float"  .= "left"
    "margin" .= "20px"
    
  ident "nextbutton" ? do
    "float"  .= "right"
    "margin" .= "20px"

  cls "post" ? do
    "text-align" .= "left"
    "margin"     .= "0px 10px 25px 10px"
  
  cls "post-header" ? do
    "margin-bottom" .= "10px"
    "position"      .= "relative"

  cls "post-headerbox" ? do
    "width"          .= "50%"
    "display"        .= "inline-block"
    "vertical-align" .= "top"

  cls "post-title" ? do
    hover ? "color" .= "#94e1c8"
    active ? "color" .= "white"
    "font-size" .= "30px"
    "font-family" .= "'Oxygen',sans-serif"
    "color" .= "#80B9B4"
    "margin" .= "0px"
    
  cls "post-subtitle" ? do
    "margin"     .= "0px"
    "margin-top" .= "10px"
    
  cls "post-content" .>. "*" ? do
    "font-size"   .= "20px"
    "line-height" .= "1.35em"
    
  cls "post-edit-button" ? do
    "margin-top" .= "10px"
    "display" .= "block"

  cls "button" ? do
    hover ? "box-shadow" .= "inset 0px 3px gray"
    active ? "background-color" .= "#c9bca0"
    "font-size"        .= "16px"
    "border"           .= "none"
    "display"          .= "inline-block"
    "cursor"           .= "pointer"
    "padding"          .= "10px"
    "background-color" .= "#daccae"
    "color"            .= "#876e71"
    "border-radius"    .= "2px"
    "margin"           .= "5px"
    "text-decoration"  .= "none"

  -- input.textfield[], input.textfield[]

  input ! "textfield" ? do
    "type" |=| "text" <> "type" |=| "password" ? do
       "display"       .= "block"
       "margin"        .= "20px auto"
       "text-align"    .= "center"
       "font-family"   .= "sans-serif"
       "font-size"     .= "18px"
       "appearance"    .= "none"
       "box-shadow"    .= "none"
       "border-radius" .= "none"
       "padding"       .= "10px"
       "border"        .= "none"
       "border-bottom" .= "solid 2px #C9C9C9"
       "transition"    .= "border 0.3s"
       "border-radius" .= "0"
       focus ? do
         "outline"       .= "none"
         "border-bottom" .= "solid 2px #969696"
  
  textarea ! "textarea" ? do
    focus ? "border-left" .= "solid 2px #969696"
    "width" .= "95%"
    "height" .= "200px"
    "padding" .= "8px 8px 8px 10px"
    "border" .= "none"
    "border-left" .= "solid 2px #C9C9C9"
    "resize" .= "none"
    "outline" .= "none"
    "font-size" .= "15pt"
    "background-color" .= "white"
    "font-family" .= "Helvetica, sans-serif"
    "display" .= "inline-block"
    "vertical-align" .= "middle"
    "appearance" .= "none"
    "border-radius" .= "0"
    
comments :: Text
comments = css' $ do
  cls "comment" ? do
    "text-align" .= "left"
    "padding" .= "10px"
    "background-color" .= "#EAEAEA"
    "position" .= "relative"
    "border-left" .= "1px solid gray"
    after ? do -- partial bottom border
      "content"    .= "''"
      "width"      .= "50px"
      "height"     .= "1px"
      "background" .= "gray"
      "position"   .= "absolute"
      "bottom"     .= "0px"
      "left"       .= "0px"
      
  cls "comment-name" ? "margin-top" .= "0px"
  
  div ? do
    cls "editor" ? "text-align" .= "left"
    cls "gobutton" ? do
      active ? "opacity" .= "0.5"
      "display"    .= "block"
      "width"      .= "100px"
      "height"     .= "40px"
      "background" .= "url('/assets/images/gobutton.svg') no-repeat"
      "margin"     .= "10px auto"
      "cursor"     .= "pointer"
      
  textarea ! "comment-textarea" ! "textarea" ? do
    focus ? "border-left" .= "solid 2px #80B9B4"
    "width"  .= "95%"
    "height" .= "200px"
    
  input ! "comment-field" ! "textfield" ? do
    focus ? "border-bottom" .= "solid 2px #80B9B4"
    "margin"  .= "10px"
    "width"   .= "40%"
    "display" .= "block"
    
editor :: Text
editor = css' $ do
  ident "editor" ? do
    "border"      .= "none"
    "background"  .= "#3d3d3d"
    "color"       .= "#c8c8c8"
    "font-family" .= "monospace"
    "font-size"   .= "14px"
    "line-height" .= "1.35em"
    "padding"     .= "18px"
    "resize"      .= "none"
    "width"       .= "calc(100% - 36px)"
    "height"      .= "500px"
    "outline"     .= "none"
    
  ident "title-field" ? do
    "font-family"      .= "'Oxygen',sans-serif"
    "width"            .= "calc(100% - 50px)"
    "text-align"       .= "center"
    "margin"           .= "25px 0px"
    "font-size"        .= "30pt"
    "border"           .= "0"
    "border-bottom"    .= "2px dotted"
    "background-color" .= "transparent"
    "outline"          .= "none"
    "color"            .= "#787878"
    
  ident "save-button" ! "button" ? do
    "color"            .= "white"
    "background-color" .= "#64A58D"
    
  ident "delete-button" ! "button" ? do
    "color"            .= "white"
    "background-color" .= "#B15D42"

  label ? do
    "display"        .= "block"
    "margin"         .= "20px"
    "font-size"      .= "20px"
    "font-family"    .= "sans-serif"
    "vertical-align" .= "middle"
    
  input <||> "type" |=| "checkbox" ? do
    "-webkit-appearance" .= "button"
    "-moz-appearance"    .= "button"
    "appearance"         .= "button"
    "border"             .= "none"
    "border-radius"      .= "15%"
    "background-color"   .= "#ccc"
    "width"              .= "25px"
    "height"             .= "25px"
    "margin"             .= "0px"
    "margin-right"       .= "5px"
    "vertical-align"     .= "middle"
    "outline"            .= "none"
    "cursor"             .= "pointer"
    checked <||> after ? do
      "border-radius"    .= "15%"
      "display"          .= "block"
      "background-color" .= "#65A38C"
      "color"            .= "white"
      "content"          .= "'\\2713'"
      "width"            .= "25px"
      "height"           .= "25px"
      "font-size"        .= "20px"
      "text-align"       .= "center"
      "vertical-align"   .= "middle"
    after <||> hover <> hover ? "background-color" .= "#7DC9AC"