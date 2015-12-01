{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Blog.App
import Web.App

-- Setup:
-- ensure your executable is setuid & owned by root:
-- sudo chmod 4775 dist/build/blog/blog
-- sudo chown root dist/build/blog/blog

main :: IO ()
main = webappMain app "Nathaniel Symer's blog." ""