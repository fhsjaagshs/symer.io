module Main
(
  main
)
where

import Blog.App
import System.Environment
import Data.Maybe

-- TODO: Daemonize

main :: IO ()
main = do
  port <- (maybe 3000 read) <$> (lookupEnv "PORT") -- should be 443
  env <- (fromMaybe "development") <$> (lookupEnv "ENV")
  startApp port env