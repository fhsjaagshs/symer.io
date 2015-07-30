module Main
(
  main
)
where

import Blog.App
import Blog.CommandLine
import Blog.Daemonize

main :: IO ()
main = do
  cmd <- getCommand
  case cmd of
    (StartCommand True port dbpass crtfile keyfile outp errp) -> do
      daemonize outp errp $ initState dbpass >>= startApp port crtfile keyfile
    (StartCommand False port dbpass crtfile keyfile outp errp) -> do
      redirectIO outp errp
      (initState dbpass >>= startApp port crtfile keyfile)
    StopCommand -> daemonizeKill
    StatusCommand -> daemonizeStatus