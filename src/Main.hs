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
    -- TODO: use dbpassword
    (StartCommand True port dbpassword crtfile keyfile outp errp) -> do
      daemonize outp errp $ initState >>= startApp port crtfile keyfile
    (StartCommand False port dbpassword crtfile keyfile outp errp) -> do
      redirectIO outp errp
      (initState >>= startApp port crtfile keyfile)
    StopCommand -> daemonizeKill
    StatusCommand -> daemonizeStatus