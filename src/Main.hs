module Main
(
  main
)
where

import Blog.App
import Blog.CommandLine
import Blog.Daemonize

import Control.Monad
import Data.Maybe

-- Setup:
-- ensure your executable is setuid & owned by root:
-- sudo chmod 4775 dist/build/blog/blog
-- sudo chown root dist/build/blog/blog

main :: IO ()
main = do
  cmd <- getCommand
  case cmd of
    (StartCommand True port dbpass crtfile keyfile outp errp) -> do
      daemonize "/tmp/blog.pid" outp errp $ do
        startRedirectProcess
        initState dbpass >>= startApp port crtfile keyfile
    (StartCommand False port dbpass crtfile keyfile outp errp) -> do
      when (isJust outp) $ redirectIO outp errp
      startRedirectProcess
      (initState dbpass >>= startApp port crtfile keyfile)
    StopCommand -> daemonizeKill 4 "/tmp/blog.pid"
    StatusCommand -> daemonizeStatus "/tmp/blog.pid"
    RedirectCommand -> startRedirect