module Main
(
  main
)
where

import Blog.App
import Blog.CommandLine
import Blog.Daemonize

-- Setup:
-- ensure your executable is setuid & owned by root:
-- sudo chmod 4775 dist/build/blog/blog
-- sudo chown root dist/build/blog/blog

main :: IO ()
main = getCommand >>= f
  where
    f c@(StartCommand True _ _ _ _ _ _ _ _ _) = do
      daemonize "/tmp/blog.pid" $ f $ c { startCmdDaemonize = False }
    f (StartCommand False port crt key pgpass pgcrt pgkey pgroot outp errp) = do
      redirectStdout outp
      redirectStderr errp
      initState pgpass pgcrt pgkey pgroot >>= startApp port crt key
    f StopCommand = daemonizeKill 4 "/tmp/blog.pid"
    f StatusCommand = daemonizeStatus "/tmp/blog.pid"