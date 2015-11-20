module Main
(
  main
)
where

import Blog.App -- contains @app@ function used in startApp
import Blog.CommandLine
import Blog.State
import Blog.System.Daemon
import Blog.System.IO
import Blog.System.HTTP

-- Setup:
-- ensure your executable is setuid & owned by root:
-- sudo chmod 4775 dist/build/blog/blog
-- sudo chown root dist/build/blog/blog

main :: IO ()
main = getCommand >>= f
  where
    f c@(StartCommand True _ _ _ _ _ _ _ _) = do
      daemonize "/tmp/blog.pid" $ f $ c { startCmdDaemonize = False }
    f (StartCommand False port crt key pgrootca pgcrt pgkey out err) = do
      redirectStdout out
      redirectStderr err
      startHTTPS app (initState pgrootca pgcrt pgkey) crt key port
    f StopCommand = daemonKill 4 "/tmp/blog.pid"
    f StatusCommand = daemonRunning "/tmp/blog.pid" >>= putStrLn . showStatus
    showStatus True = "running"
    showStatus False = "stopped"