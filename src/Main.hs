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
    f c@(StartCommand True _ _ _ _ _ _ _) = do
      daemonize "/tmp/blog.pid" $ f $ c { startCmdDaemonize = False }
    f (StartCommand False port crt key pgpass pgrootca outp errp) = do
      redirectStdout outp
      redirectStderr errp
      initState pgpass pgrootca >>= startApp True crt key port
    f StopCommand = daemonKill 4 "/tmp/blog.pid"
    f StatusCommand = do
      running <- daemonRunning "/tmp/blog.pid"
      if running
        then putStrLn "running"
        else putStrLn "stopped"
    
startApp :: Bool -> FilePath -> FilePath -> (Int -> AppState -> IO ())
startApp False _   _   = startHTTP app
startApp True  crt key = startHTTPS app preredirect onkill crt key
  where
    preredirect = putStrLn "starting HTTP -> HTTPS process"
    onkill = putStrLn "killing HTTP -> HTTPS process"