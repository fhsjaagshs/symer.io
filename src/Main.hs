{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Blog.App
import Blog.CommandLine
import System.WebApp

import Crypto.BCrypt
import qualified Data.ByteString.Char8 as B (pack, putStrLn)

-- Setup:
-- ensure your executable is setuid & owned by root:
-- sudo chmod 4775 dist/build/blog/blog
-- sudo chown root dist/build/blog/blog

main :: IO ()
main = getCommand >>= f
  where
    f c@(StartCommand True _ _ _ _ _) = do
      daemonize "/tmp/blog.pid" $ f $ c { startCmdDaemonize = False }
    f (StartCommand False port crt key out err) = do
      redirectStdout out
      redirectStderr err
      startHTTPS app port crt key
    f StopCommand = daemonKill 4 "/tmp/blog.pid"
    f StatusCommand = daemonRunning "/tmp/blog.pid" >>= putStrLn . showStatus
    f (PasswordCommand pwd) = hashPasswordUsingPolicy (HashingPolicy 12 "$2b$") (B.pack pwd) >>= g
      where g (Just v) = B.putStrLn v
            g Nothing = return ()
    showStatus True = "running"
    showStatus False = "stopped"