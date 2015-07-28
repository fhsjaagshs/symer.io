module Main
(
  main
)
where

import Blog.App
import Blog.CommandLine
import Blog.State

import System.Environment
import Data.Maybe
import System.Posix.Daemonize
import System.Posix.Signals
import System.Directory

daemonized :: (IO a) -> (a -> IO ()) -> IO ()
daemonized initializer action = serviced $ CreateDaemon {
    privilegedAction = initializer,
    program = action,
    name = Just "blog",
    user = Nothing,
    group = Nothing,
    syslogOptions = [],
    pidfileDirectory = Nothing,
   }

main :: IO ()
main = do
  env <- (fromMaybe "development") <$> (lookupEnv "ENV")
  cmd <- getCommand
  case cmd of
    -- TODO: use dbpassword
    (StartCommand True port dbpassword crtfile keyfile) -> daemonized initState (startApp port env crtfile keyfile)
    (StartCommand False port dbpassword crtfile keyfile) -> initState >>= startApp port env crtfile keyfile
    (StopCommand pidFile) -> readFile pidFile >>= signalProcess sigINT . read
    (StatusCommand pidFile) -> doesFileExist pidFile >>= putStrLn . f
      where
        f True = "running"
        f False = "stopped"