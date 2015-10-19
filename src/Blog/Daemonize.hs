{-# OPTIONS -fno-warn-unused-do-bind #-}
module Blog.Daemonize
(
  daemonize,
  daemonizeStatus,
  daemonizeKill,
  redirectStdout,
  redirectStderr,
  redirectStdin
)
where
  
import Control.Exception
import Control.Monad hiding (forever)

import System.IO
import System.Exit
import System.Posix

-- TODO: factor out case statements

daemonizeKill :: Int -> FilePath -> IO ()
daemonizeKill timeout pidFile = do
  mpid <- pidRead pidFile
  removeLink pidFile
  case mpid of
    Nothing -> return ()
    Just pid -> do 
      islive <- pidLive pid
      when islive $ do
        signalProcess sigTERM pid
        wait timeout pid

daemonizeStatus :: FilePath -> IO ()
daemonizeStatus pidFile = fileExist pidFile >>= f where
  f False = putStrLn "stopped"
  f True = do
    mpid <- pidRead pidFile
    case mpid of
      Nothing -> putStrLn "stopped"
      Just pid -> do
        res <- pidLive pid
        if res
          then putStrLn "running"
          else putStrLn "stopped, pidfile remaining"

daemonize :: FilePath -> IO () -> IO ()
daemonize pidFile program = do
  forkProcess $ do
    createSession
    forkProcess $ do
      pidWrite pidFile
      redirectStdout $ Just "/dev/null"
      redirectStderr $ Just "/dev/null"
      redirectStdin $ Just "/dev/null"
      closeFd stdInput -- close STDIN
      installHandler sigHUP Ignore Nothing
      program
    exitImmediately ExitSuccess
  exitImmediately ExitSuccess

-- README: buffering is bad, mmmmkay?
-- These functions will disable buffering
-- it's probably fine with terminals, but NOT fine with files

redirectStdout :: Maybe FilePath -> IO ()
redirectStdout Nothing = return ()
redirectStdout (Just path) = do
  swapFd stdOutput path
  hSetBuffering stdout NoBuffering

redirectStderr :: Maybe FilePath -> IO ()
redirectStderr Nothing = return ()
redirectStderr (Just path) = do
  swapFd stdError path
  hSetBuffering stderr NoBuffering

redirectStdin :: Maybe FilePath -> IO ()
redirectStdin Nothing = return ()
redirectStdin (Just path) = do
  swapFd stdInput path
  hSetBuffering stdin NoBuffering

{- Internal -}

safeOpenFd :: FilePath -> IO Fd
safeOpenFd p = do
  exists <- fileExist p
  when (not exists) $ writeFile p ""
  fd <- openFd p ReadWrite Nothing defaultFileFlags
  setFdOption fd AppendOnWrite True
  return fd

swapFd :: Fd -> FilePath -> IO ()
swapFd old path = do
  new <- safeOpenFd path
  dupTo new old
  closeFd new

wait :: Int -> CPid -> IO ()
wait secs pid = (when <$> pidLive pid) >>= \w -> w f
  where f | secs > 0 = do
            usleep 1000000
            wait (secs-1) pid
          | otherwise = do
            putStrLn $ "force killing PID " ++ (show pid)
            signalProcess sigKILL pid

pidWrite :: FilePath -> IO ()
pidWrite pidPath = getProcessID >>= writeFile pidPath . show

pidRead :: FilePath -> IO (Maybe CPid)
pidRead pidFile = fileExist pidFile >>= f where
  f True  = fmap (Just . read) . readFile $ pidFile
  f False = return Nothing

pidLive :: CPid -> IO Bool
pidLive pid = (getProcessPriority pid >> return True) `catch` f where
  f :: IOException -> IO Bool
  f _ = return False