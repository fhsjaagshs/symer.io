{-# OPTIONS -fno-warn-unused-do-bind #-}
module Blog.Daemonize
(
  daemonize,
  daemonize',
  detachProcess,
  detachProcess',
  daemonizeStatus,
  daemonizeKill,
  redirectIO
)
where
  
import Control.Exception
import Control.Monad hiding (forever)

import System.IO
import System.Exit
import System.Posix

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
daemonizeStatus pidFile = pidExists pidFile >>= f where
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
          
daemonize :: FilePath -> Maybe FilePath -> Maybe FilePath -> IO () -> IO ()
daemonize pidFile outpath errpath program = do
  detachProcess pidFile outpath errpath program
  exitImmediately ExitSuccess
  
daemonize' :: FilePath -> IO () -> IO ()
daemonize' pf p = daemonize pf Nothing Nothing p
  
detachProcess :: FilePath -> Maybe FilePath -> Maybe FilePath -> IO () -> IO ()
detachProcess pidFile outpath errpath program = do
  void $ forkProcess $ do
    createSession
    forkProcess $ do
      pidWrite pidFile
      redirectIO outpath errpath
      installHandler sigHUP Ignore Nothing
      program
    exitImmediately ExitSuccess
    
detachProcess' :: FilePath -> IO () -> IO ()
detachProcess' pf p = detachProcess pf Nothing Nothing p

redirectIO :: Maybe String -> Maybe String -> IO ()
redirectIO outpath errpath = do
  dnull <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
  closeFd stdInput >> dupTo dnull stdInput
  case outpath of
    Nothing -> closeFd stdOutput >> dupTo dnull stdOutput >> return ()
    Just out -> do
      fd <- safeOpenFd out
      dupTo fd stdOutput
      closeFd fd
  case errpath of
    Nothing -> closeFd stdError >> dupTo dnull stdError >> return ()
    Just err -> do
      fd <- safeOpenFd err
      dupTo fd stdError
      closeFd fd
  closeFd dnull
  
  -- buffering is bad, mmmmkay?
  -- it's probably fine with terminals, but NOT fine with files
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  
  where
    safeOpenFd p = do
      exists <- fileExist p
      when (not exists) $ writeFile p ""
      fd <- openFd p ReadWrite Nothing defaultFileFlags
      setFdOption fd AppendOnWrite True
      return fd
  
{- Internal -}

wait :: Int -> CPid -> IO ()
wait secs pid = do
  islive <- pidLive pid
  when islive $ do
    if secs > 0
      then do
        usleep 1000000
        wait (secs-1) pid
      else do
        putStrLn $ "sending sigKill to process " ++ (show pid)
        signalProcess sigKILL pid

pidWrite :: FilePath -> IO ()
pidWrite pidPath = getProcessID >>= writeFile pidPath . show

pidExists :: FilePath -> IO Bool
pidExists = fileExist

pidRead :: FilePath -> IO (Maybe CPid)
pidRead pidFile = pidExists pidFile >>= f where
  f True  = fmap (Just . read) . readFile $ pidFile
  f False = return Nothing

pidLive :: CPid -> IO Bool
pidLive pid = (getProcessPriority pid >> return True) `catch` f where
  f :: IOException -> IO Bool
  f _ = return False