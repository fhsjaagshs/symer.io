{-# OPTIONS -fno-warn-unused-do-bind #-}
module Blog.Daemonize
(
  daemonize,
  daemonizeStatus,
  daemonizeKill,
  redirectIO
)
where
  
import Control.Exception
import Control.Monad hiding (forever)

import System.Environment
import System.Exit
import System.Posix

pidPath :: IO String
pidPath = getProgName >>= \n -> return $ "/tmp/" ++ n ++ ".pid" 

pidWrite :: IO ()
pidWrite = pidPath >>= \ppath -> getProcessID >>= writeFile ppath . show

pidExists :: IO Bool
pidExists = pidPath >>= fileExist

pidRead :: IO (Maybe CPid)
pidRead = pidExists >>= f where
  f True  = pidPath >>= fmap (Just . read) . readFile
  f False = return Nothing

pidLive :: CPid -> IO Bool
pidLive pid = (getProcessPriority pid >> return True) `catch` f where
  f :: IOException -> IO Bool
  f _ = return False

daemonizeKill :: IO ()
daemonizeKill = do
  mpid <- pidRead
  pidPath >>= removeLink
  case mpid of
    Nothing -> return ()
    Just pid -> do 
      islive <- pidLive pid
      when islive $ do
        signalProcess sigTERM pid
        wait 4 pid

wait :: Int -> CPid -> IO ()
wait secs pid = do
  islive <- pidLive pid
  when islive $ do
    if secs > 0
      then do
        usleep 1000000
        wait (secs-1) pid
      else signalProcess sigKILL pid
                     
daemonizeStatus :: IO ()
daemonizeStatus = pidExists >>= f where
  f False = putStrLn "stopped"
  f True = do
    mpid <- pidRead
    case mpid of
      Nothing -> putStrLn "stopped"
      Just pid -> do
        res <- pidLive pid
        if res
          then putStrLn "running"
          else putStrLn "stopped, pidfile remaining"   
          
          
daemonize :: Maybe String -> Maybe String -> IO () -> IO ()
daemonize outpath errpath program = do
  -- http://stackoverflow.com/questions/31716551 <- mine
  -- http://stackoverflow.com/questions/5517913
  forkProcess $ do
    createSession
    forkProcess $ do
      pidWrite
      redirectIO outpath errpath
      installHandler sigHUP Ignore Nothing
      program
    exitImmediately ExitSuccess
  exitImmediately ExitSuccess

    
{- Internal -}
  
redirectIO :: Maybe String -> Maybe String -> IO ()
redirectIO outpath errpath = do
  dnull <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
  closeFd stdInput >> dupTo dnull stdInput
  case outpath of
    Nothing -> closeFd stdOutput >> dupTo dnull stdOutput >> return ()
    Just out -> do
      fd <- openFd out ReadWrite Nothing defaultFileFlags
      setFdOption fd AppendOnWrite True
      dupTo fd stdOutput
      closeFd fd
      return ()
  case errpath of
    Nothing -> closeFd stdError >> dupTo dnull stdError >> return ()
    Just err -> do
      fd <- openFd err ReadWrite Nothing defaultFileFlags
      setFdOption fd AppendOnWrite True
      dupTo fd stdError
      closeFd fd
  closeFd dnull