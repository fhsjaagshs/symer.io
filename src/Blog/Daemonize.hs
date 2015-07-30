{-# OPTIONS -fno-warn-unused-do-bind #-}
module Blog.Daemonize
(
  daemonize,
  daemonizeStatus,
  daemonizeKill
)
where
  
import Control.Exception
import Control.Monad hiding (forever)

import System.IO
import System.Environment
import System.Exit
import System.Posix
import System.Posix.Syslog (Priority(..),syslog)

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

daemonize :: Maybe String -> Maybe String -> (IO a) -> (a -> IO ()) -> IO ()
daemonize outpath errpath privilegedAction program = do
  daemonize' outpath errpath $ do
    pidWrite
    forever $ do
      v <- privilegedAction
      -- Just ud <- getUserID "daemon"
      -- Just gd <- getGroupID "daemon"
      -- setEffectiveGroupID gd
      -- setEffectiveUserID ud
      program v
    
{- Internal -}
  
daemonize' :: Maybe String -> Maybe String -> IO () -> IO () 
daemonize' outpath errpath program = do
  -- must flush or else file desc below doesn't work...
  -- http://stackoverflow.com/questions/31716551 <- mine
  -- http://stackoverflow.com/questions/5517913
  hFlush stdout
  hFlush stderr
  forkProcess $ do
    createSession
    forkProcess $ do
      redirectIO outpath errpath
      blockSignal sigHUP
      program
    exitImmediately ExitSuccess
  exitImmediately ExitSuccess
  
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

forever :: IO () -> IO ()
forever program = program `catch` restart where
  restart :: SomeException -> IO () 
  restart e = do
    syslog Error $ "unexpected exception: " ++ show e
    syslog Error "restarting in 5 seconds"
    usleep 5000000
    forever program
  
blockSignal :: Signal -> IO () 
blockSignal sig = installHandler sig Ignore Nothing >> (return ())

getGroupID :: String -> IO (Maybe GroupID)
getGroupID group = try (fmap groupID (getGroupEntryForName group)) >>= return . f
  where
    f :: Either IOException GroupID -> Maybe GroupID
    f (Left _)    = Nothing
    f (Right gid) = Just gid

getUserID :: String -> IO (Maybe UserID)
getUserID user = try (fmap userID (getUserEntryForName user)) >>= return . f
  where
    f :: Either IOException UserID -> Maybe UserID
    f (Left _)    = Nothing
    f (Right uid) = Just uid