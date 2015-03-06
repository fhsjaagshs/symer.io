{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Auth (checkToken, saveToken, refreshToken, deleteToken) where
  
import           Control.Monad.IO.Class
--import           Control.Applicative
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Aeson as A
import qualified Database.Redis as R

import System.Random
import System.IO.Unsafe

-------------------------------------------------------------------------------
-- | Helper functions
    
randomStr :: Int -> String
randomStr len = take len $ randomRs ('a','z') $ unsafePerformIO newStdGen
   
-------------------------------------------------------------------------------
-- | Core functions

checkToken :: (FromJSON a) => R.Connection -> Maybe B.ByteString -> IO (Maybe a)
checkToken _     Nothing = return Nothing
checkToken redis (Just token) = R.runRedis redis $ do
  v <- R.get token
  case v of
    Right (Just json) -> return $ A.decodeStrict json
    _ -> return $ Nothing

saveToken :: (ToJSON a) => R.Connection -> a -> IO B.ByteString
saveToken redis user = R.runRedis redis $ do
  let token = B.pack $ randomStr 10
  R.set token (BL.toStrict $ A.encode user)
  return token
  
refreshToken :: R.Connection -> B.ByteString -> IO ()
refreshToken redis token = R.runRedis redis $ do
  R.expire token (3600*24)
  return ()
  
deleteToken :: R.Connection -> B.ByteString -> IO ()
deleteToken redis token = R.runRedis redis $ do
  liftIO $ print token
  return ()