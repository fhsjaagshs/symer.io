{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Auth (getObject, saveObject, refreshObject, deleteObject) where

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

getObject :: (FromJSON a) => R.Connection -> Maybe B.ByteString -> IO (Maybe a)
getObject _     Nothing = return Nothing
getObject redis (Just token) = R.runRedis redis $ do
  v <- R.get token
  case v of
    Right (Just j) -> return $ A.decodeStrict j
    _ -> return $ Nothing

saveObject :: (ToJSON a) => R.Connection -> a -> IO B.ByteString
saveObject redis user = R.runRedis redis $ do
  let token = B.pack $ randomStr 15
  R.set token (BL.toStrict $ A.encode user)
  return token
  
refreshObject :: R.Connection -> Maybe B.ByteString -> IO ()
refreshObject _     Nothing      = return ()
refreshObject redis (Just token) = R.runRedis redis $ do
  R.expire token (3600*24)
  return ()
  
deleteObject :: R.Connection -> Maybe B.ByteString -> IO ()
deleteObject _     Nothing = return ()
deleteObject redis (Just token) = R.runRedis redis $ do
  R.del [token]
  return ()