{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Auth (AuthUser(..), checkToken, saveToken, refreshToken, deleteToken) where
  
import           Control.Monad.IO.Class
import           Control.Applicative
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import           Data.Aeson as A
import qualified Database.Redis as R

import System.Random
import System.IO.Unsafe
  
data AuthUser = AuthUser {
  username :: T.Text,
  userId :: Integer
}

instance FromJSON AuthUser where
  parseJSON (Object v) = AuthUser <$>
                         v .: "username" <*>
                         v .: "userId"
                         
instance ToJSON AuthUser where
  toJSON (AuthUser u uid) = 
    object [
      "username" .= u,
      "userId"   .= uid
    ]
    
-------------------------------------------------------------------------------
-- | Helper functions
    
randomStr :: Int -> String
randomStr len = take len $ randomRs ('a','z') $ unsafePerformIO newStdGen
   
-------------------------------------------------------------------------------
-- | Core functions

checkToken :: R.Connection -> Maybe B.ByteString -> IO (Maybe AuthUser)
checkToken _     Nothing = return Nothing
checkToken redis (Just token) = R.runRedis redis $ do
  v <- R.get token
  liftIO $ print v
  return Nothing
  -- jsonMaybe <- liftIO $ R.get token
  -- case jsonMaybe of
  --   Nothing -> return Nothing
  --   (Just json) -> return $ A.decodeStrict $ fromJust json

saveToken :: R.Connection -> AuthUser -> IO B.ByteString
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