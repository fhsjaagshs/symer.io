{-# LANGUAGE OverloadedStrings #-}

module Auth (AuthUser(..), checkToken, saveToken, refreshToken, deleteToken) where
  
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import           Data.Aeson as A
import qualified Database.Redis as R

import System.Random
import System.IO.Unsafe
  
AuthUser = AuthUser {
  username :: T.Text,
  userId :: Integer
}

instance FromJSON AuthUser where
  parseJSON (Object v) = AuthUser <$>
                         v .: "username" <*>
                         v .: "userId"
                         
instance ToJSON AuthUser where
  toJSON (AuthUser username userId) = 
    object [
      "username" .= username,
      "userId"   .= userId
    ]
    
-------------------------------------------------------------------------------
-- | Helper functions
    
randomStr :: Integer -> String
randomStr len = take len $ randomRs ('a','z') $ unsafePerformIO newStdGen
   
-------------------------------------------------------------------------------
-- | Core functions

checkToken :: R.Connection -> Maybe B.ByteString -> IO (Maybe AuthUser)
checkToken _     Nothing = return Nothing
checkToken redis (Just token) = R.runRedis redis $ do
  json <- R.get token
  return $ A.decode json

saveToken :: R.Connection -> AuthUser -> IO B.ByteString
saveToken redis user = R.runRedis redis $ do
  let token = B.pack $ randomStr 10
  R.set token (A.encode user)
  return token
  
refreshToken :: R.Connection -> B.ByteString -> IO ()
refreshToken _     Nothing = return ()
refreshToken redis token = R.runRedis redis $ do
  R.expire token (3600*24)