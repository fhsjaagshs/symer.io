{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Blog.Auth
(
  getUser,
  setUser,
  authenticate,
  deleteAuth,
  accessToken
)
where
  
import Blog.State
import Blog.Cookie

import Control.Monad (void)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Aeson as A
import qualified Database.Redis as R

import System.Random

accessToken :: ActionT e m String
accessToken = Scotty.reqHeader "Cookie" >>= f
  where
    cookieToTuple (Cookie k v _ _ _ _ _) = (k, v)
    lookupToken  = (<$>) . T.pack . lookup "token" . map cookieToTuple
    -- f = (<$>) . lookupToken . fromMaybe [] . parseCookies . TL.unpack
    -- TODO: Fixme

getUser :: ActionT TL.Text WebM (Maybe User)
getUser = do
  a@(State _ _ authMap) <- webM $ state
  atoken <- accessToken
  
  let res = lookup atoken authMap
  case res of
    (Just (user, timeout)) -> 
  webM $ puts $ a { stateAuthMap = res }
  
  
  redis <- webM $ gets stateRedis
  atoken <- accessToken
  liftIO $ R.runRedis redis $ do
    R.expire atoken (3600*24)
    R.get atoken >>= A.decodeStrict
      
setUser :: User -> ActionT TL.Text WebM ()
setUser user = do
  token <- take 15 $ randomRs ('a','z') <$> newStdGen
  redis <- webM $ gets stateRedis
  saveUser redis token user
  setTokenCookie token
  where
    cookie token = def { cookieKey = "token", cookieValue = token }
    saveUser redis token = liftIO . R.runRedis redis . R.set token . BL.toStrict . A.encode
    setTokenCookie = addHeader "Set-Cookie" . TL.pack . serializeCookie . cookie
    
deleteAuth :: ActionT TL.Text WebM ()
deleteAuth = do
  redis <- webM $ gets stateRedis
  atoken <- accessToken
  void $ R.runRedis redis $ R.del [token]

authenticate :: ActionT TL.Text WebM (Maybe User)
authenticate = do
  redis <- webM $ gets stateRedis
  maybeUser <- (getUser redis)
  case maybeUser of
    (Just user) -> return $ Just user
    _ -> do
      return Nothing
      redirect "/login?error_message=Login%20required%2E"