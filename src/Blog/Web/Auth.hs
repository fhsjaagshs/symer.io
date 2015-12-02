{-# LANGUAGE OverloadedStrings #-}

module Blog.Web.Auth
(
  getAuthenticatedUser,
  setAuthenticatedUser,
  authenticate,
  deleteAuth,
  accessToken
)
where
  
import Blog.Postgres
import Blog.User
import Web.App.Cookie

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.HashMap.Strict as H (lookup)

import Data.Maybe
import System.Random
import Web.Scotty.Trans as Scotty
import Control.Monad.IO.Class (liftIO)

accessToken :: PostgresActionM (Maybe String)
accessToken = f <$> Scotty.header "Cookie"
  where
    f = maybe Nothing (g . parseCookie . BL.toStrict . TL.encodeUtf8)
    g = maybe Nothing (H.lookup "token" . cookiePairs)

authenticate :: PostgresActionM (Maybe User)
authenticate = getAuthenticatedUser >>= f
  where f Nothing = redirect "/login?err=Login%20required%2E"
        f v = return v

getAuthenticatedUser :: PostgresActionM (Maybe User)
getAuthenticatedUser = accessToken >>= f
  where f (Just t) = listToMaybe <$> postgresQuery sql [t]
        f Nothing = return Nothing
        sql = "SELECT u.* FROM users u, auth a WHERE a.token=? AND a.user_id=u.id LIMIT 1"

setAuthenticatedUser :: User -> PostgresActionM ()
setAuthenticatedUser (User uid _ _ _) = do
  token <- liftIO $ TL.pack . take 15 . curry randomRs 'a' 'z' <$> newStdGen
  postgresExec "INSERT INTO auth (token,user_id) VALUES (?,?)" (token, uid)
  Scotty.addHeader "Set-Cookie" $ mconcat ["token=", token, "; HttpOnly; Secure"] -- this screws up pre warp-3.1.4

deleteAuth :: PostgresActionM ()
deleteAuth = accessToken >>= f
  where f Nothing = return ()
        f (Just token) = do
          postgresExec "DELETE FROM auth WHERE token=?" [token :: String]
          Scotty.addHeader "Set-Cookie" "token=deleted; max-age=-1"