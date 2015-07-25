module Blog.Caching
(
  setCacheControl,
  cachedBody,
  rawBodyCached
)
where
  
import Blog.State

import Web.Scotty.Trans
import qualified Database.Redis as Redis

setCacheControl :: ActionT e m ()
setCacheControl = setHeader "Cache-Control" "public, max-age=3600, s-max-age=3600, no-cache, must-revalidate, proxy-revalidate" -- 1 hour

cachedBody :: B.ByteString -> IO BL.ByteString -> ActionT TL.Text WebM ()
cachedBody key valueFunc = do
  redis <- webM $ gets stateRedis
  env <- liftIO $ Helpers.safeGetEnv "ENVIRONMENT" "development"
  if env == "production"
    then do
      redisValue <- liftIO . Redis.runRedis redis . Redis.get $ key
      
      case redisValue of
        Right (Just cached) -> rawBodyCached . BL.fromStrict $ cached
        _ -> do
          v <- valueFunc
          liftIO $ Redis.runRedis redis $ do
            Redis.set key . BL.toStrict $ v
            Redis.expire key 3600
          rawBodyCached v
          
    else valueFunc >>= Scotty.raw
  
rawBodyCached :: BL.ByteString -> ActionT e m ()
rawBodyCached str = do
  let hashSum = (md5Sum str)
  setHeader "Vary" "Accept-Encoding"
  maybeinm <- Scotty.header "If-None-Match"
  case maybeinm of
    Just inm -> do
      if hashSum == (TL.encodeUtf8 inm)
        then status $ Status 304 ""
        else do
          setHeader "ETag" $ TL.decodeUtf8 hashSum
          Scotty.raw str
    Nothing -> do
      setHeader "ETag" $ TL.decodeUtf8 hashSum
      Scotty.raw str
