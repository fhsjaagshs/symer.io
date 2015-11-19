module Blog.System.FileCache
(
  FileCache(..),
  mkFileCache,
  teardownFileCache,
  fcinsert,
  fcdelete,
  fclookup
)
where

import           System.FSNotify

import           Data.List ((\\))
import qualified Data.HashTable.IO as HT
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
-- import           Data.Int (Int32, Int64)
-- import           Data.Bits
-- import           Data.Char (ord)

import           Control.Concurrent.MVar

type HashTable k v = HT.CuckooHashTable k v

data FileCache = FileCache {
  fileCacheBasePath  :: FilePath,
  fileCacheHashTable :: MVar (HashTable ByteString ByteString),
  fileCacheFSMonitor :: WatchManager
}

mkFileCache :: FilePath -> IO FileCache
mkFileCache basePath = do
  fc <- FileCache basePath <$> (HT.new >>= newMVar) <*> startManager
  startWatching fc
  return fc
  where
    startWatching fc@(FileCache bp _ m) = watchDir m bp p (f fc)
    p = const True
    f fc@(FileCache bp _ _) (Added pth _) = B.readFile pth >>= fcinsert fc (B.pack $ pth \\ bp)
    f fc@(FileCache bp _ _) (Modified pth _) = B.readFile pth >>= fcinsert fc (B.pack $ pth \\ bp)
    f fc@(FileCache bp _ _) (Removed pth _) = fcdelete fc (B.drop (length bp) (B.pack pth))
        

-- FIXME: remove entries from hashtable????
teardownFileCache :: FileCache -> IO ()
teardownFileCache = stopManager . fileCacheFSMonitor

fcinsert :: FileCache -> ByteString -> ByteString -> IO ()
fcinsert (FileCache _ mht _) k v = withMVar mht $ \ht -> HT.insert ht k v

fcdelete :: FileCache -> ByteString -> IO ()
fcdelete (FileCache _ mht _) k = withMVar mht $ \ht -> HT.delete ht k

fclookup :: FileCache -> ByteString -> IO (Maybe ByteString)
fclookup (FileCache _ mht _) k = withMVar mht $ \ht -> HT.lookup ht k