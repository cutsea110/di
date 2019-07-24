{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Prelude hiding (read)
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Lens
import Data.Aeson hiding (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Network.HTTP.Conduit

type Key   = BS.ByteString
type Value = BS.ByteString

data DataStoreF a = Create Key Value (() -> a)
                  | Read   Key       (Maybe Value -> a)
                  | Update Key Value (() -> a)
                  | Delete Key       (() -> a)
                  deriving Functor

type DataStoreT m = FreeT DataStoreF m

create :: MonadFree DataStoreF m => Key -> Value -> m ()
create k v = liftF $ Create k v id
read :: MonadFree DataStoreF m => Key -> m (Maybe Value)
read   k   = liftF $ Read   k   id
update :: MonadFree DataStoreF m => Key -> Value -> m ()
update k v = liftF $ Update k v id
delete :: MonadFree DataStoreF m => Key -> m ()
delete k   = liftF $ Delete k   id

upsert :: MonadFree DataStoreF m => Key -> Value -> m ()
upsert k v = do
  peek <- read k
  case peek of
    Just _  -> update k v
    Nothing -> create k v

runMockDS :: MonadIO io => DataStoreT io a -> io a
runMockDS = iterT interpret
  where
    interpret (Create k v r) = r =<< liftIO (BS.putStrLn $ BS.concat ["Create: ", k, ", ", v])
    interpret (Read   k   r) = r =<< liftIO (Nothing <$ (BS.putStrLn $ BS.concat ["Read: ", k]))
    interpret (Update k v r) = r =<< liftIO (BS.putStrLn $ BS.concat ["Update: ", k, ", ", v])
    interpret (Delete k   r) = r =<< liftIO (BS.putStrLn $ BS.concat ["Delete: ", k])

updateList :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
updateList k v = map (\r@(k', _) -> if k' == k then (k, v) else r)

deleteList :: Eq k => k -> [(k, v)] -> [(k, v)]
deleteList k = filter (\(k', _) -> k' /= k)

runIORefDS :: MonadIO io => IORef [(Key, Value)] -> DataStoreT io a -> io a
runIORefDS ref = iterT interpret
  where
    interpret (Create k v r) = r =<< liftIO (modifyIORef ref ((k, v):))
    interpret (Read   k   r) = r =<< liftIO (lookup k <$> readIORef ref)
    interpret (Update k v r) = r =<< liftIO (modifyIORef ref (updateList k v))
    interpret (Delete k   r) = r =<< liftIO (modifyIORef ref (deleteList k))

action :: (MonadFree DataStoreF m, MonadIO m) => m ()
action = do
  upsert "key" "value"
  v <- read "key"
  liftIO $ print v

testMockDS :: IO ()
testMockDS = runMockDS action

testIORefDS :: IO ()
testIORefDS = do
  ref <- newIORef ([] :: [(Key, Value)])
  runIORefDS ref action

main = do
  print "testing MockDS"
  testMockDS
  print "testing IORefDS"
  testIORefDS
