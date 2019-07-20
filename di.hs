{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Prelude hiding (read)
import Control.Monad.Reader
import qualified Database.Redis as Redis
import qualified Data.ByteString as BS
import Data.IORef

type Key   = BS.ByteString
type Value = BS.ByteString

-- | interface for simple KVS.
class Monad repr => DataStoreSYM repr where
  create :: Key -> Value -> repr ()
  read   :: Key          -> repr (Maybe Value)
  update :: Key -> Value -> repr ()
  delete :: Key          -> repr ()

-- | update when the Key already exists, the other case insert it.
upsert :: DataStoreSYM repr => Key -> Value -> repr ()
upsert k v = do
  peek <- read k
  case peek of
    Just _  -> update k v
    Nothing -> create k v

-------------------------------

-- | implementation for Mock which has no action
instance DataStoreSYM IO where
  create k v = BS.putStrLn $ BS.concat ["Create: ", k, ", ", v]
  read   k   = Nothing <$ (BS.putStrLn $ BS.concat ["Read: ", k])
  update k v = BS.putStrLn $ BS.concat ["Update: ", k, ", ", v]
  delete k   = BS.putStrLn $ BS.concat ["Delete: ", k]

runMockDS ::  IO a -> IO a
runMockDS = id

-- | simple implementation of KVS by using IORef
type IORefDS = ReaderT (IORef [(Key, Value)]) IO

updateList :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
updateList k v = map (\r@(k', _) -> if k' == k then (k, v) else r)

deleteList :: Eq k => k -> [(k, v)] -> [(k, v)]
deleteList k = filter (\(k', _) -> k' /= k)

instance DataStoreSYM IORefDS where
  create k v = ask >>= (\ref -> liftIO $ modifyIORef ref ((k, v):))
  read   k   = ask >>= (\ref -> liftIO $ lookup k <$> readIORef ref)
  update k v = ask >>= (\ref -> liftIO $ modifyIORef ref (updateList k v))
  delete k   = ask >>= (\ref -> liftIO $ modifyIORef ref (deleteList k))

runIORefDS :: IORef [(Key, Value)] -> IORefDS a -> IO a
runIORefDS ref dsl = runReaderT dsl ref

handleError :: Show a => Either a b -> b
handleError = either (error . show) id

instance DataStoreSYM Redis.Redis where
  create k v = () <$ Redis.set k v
  read   k   = handleError <$> Redis.get k
  update k v = () <$ Redis.set k v
  delete k   = () <$ Redis.del [k]

runRedis :: Redis.Connection -> Redis.Redis a -> IO a
runRedis conn redis = Redis.runRedis conn redis

testMockDS :: IO ()
testMockDS = runMockDS $ upsert "key" "value"

testIORefDS :: IO ()
testIORefDS = do
  ref <- newIORef ([] :: [(Key, Value)])
  v <- runIORefDS ref $ do
    upsert "key" "value"
    read "key"   
  print v
  return ()

testRedisDS :: IO ()
testRedisDS = do
  conn <- Redis.checkedConnect Redis.defaultConnectInfo
  v <- runRedis conn $ do
    upsert "key" "value"
    read "key"
  print v
  return ()

main :: IO ()
main = do
  print "testing mockDS"
  testMockDS
  print "testing mockIORefDS"
  testIORefDS
  print "testing mockRedisDS"
  testRedisDS
