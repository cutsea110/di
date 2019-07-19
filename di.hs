{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (read)
import qualified Data.ByteString as BS
import Data.IORef

type Key   = BS.ByteString
type Value = BS.ByteString

-- | interface for simple KVS.
class DataStore a where
  create :: Key -> Value -> a -> IO ()
  read   :: Key          -> a -> IO (Maybe Value)
  update :: Key -> Value -> a -> IO ()
  delete :: Key          -> a -> IO ()

-- | update when the Key already exists, the other case insert it.
upsert :: DataStore a => Key -> Value -> a -> IO ()
upsert k v ds = do
  peek <- read k ds
  case peek of
    Just _  -> update k v ds
    Nothing -> create k v ds

-------------------------------

-- | simple implementation of KVS by using IORef
newtype IORefImpl = IORefImpl (IORef [(Key, Value)])

updateList :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
updateList k v = map (\r@(k', _) -> if k' == k then (k, v) else r)

deleteList :: Eq k => k -> [(k, v)] -> [(k, v)]
deleteList k = filter (\(k', _) -> k' /= k)

instance DataStore IORefImpl where
  create k v (IORefImpl ref) = modifyIORef ref ((k, v):)
  read   k   (IORefImpl ref) = lookup k <$> readIORef ref
  update k v (IORefImpl ref) = modifyIORef ref (updateList k v)
  delete k   (IORefImpl ref) = modifyIORef ref (deleteList k)

-- | implementation for Mock which has no action
data MockImpl = MockImpl

instance DataStore MockImpl where
  create k v _ = BS.putStrLn $ BS.concat ["Create: ", k, ", ", v]
  read   k   _ = Nothing <$ (BS.putStrLn $ BS.concat ["Read: ", k])
  update k v _ = BS.putStrLn $ BS.concat ["Update: ", k, ", ", v]
  delete k   _ = BS.putStrLn $ BS.concat ["Delete: ", k]

testMockImpl = upsert "key" "value" MockImpl
testIORefImpl = do
  ref <- newIORef ([] :: [(Key, Value)])
  upsert "key" "value" (IORefImpl ref)
  kvs <- readIORef ref
  print kvs

main :: IO ()
main = do
  print "testing MockImpl ... "
  testMockImpl
  print "testing IORefImpl ... "
  testIORefImpl
