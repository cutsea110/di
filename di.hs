{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Prelude hiding (read)
import Control.Monad.Reader
import Control.Lens
import Data.Aeson hiding (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import qualified Database.Redis as Redis
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Network.HTTP.Conduit

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

type Price = Double

class Monad repr => BitcoinSYM repr where
  getPrice :: repr Price

saveBTCPrice :: (BitcoinSYM repr, DataStoreSYM repr) => repr ()
saveBTCPrice = do
  price <- getPrice
  upsert "BTC Price" (BC.pack $ show price)

instance BitcoinSYM IO where
  getPrice = putStrLn "Get BTC Price" >> pure 1000000.0

runMockBTC :: IO a -> IO a
runMockBTC = id

testMockBTC :: IO ()
testMockBTC = runMockBTC saveBTCPrice

type BitFlyer = ReaderT Manager IO

getBitFlyerBTCPrice :: Manager -> IO Price
getBitFlyerBTCPrice manager = do
  body <- responseBody <$> httpLbs "https://api.bitflyer.jp/v1/ticker" manager
  pure $ (decode body :: Maybe Aeson.Value) ^?! _Just . key "ltp" . _Double

instance BitcoinSYM BitFlyer where
  getPrice = ask >>= (\manager -> liftIO $ getBitFlyerBTCPrice manager)

runBitFlyer :: Manager -> BitFlyer a -> IO a
runBitFlyer manager dsl = runReaderT dsl manager

testMockDS :: IO ()
testMockDS = runMockDS $ upsert "key" "value"

testIORefDS :: IO ()
testIORefDS = do
  ref <- newIORef ([] :: [(Key, Value)])
  runIORefDS ref $ do
    upsert "key" "value"
    v <- read "key"   
    liftIO $ print v

testRedisDS :: IO ()
testRedisDS = do
  conn <- Redis.checkedConnect Redis.defaultConnectInfo
  runRedis conn $ do
    upsert "key" "value"
    v <- read "key"
    liftIO $ print v

main :: IO ()
main = do
  print "testing mockDS"
  testMockDS
  print "testing mockIORefDS"
  testIORefDS
  print "testing mockRedisDS"
  testRedisDS
  print "testing mockBTC"
  testMockBTC
