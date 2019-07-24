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

main = print "Hello"
