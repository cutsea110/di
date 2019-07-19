import Prelude hiding (read)
import Data.ByteString

type Key   = ByteString
type Value = ByteString

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
