{-# LANGUAGE DefaultSignatures #-}

-- | Store and Load Contexts
module Store where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import GHC.IO (FilePath)
import System.Directory
import GHC.Generics
import Control.Monad (liftM)
import qualified Data.ByteString as BS
import Control.Exception (throwIO, Handler (Handler), catches, catch, IOException, Exception)
import Data.Data (Typeable)
import Data.Map (Map)

data StoreException = ReadStoreFile String | StoreDeserialize String deriving (Show, Typeable)
instance Exception StoreException

-- instance ToJson (File,State,Context)
-- instance FromJson (File,State,Context)

-- type States = Map String State

-- loadStore::FilePath -> IO (Context, States)
-- loadContext path = do
--   exists <- doesFileExist path
--   if exists then do
--     filedata <- BS.readFile path
--     case decode filedata of
--       Left str -> throwIO $ StoreDeserialize str
--       Right ctx -> return ctx
--   else return $ Context []

-- saveContext::FilePath -> Context -> States -> IO ()
-- saveContext path ctx sts = BS.writeFile path . encode (ctx, sts)
