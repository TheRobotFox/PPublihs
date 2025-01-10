{-# OPTIONS_GHC -XDeriveGeneric -XDefaultSignatures #-}

-- | PPublihs State Container
module Context where

import Data.Serialize (encode, decode, Serialize)
import Files
import GHC.IO (FilePath)
import System.Directory
import GHC.Generics
import Control.Monad (liftM)
import qualified Data.ByteString as BS
import Control.Exception (throwIO, Handler (Handler), catches, catch, IOException, Exception)
import Data.Data (Typeable)

data ContextLoadException = ReadContextFile String | ContextDeserialize String deriving (Show, Typeable)
instance Exception ContextLoadException

instance Serialize File
instance Serialize State
data Context = Context{states::[(String, State)], trackDirs::[FilePath], trackFiles::[FilePath]} deriving (Generic, Show)
instance Serialize Context

loadContext::FilePath -> IO Context
loadContext path = do
  exists <- doesFileExist path
  if exists then do
    filedata <- BS.readFile path
    case decode filedata of
      Left str -> throwIO $ ContextDeserialize str
      Right ctx -> return ctx
  else return $ Context []

saveContext::FilePath -> Context -> IO ()
saveContext path = BS.writeFile path . encode
