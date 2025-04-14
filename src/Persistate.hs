-- | Persist State from Over Function

module Persistate where
import Files (tryLoad, createFile)
import Data.Maybe (isNothing)
import Control.Monad (when)
import System.Directory.Internal.Prelude (fromMaybe)
import Data.Aeson (ToJSON, FromJSON)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))

-- load/update persistent State from File
runPersistate :: (FromJSON e, ToJSON e) => FilePath -> e -> ReaderT e IO (a,e) -> IO a
runPersistate file dflt f = do

  loadState <- tryLoad file
  when (isNothing loadState) . putStrLn $ "Could not load previous Module State!"

  (res, newState) <- runReaderT f . fromMaybe dflt $ loadState
  createFile file newState
  return res
