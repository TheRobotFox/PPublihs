{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
-- | Modules

module Module (runModule, getModules, generateDefaultModules) where
import Data.Map (Map)
import Files (Checksum)
import Control.Exception (Exception)
import Data.Aeson
import GHC.Generics (Generic)
import Track (Track)
import Control.Monad.Trans.Reader (ReaderT)
import Module.Env
import Module.Cache

data ModuleState = ModuleState{cache :: Map [String] (FilePath, Checksum), prevCfg :: ModuleConfig, prevTrkList :: Map String (Track Checksum)}
  deriving Generic

instance FromJSON ModuleState
instance ToJSON ModuleState

data ModuleExecExpection = CacheError String | ModuleConfigError String deriving (Show, Typeable)
instance Exception ModuleExecExpection

run :: ReaderT Env IO [CacheEntry] -> ReaderT ModuleState IO (ModuleState, ())
run f = do

  cfg <- lift $ do
    path <- getXdgDirectory XdgConfig . (appName </> "modules" </> modName)
    return $ tryLoad path >>= \case Just a ->return a; Nothing ->throwIO . ModuleConfigError $ "Module Config does not exist!"
  loadState <- ask

  newCache <- flip runReaderT (Env modState newCfg trkList) . sync newCfg $ render
  return $ ModuleState newCache newCfg trkList
