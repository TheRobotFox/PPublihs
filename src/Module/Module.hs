{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
-- | Modules

module Module (runModule, getModules, generateDefaultModules) where
import Data.Map (Map, filterWithKey, mapWithKey)
import Files (Checksum)
import Control.Exception (Exception)
import Data.Aeson
import GHC.Generics (Generic)
import Track (Track, matchSource)
import Control.Monad.Trans.Reader (ReaderT)
import Module.Env
import Persistate (runPersistate)
import GHC.IO (FilePath)

data ModuleState = ModuleState{cache :: Map String (FilePath, Checksum),
                               previousConfig :: ModuleConfig,
                               previousTracks :: Map String (Track String)}
  deriving Generic

instance FromJSON ModuleState
instance ToJSON ModuleState

data ModuleExecExpection = CacheError String | ModuleConfigError String deriving (Show, Typeable)
instance Exception ModuleExecExpection

-- validate cache and remove invalid Files
getCached :: ModuleConfig -> ReaderT ModuleState IO [String]
getCached newCfg = do

  dirty <- if newCfg /= config ask then do
      lift . putStrLn $ "Module Config has Changed, invalidating Cache!"
      return $ keys modCache
    else
      lift . fmap (keys . filter (not . valid)) $ modCache

  when (length dirty > 0) $ lift . putStrLn $ "Cleaning dirty Cache Files: " ++ show dirty

  lift . mapM_ (moveJunk . fst . (modCache!)) $ dirty -- move Invalid File to Junk
  return . map snd . filterWithKey (const . not . (`elem` dirty)) $ modCache

  where valid path cksm = (fmap ((== cksm) . md5Str) . BS.readFile $ path)
            `catch` \(_ :: IOException)->putStrLn ("Could not read Track from Modcache") >> return False


getCacheEntry :: FilePath -> IO CacheEntry
getCacheEntry = sequence $ liftA2 (,) <*> fmap md5Str . BS.readFile

run :: String -> Map String (Track Checksum) -> ReaderT ModuleState IO (ModuleState, ())
run modName trkList = do

  newCfg <- lift $ do
    path <- getXdgDirectory XdgConfig . (appName </> "modules" </> modName)
    return $ tryLoad path >>= \case Just a ->return a; Nothing ->throwIO . ModuleConfigError $ "Module Config does not exist!"

  cached <- getCached newCfg
  newCache <- map getCacheEntry . runReaderT (sync newCfg) $
    Env trkList (previousTracks ask) cached modName

  return $ ModuleState newCache newCfg
