-- |

module Module.Sync where
-- import Module.Env


repareCache :: ReaderT Env

sync :: Map String (Track Checksum) -> String -> (RenderSettings -> [Task] -> IO [(String, FilePath)]) -> ReaderT Env
sync trkList modName render = do

  modDir <- getXdgDirectory XdgConfig . combine appName $ "modules"
  newCfg <- (tryLoad $ modDir </> modName) >>= \case Just a ->return a; Nothing ->throwIO . ModuleConfigError $ "Module Config does not exist!"
  loadState <- tryLoad . combine "cache" $ modName

  when (isNothing loadState) . putStrLn $ "Could not load previous Module State!"
  let modState = fromMaybe (ModuleState mempty newCfg mempty) loadState

  newCache <- flip runReaderT (Env modState newCfg trkList) . sync newCfg $ render
  createFile (combine "cache" modName) $ ModuleState newCache newCfg trkList
