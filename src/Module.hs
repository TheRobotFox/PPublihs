{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
-- | Modules

module Module (run, getModules, generateDefaultModules, ModuleState (..)) where
import Data.Map (Map, keys)
import Files (Checksum, md5Str)
import Control.Exception (Exception, IOException, catch)
import Data.Aeson
import GHC.Generics (Generic)
import Track (TrackList)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Module.Env ( Env(Env), ModuleConfig )
import Data.Data (Typeable)
import Render (RenderSettings, Format (..))
import System.Directory (getXdgDirectory, XdgDirectory (XdgConfig), listDirectory, createDirectoryIfMissing)
import System.FilePath (combine)
import qualified Data.ByteString.Lazy as BSL
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS

data ModuleState = ModuleState{cache :: Map String (FilePath, Checksum),
                               previousConfig :: ModuleConfig,
                               previousTracks :: TrackList}
  deriving Generic

instance FromJSON ModuleState
instance ToJSON ModuleState

data ModuleExecExpection = CacheError String | ModuleConfigError String deriving (Show, Typeable)
instance Exception ModuleExecExpection


getModules :: String -> IO [FilePath]
getModules appName = do
  cfgDir <- getXdgDirectory XdgConfig appName
  listDirectory $ combine cfgDir "modules"

defaultModules :: [(String, ModuleConfig)]
defaultModules = [("flac" , Folder (Flac, []             ) 60),
                  ("mp3"  , Folder (Mp3 , []             ) 60),
                  ("amuse", Folder (Wav , [("a","44100")]) 60),
                  ("full" , Concat (Mp3 , []             )   ),
                  ("video", Concat (Mp4 , []             )   )]

generateDefaultModules ::  String ->IO ()
generateDefaultModules appName = do
  modDir <- getXdgDirectory XdgConfig . combine appName $ "modules"
  createDirectoryIfMissing True modDir
  let write (mod', cfg) = BSL.writeFile (combine modDir mod') . encode $ cfg

  present <- getModules appName
  mapM_ write $ filter (not . (`elem` present) . fst) defaultModules


-- validate cache and remove invalid Files
getCached :: ModuleConfig -> ReaderT ModuleState IO (Map String FilePath)
getCached newCfg = do
  prevCfg <- fmap previousConfig ask
  modCache <- fmap cache ask
  dirty <- if newCfg /= prevCfg then do
      lift . putStrLn $ "Module Config has Changed, invalidating Cache!"
      return $ keys modCache
    else
      lift . fmap keys . filterM (uncurry invalid) $ modCache

  when (length dirty > 0) $ lift . putStrLn $ "Cleaning dirty Cache Files: " ++ show dirty

  lift . mapM_ (moveJunk . fst . (modCache!)) $ dirty -- move Invalid File to Junk
  return . Map.map fst . Map.filter (uncurry $ const . not . (`elem` dirty)) $ modCache

  where invalid path cksm = (fmap ((/= cksm) . md5Str) . BS.readFile $ path)
            `catch` \(_ :: IOException)->putStrLn ("Could not read Track from Modcache") >> return False


getCacheEntry :: FilePath -> IO (FilePath, Checksum)
getCacheEntry = sequence . ((,) <*> fmap md5Str . BS.readFile)

run :: String -> TrackList -> ReaderT ModuleState IO ((), ModuleState)
run modName trkList = do

  newCfg <- lift $ do
    path <- getXdgDirectory XdgConfig $ appName </> "modules" </> modName
    load <- tryLoad path
    case load of
      Just a ->return a
      Nothing ->throwIO . ModuleConfigError $ "Module Config does not exist!"

  cached <- getCached newCfg
  prevTrks <- fmap previousTracks ask
  newCache <- lift . mapM getCacheEntry . runReaderT (sync newCfg) $
    Env trkList prevTrks cached modName

  return $ ((),ModuleState newCache newCfg trkList)
