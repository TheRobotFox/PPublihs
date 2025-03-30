{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Modules

module Module (runModule, getModules, generateDefaultModules) where
import System.Directory (listDirectory, createDirectoryIfMissing, getXdgDirectory, XdgDirectory (XdgConfig), removeFile)
import Data.Aeson (FromJSON, ToJSON, encode)
-- import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Generics (Generic)
import Data.Map ( Map, keys, fromList, filterWithKey, (!), toList, union)
import qualified Data.Map as Map
import Prelude hiding (lookup)
import System.FilePath (combine, (</>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Exception (throwIO, Exception, IOException, catch)
import Data.List (find, filter)
import Data.Data ( Typeable )
import Files (moveJunk, md5Str, tryLoad, createFile, Checksum (Checksum))
import Data.Function ( on )
import Data.Maybe (fromMaybe, catMaybes, isNothing, isJust)
import Track (Track (metadata, source), Metadata (..), Attr (..), File (..), trackCacheEntry, matchSource)
import Env (appName)
import Control.Monad.Trans.Reader (ReaderT(runReaderT), ask)
import Render (RenderSettings (..), Task (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad (when, filterM)


data ModuleState = ModuleState{cache :: Map String (FilePath, Checksum), config :: RenderSettings, prevTrkList :: Map String (Track Checksum)}
  deriving Generic

data Env = Env{state :: ModuleState, newconfig :: RenderSettings, trackList :: Map String (Track Checksum)}

instance FromJSON ModuleState
instance ToJSON ModuleState

data ModuleExecExpection = CacheError String | ModuleConfigError String deriving (Show, Typeable)

instance Exception ModuleExecExpection

  -- Generate Default Modues

getModules :: IO [FilePath]
getModules = do cfgDir <- getXdgDirectory XdgConfig appName
                listDirectory $ combine cfgDir "modules"


mp3mtdt :: [Metadata]
mp3mtdt = [File Cover, Attr Artist, Attr Album, Attr Year, Attr Title, Attr Genre, Attr Nr]

defaultModules :: [(String, RenderSettings)]
defaultModules = [("flac", SingleRender [File Cover] [] "flac"),
                  ("mp3", SingleRender mp3mtdt [] "mp3"),
                  ("amuse", SingleRender [] ["-a 44100"] "wav"),
                  ("full", MergedRender mp3mtdt [] "mp3"),
                  ("video", MergedRender [File Video] [] "mp4")]

generateDefaultModules :: IO ()
generateDefaultModules = do
  modDir <- getXdgDirectory XdgConfig . combine appName $ "modules"
  createDirectoryIfMissing True modDir
  let write (mod', cfg) = BSL.writeFile (combine modDir mod') . encode $ cfg

  present <- getModules
  mapM_ write $ Data.List.filter (not . (`elem` present) . fst) defaultModules

-- Match Tracks to Cached (Rendered) Files

getCached :: ReaderT Env IO [String]
getCached = do
  modCache <- fmap (cache . state) ask
  oldCfg <- fmap (config . state) ask
  newCfg <- fmap newconfig ask

  dirty <- if oldCfg /= newCfg then do
      lift . putStrLn $ "Module Config has Changed, invalidating Cache!"
      return $ keys modCache
    else
      lift . fmap (keys . Map.filter (id)) . mapM (uncurry verifyCache) $ modCache

  when (length dirty > 0) $ lift . putStrLn $ "Cleaning dirty Cache Files: " ++ show dirty

  lift . mapM_ (moveJunk . fst . (modCache!)) $ dirty -- move Invalid File to Junk
  return . Data.List.filter (not . (`elem` dirty)) . keys $ modCache

  where verifyCache path cksm = (fmap ((/= cksm) . md5Str) . BS.readFile $ path)
            `catch` \(_ :: IOException)->putStrLn ("Could not read Track from Modcache") >> return True


matchCached :: ReaderT Env IO [(Maybe String, Maybe String)]
matchCached = do
  cached   <- getCached
  trkListPrev <- fmap (prevTrkList . state) ask
  trkList     <- fmap trackList ask

  let cachedTrksSrcs = filterWithKey (const . (`elem` cached)) trkListPrev
  return $ on matchSource (toList . Map.map source) cachedTrksSrcs trkList

metadataChanged :: String -> String -> ReaderT Env IO (Maybe Task)
metadataChanged prev new = do
  supportedMtdt <- fmap (supported . newconfig) ask
  trkA          <- fmap (flip (!) prev . prevTrkList . state) ask
  trkB          <- fmap (flip (!) new . trackList) ask
  modCache         <- fmap (cache . state) ask

  let prevPath = fst $ modCache!prev

  return $ if | matches supportedMtdt trkA trkB -> Just $ UpdateMetadata prevPath new
              | matches [Attr Title, Attr Nr] trkA trkB -> Just $ Move prevPath new
              | otherwise -> Nothing

 where matches mtdt = on (/=) (filterWithKey (const . (`elem` mtdt)). metadata)


sync :: ([Task] -> IO [(String,FilePath)]) -> ReaderT Env IO (Map String (FilePath, Checksum))
sync render = do
  matches  <- matchCached
  tasks    <- fmap catMaybes . mapM (uncurry getTask) $ matches
  outputs  <- lift $ render tasks
  cacheNew <- lift . fmap fromList . mapM (sequence . fmap trackCacheEntry) $ outputs
  keep <- fmap (map (fromMaybe (error "") . fst)) . filterM (uncurry $ isunchanged) $ matches
  cacheKeep <- fmap (filterWithKey (const . (`elem` keep)) . cache . state) ask
  return $ union cacheNew cacheKeep

  where
        getTask :: Maybe String -> Maybe String -> ReaderT Env IO (Maybe Task)
        getTask (Just a) (Just b) = metadataChanged a b
        getTask Nothing (Just b) = return . Just . Render $ b
        getTask (Just a) Nothing = do
          file <- fmap (fst . flip (!) a . cache . state) ask
          lift . removeFile $ file
          return Nothing

        isunchanged (Just a) (Just b) = fmap isNothing . metadataChanged a $ b
        isunchanged _ _ = return False

runModule :: Map String (Track Checksum) -> String -> (RenderSettings -> [Task] -> IO [(String, FilePath)]) -> IO ()
runModule trkList modName render = do

  modDir <- getXdgDirectory XdgConfig . combine appName $ "modules"
  newCfg <- (tryLoad $ modDir </> modName) >>= \case Just a ->return a; Nothing ->throwIO . ModuleConfigError $ "Module Config does not exist!"
  loadState <- tryLoad . combine "cache" $ modName

  when (isNothing loadState) . putStrLn $ "Could not load previous Module State!"
  let modState = fromMaybe (ModuleState mempty newCfg mempty) loadState

  newCache <- flip runReaderT (Env modState newCfg trkList) . sync $ render newCfg
  createFile (combine "cache" modName) $ ModuleState newCache newCfg trkList
