{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
-- | Modules

module Module (runModule, getModules, generateDefaultModules) where
import System.Directory (listDirectory, createDirectoryIfMissing, getXdgDirectory, XdgDirectory (XdgConfig), removeFile)
import Data.Aeson (FromJSON, ToJSON, encode)
-- import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Generics (Generic)
import Data.Map ( Map, keys, fromList, filterWithKey, (!), toList)
import qualified Data.Map as Map
import Prelude hiding (lookup)
import System.FilePath (combine, (</>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Exception (throwIO, Exception)
import Data.List (find, filter)
import Data.Data ( Typeable )
import Files (moveJunk, md5Str, tryLoad, createFile, Checksum)
import Data.Function ( on )
import Data.Maybe (fromMaybe, catMaybes)
import Track (Track (metadata, source), Metadata (..), Attr (..), File (..))
import Env (appName)
import Control.Monad.Trans.Reader (ReaderT(runReaderT), ask)
import Render (RenderSettings (..), Task (..))
import Control.Monad.Trans.Class (lift)


data ModuleState = ModuleState{cache :: Map String (FilePath, Checksum), config :: RenderSettings, prevTrkList :: Map String (Track Checksum)}
  deriving Generic

data Env = Env{state :: ModuleState, newconfig :: RenderSettings, trackList :: Map String (Track Checksum)}

instance FromJSON ModuleState
instance ToJSON ModuleState

data ModuleExecExpection = CacheError String | ModuleConfigError String deriving (Show, Typeable)

instance Exception ModuleExecExpection

  -- Generate Default Modues

getModules :: IO [FilePath]
getModules = do cfgDir <- getXdgDirectory XdgConfig "ppublish"
                listDirectory $ combine cfgDir "modules"


mp3mtdt :: [Metadata]
mp3mtdt = [File Cover, Attr Artist, Attr Album, Attr Year, Attr Title, Attr Genre]

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

  dirty <- if oldCfg /= newCfg then
      return $ keys modCache
    else
      lift . fmap (keys . Map.filter (id)) . mapM (\(p,c)->fmap ((/= c) . md5Str) . BS.readFile $ p) $ modCache

  lift . mapM_ (moveJunk . fst . (modCache!)) $ dirty -- move Invalid File tto Junk
  return . Data.List.filter (not . (`elem` dirty)) . keys $ modCache

matchSource :: [(String, Checksum)] -> [(String, Checksum)] -> [(Maybe String, Maybe String)]
matchSource [] x =  map ((,) Nothing . Just . fst) x
matchSource x [] =  map (flip (,) Nothing . Just . fst) x
matchSource prev (x:xs) = (match, Just . fst $ x) : case match of
                       Just rm -> matchSource ( Data.List.filter ((/= rm) . fst) prev) xs
                       Nothing -> matchSource prev xs
  where match = fmap fst . find (on (==) snd x) $ prev

matchCached :: ReaderT Env IO [(Maybe String, Maybe String)]
matchCached = do
  cached      <- getCached
  trkListPrev <- fmap (prevTrkList . state) ask
  trkList     <- fmap trackList ask

  let cachedTrksSrcs = (toList . Map.map source . filterWithKey (const . (`elem` cached)))
  return $ on matchSource cachedTrksSrcs trkListPrev trkList

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


sync :: ([Task] -> IO FilePath) -> [(Maybe String, Maybe String)] -> ReaderT Env IO (Map String (FilePath, Checksum))
sync render matches = fmap (fromList . catMaybes) . mapM (uncurry getTask) $ matches

  where cacheEntry = sequence . liftA2 (,) id (fmap md5Str . BS.readFile)

        getTask :: Maybe String -> Maybe String -> ReaderT Env IO (Maybe (String, Task))
        getTask (Just a) (Just b) = fmap sequence . sequence . ((,) <*> metadataChanged a) $ b
        getTask Nothing (Just b) = return . Just . (,) b . Render $ b
        getTask (Just a) Nothing = do
          file <- fmap (fst . flip (!) a . cache . state) ask
          lift . removeFile $ file
          return Nothing
        getTask Nothing Nothing = error "absurd Track"


runModule :: Map String (Track Checksum) -> String -> (RenderSettings -> Task -> IO FilePath) -> IO ()
runModule trkList modName render = do

  modDir <- getXdgDirectory XdgConfig . combine appName $ "modules"
  newCfg <- (tryLoad $ modDir </> modName) >>= \case Just a ->return a; Nothing ->throwIO . ModuleConfigError $ "Module Config does not exist!"
  modState <- fmap (fromMaybe (ModuleState mempty newCfg mempty)) . tryLoad . combine "cache" $ modName

  newCache <- flip runReaderT (Env modState newCfg trkList) $ matchCached >>= sync (render newCfg)
  createFile (combine "cache" modName) $ ModuleState newCache newCfg trkList
