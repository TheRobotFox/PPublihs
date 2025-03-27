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
import Files (Checksum, moveJunk, md5Str, tryLoad, createFile)
import Data.Function ( on )
import Data.Maybe (fromMaybe, mapMaybe)
import Track (Track (metadata, source), Metadata (..), Attr (..), File (..))
import Env (appName)
import Control.Monad (ap)
import Control.Monad.Trans.Reader (ReaderT(runReaderT))


data ModuleState = ModuleState{cache :: Map String (FilePath, Checksum), config :: RenderSettings, prevTrkList :: Map String (Track Checksum)}
  deriving Generic

data Env = Env{name :: String, state :: ModuleState, config :: RenderSettings, trackList :: Map String (Track Checksum)}

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
  cache <- cache . state . ask
  oldCfg <- config . state ask
  newCfg <- config . ask
  dirty <- if oldCfg /= newCfg then
      return $ keys cache
    else
      lift . fmap (keys . Map.filter (id)) . mapM (\(p,c)->fmap ((/= c) . md5Str) . BS.readFile $ p) $ cache

  mapM_ (moveJunk . fst . (cache!)) dirty -- move Invalid File tto Junk
  return . Data.List.filter (not . (`elem` dirty)) . keys $ cache

matchSource :: [(String, Checksum)] -> [(String, Checksum)] -> [(Maybe String, Maybe String)]
matchSource [] x =  map ((,) Nothing . Just . fst) x
matchSource x [] =  map (flip (,) Nothing . Just . fst) x
matchSource prev (x:xs) = (match, Just . fst $ x) : case match of
                       Just rm -> matchSource ( Data.List.filter ((/= rm) . fst) prev) xs
                       Nothing -> matchSource prev xs
  where match = fmap fst . find (on (==) snd x) $ prev

matchCached :: ReaderT Env IO [(Maybe String, Maybe String)]
matchCached = do
  cached <- getCached
  let cachedTrksSrcs = (toList . Map.map source . filterWithKey (const . (`elem` cached)))

  trkListPrev <- prevTrkList . state . ask
  trkList <- trackList . ask
  return $ on matchSource cachedTrksSrcs trkListPrev trkList

metadataChanged :: String -> String -> ReaderT Env IO (Maybe Task)
metadataChanged prev new = do
  supportedMtdt <- supported . config . ask
  trkA <- fmap (flip (!) prev) . prevTrkList . state . ask
  trkB <- fmap (flip (!) new) . trackList . ask

  cache <- cache . state . ask
  let prevPath = fst $ cache!prev

  return $ if | matches supportedMtdt trkA trkB -> Just $ UpdateMetadata prevPath new
              | matches [Attr Title, Attr Nr] trkA trkB -> Just $ Move prevPath new
              | otherwise -> Nothing

 where matches mtdt = on (/=) (filterWithKey (const . (`elem` mtdt)). metadata)


sync :: (Task -> IO FilePath) -> [(Maybe String, Maybe String)] -> ReaderT Env IO (Map String (FilePath, Checksum))
sync render matches = fmap fromList . sequence . mapMaybe exec $ matches

  where cacheEntry file = sequence . liftA2 (,) id (md5Str . BSL.readFile)
        exec (Just a, Just b) = do
          task <- metadataChanged a b
          current <- fmap (flip (!) a) . cache . state . ask
          return . Just . (,) b . fromMaybe current . fmap (fmap cacheEntry . render) task
        exec (Nothing, Just b) = Just . (,) b . lift . fmap cacheEntry . render $ Render b
        exec (Just a, Nothing) = do
          file <- fst . fmap (flip (!) a) . cache . state . ask
          removeFile file
          return Nothing


runModule :: Map String (Track Checksum) -> (RenderSettings -> Task -> IO FilePath) -> String -> IO ()
runModule trkList render modName = do

  -- load
  modDir <- getXdgDirectory XdgConfig . combine appName $ "modules"
  newCfg <- (tryLoad $ modDir </> modName) >>= \case Just a ->return a; Nothing ->throwIO . ModuleConfigError $ "Module Config does not exist!"
  modState <- fmap (fromMaybe (ModuleState mempty newCfg mempty)) . tryLoad . combine "cache" $ modName

  flip runReaderT (Env modName modState newCfg trkList) $ do

    machted <- matchCache
    lift . putStrLn . unlines . map show $ match

  -- render

  let getCachePath a = flip ! a . cache $ modState
      run (Just a, Just b)  = render newCfg $ metadataChanged (supported . config $ modState)
                                   ((prevTrkList modState)!a) (trkList!b) (getCachePath a) b

      run (Just a, Nothing) = [Delete . getCachePath $ a]
      run (Nothing, Just a) = [Render a]
      run (Nothing, Nothing) = error "Absurd Track"
      tasks = mapMaybe getTask $ match

  putStrLn . unlines map show tasks
  rndrd <- fmap fromList . mapM (sequence . liftA2 (,) fst (uncurry render)) $ tasks
  rndrcksm <- sequence . Map.map (sequence . ap (,) (fmap md5Str . BS.readFile)) $ rndrd


  -- write new Cache
  let newCache = Map.union rndrcksm . filterWithKey (const . (`elem` cached)) . cache $ modState
  createFile (combine "cache" modName) $ ModuleState newCache newCfg trkList
