{-# LANGUAGE DeriveGeneric #-}
-- | Modules

module Module (runModule, getModules, generateDefaultModules) where
import FFMpeg (RenderSettings (..))
import System.Directory (listDirectory, doesFileExist, renameFile, removeFile, createDirectoryIfMissing)
import Data.Aeson (eitherDecode, FromJSON, ToJSON, encode)
-- import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Generics (Generic)
import Data.Map ( lookup, Map, keys, toList, union, fromList )
import Prelude hiding (lookup)
import System.FilePath (combine, (</>), replaceExtension)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Exception (throwIO, Exception)
import Data.List (elemIndices, find, findIndex)
import Control.Monad (filterM, msum, ap)
import Data.Data ( Typeable )
import Files (Checksum, moveJunk, md5Str)
import Data.Function ( on )
import Control.Monad.Trans.Reader (ReaderT(runReaderT), ask)
import Control.Monad.Trans.Class
import Data.Maybe (fromMaybe)
import Track (Track)


data ModuleState = ModuleState{rendered :: Map String (FilePath, Checksum), config :: RenderSettings, last :: Map String (Track Checksum)}
  deriving Generic

instance FromJSON ModuleState
instance ToJSON ModuleState

data ModuleExecExpection = CacheError String | ModuleConfigError String deriving (Show, Typeable)

instance Exception ModuleExecExpection

  -- Generate Default Modues

getModules :: IO [FilePath]
getModules = do cfgDir <- configDir
                listDirectory $ combine cfgDir "modules"


mp3mtdt :: [Field]
mp3mtdt = [File "cover", Attr "title", Attr "author", Attr "album", Attr "year", Attr "track", Attr "genre"]

defaultModules :: [(String, RenderSettings)]
defaultModules = [("flac", SingleRender [File "cover"] [] "flac"),
                  ("mp3", SingleRender mp3mtdt [] "mp3"),
                  ("amuse", SingleRender [] ["-a 44100"] "wav"),
                  ("full", MergedRender mp3mtdt [] "mp3"),
                  ("video", MergedRender [File "video"] [] "mp4")]

generateDefaultModules :: IO ()
generateDefaultModules = do
  confDir <- configDir
  let modDir  = combine confDir "modules"
  createDirectoryIfMissing True modDir
  let write (mod, cfg) = BSL.writeFile (combine modDir mod) . encodePretty $ cfg

  present <- getModules
  mapM_ write $ filter (not . (`elem` present) . fst) defaultModules

-- Module props

readConfig :: String -> IO RenderSettings
readConfig name = do
  cfgDir <- configDir
  exists <- doesFileExist (cfgDir </> "modules" </> name)
  if exists then do
    cfg <- BSL.readFile (cfgDir </> "modules" </> name)
    case eitherDecode cfg of
      Right r -> return r
      Left err -> throwIO $ ModuleConfigError err
  else
    throwIO $ ModuleConfigError "Does not exist"


moduleOutput :: String -> String -> [TrackName] -> TrackName -> FilePath
moduleOutput modName fmt order tn@(TrackName track) = modName </> (show . head . (elemIndices tn) $ order) ++ "." ++ replaceExtension track fmt


-- Manage Cache

cleanupCache :: (TrackName -> FilePath) -> [TrackName] -> IO ()
cleanupCache outPath = mapM_ (moveJunk . outPath)

filesChanged :: (TrackName -> FilePath) -> Map TrackName Checksum -> IO [TrackName]
filesChanged outPath = fmap (map fst) . filterM (liftA2 (fmap) ((/=) . snd) (fmap md5Str . BS.readFile . outPath . fst)) . toList



-- Matchback Tracks to Cache

matchTrack :: [(TrackName, Checksum)] -> (TrackName, Checksum) -> Maybe TrackName
matchTrack trackList track = fmap fst . msum $ [matchBy fst, matchBy snd]
        where matchBy fn = find (on (==) fn track) trackList


matchback :: [(TrackName, Checksum)] -> [(TrackName, Checksum)] -> [(Maybe TrackName, Maybe TrackName)]
matchback [] x =  map ((,) Nothing . Just . fst) x
matchback x [] =  map (flip (,) Nothing . Just . fst) x
matchback prev (x:xs) = let match = matchTrack prev x in
  (match, Just . fst $ x) : case match of
                       Just rm -> matchback ( filter ((/= rm) . fst) prev) xs
                       Nothing -> matchback prev xs

trackChanges :: [(TrackName, Checksum)] -> [(TrackName, Checksum)] -> [(Maybe TrackName, Maybe TrackName)]
trackChanges prev now = filter (not . eq) . matchback prev $ now
  where eq (a,b) = a==b && (((== a) . Just . fst) `findIndex` prev) == (((== b) . Just . fst) `findIndex` now) -- filter unchanged Tracks


data RenderInfo = RenderInfo{prevOutput :: TrackName->FilePath,
                             newOutput :: TrackName->FilePath,
                             renderSettings :: RenderSettings,
                             rerender :: TrackName -> ReaderT (Fields String) IO (),
                             validatemetadata :: [Field] -> Bool}

execute :: RenderInfo -> (Maybe TrackName, Maybe TrackName) -> ReaderT (Fields String) IO ()
execute (RenderInfo pO nO cfg _ mtdt) (Just from, Just to) = if mtdt . FFMpeg.metadata $ cfg then
    lift $ renameFile (pO from) (nO to)
  else do
    fields <- ask
    lift $ do
      putStrLn $ "ffmpeg -i " ++ pO from ++ " -c copy -s " ++ show fields ++ " -> " ++ nO to ++ "; delete old"
execute cfg (Just from, Nothing) = lift . removeFile . prevOutput cfg $ from
execute cfg (Nothing, Just to) = rerender cfg $ to



runModule :: Env -> String -> IO ()
runModule env modName = do

  let prevState = state env

  -- load
  newCfg <- readConfig modName
  let cacheFile = (combine "cache" modName)
  modState <- loadOrCreate cacheFile $ return . ModuleState mempty newCfg $ LocalState [] (Env.metadata prevState)

  let (prevOut, newOut) = on (,) (moduleOutput modName (format . config $ modState) . map fst . tracks) (current modState) prevState
      matched = on trackChanges tracks (current modState) prevState

  -- validate cache Files
  dirty <- if newCfg/=config modState then
    return . keys . rendered $ modState -- invalidate whole cache if render Settigns changed
  else
    filesChanged prevOut (rendered modState)

  cleanupCache prevOut dirty
  let cacheMatch = map (fmap $ \t->case t of
        Just x -> if x `elem` dirty then Nothing else Just x
        Nothing -> Nothing) $ matched

  -- render
  putStrLn $ unlines . map show $ cacheMatch
  let renderInfo = (RenderInfo prevOut newOut newCfg (flip ((render env) <*> newOut) newCfg) (metadataValid (current modState) prevState))

  mapM_ (liftA2 (runReaderT) (fromMaybe mempty . fmap (getMetadata prevState) . snd) (execute renderInfo)) cacheMatch

  -- write new Cache
  rndrcksm <- mapM (sequence . ap (,) (fmap md5Str . BS.readFile . newOut)) . map fst . tracks $ prevState
  BSL.writeFile cacheFile . encode $ ModuleState (union (fromList rndrcksm) . rendered $ modState) newCfg prevState
