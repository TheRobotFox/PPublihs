{-# LANGUAGE DeriveGeneric #-}
-- | Modules

module Module where
import Files ( Checksum, TrackName(..), readMD5, moveJunk )
import FFMpeg (RenderSettings (..))
import Settings (configDir)
import System.Directory (listDirectory, doesFileExist, renameFile)
import Data.Aeson (eitherDecode, FromJSON, ToJSON)
import GHC.Generics (Generic)
import State (LocalState (..), emptyState)
import Diff (Event (..), TrackEvent (..), getEvents)
import Data.Map ( Map, toList, fromList, insert, findWithDefault )
import Prelude hiding (lookup)
import System.FilePath (combine, (</>), replaceExtension)
import qualified Data.ByteString.Lazy as BS
import Control.Exception (throwIO, Exception)
import Data.List (elemIndices)
import Control.Monad (msum, filterM, liftM2)
import Data.Data
import Data.Maybe (fromMaybe)

data ModuleState = ModuleState{cacheChecksums :: [(TrackName, Checksum)], config :: RenderSettings, current :: LocalState}
  deriving Generic

instance FromJSON ModuleState
instance ToJSON ModuleState

data ModuleExecExpection = CacheError String | ModuleConfigError String deriving (Show, Typeable)

instance Exception ModuleExecExpection

  -- TODO create Folder if missing
  -- Generate Default Modues


-- defaultModules :: [String]
-- defaultModules = ["flac", "mp3", "amuse", "full", "video"]

-- generateDefault :: FilePath -> IO ()
-- generateDefault "flac" = return
-- generateDefault "mp3" = return
-- generateDefault "amuse" = return
-- generateDefault "full" = return
-- generateDefault "video" = return

getModules :: IO [FilePath]
getModules = do cfgDir <- configDir
                listDirectory $ combine cfgDir "modules"


-- Module loading


readConfig :: String -> IO RenderSettings
readConfig name = do
  cfgDir <- configDir
  exists <- doesFileExist (cfgDir </> "modules" </> name)
  if exists then do
    cfg <- BS.readFile (cfgDir </> "modules" </> name)
    case eitherDecode cfg of
      Right r -> return r
      Left err -> throwIO $ ModuleConfigError err
  else
    throwIO $ ModuleConfigError "Does not exist"

cleanState :: String -> IO ModuleState
cleanState name = do cfg <- readConfig name
                     return $ ModuleState [] cfg emptyState

loadFromCache :: String -> IO ModuleState
loadFromCache name =
  do exists <- doesFileExist cache
     if exists then do
        cached <- BS.readFile cache
        case eitherDecode cached of
          Right r -> return r
          Left err -> throwIO $ CacheError err
     else
        cleanState name
  where cache = combine "cache" name

loadModule :: String -> IO ModuleState
loadModule name =
  do cached <- loadFromCache name
     cfg <- readConfig name
     if config cached /= cfg then
       return cached{cacheChecksums = []}
     else
       return cached
-- Translate Events to Tasks
data ModuleTask = MoveCached (Maybe TrackName) | Rerender | Delete -- TODO RerenderMetadata
type Tasks = Map TrackName ModuleTask

insertTask :: TrackName -> ModuleTask -> Tasks -> Tasks
insertTask trk task m = insert trk (maxTask (findWithDefault (MoveCached Nothing) trk m) task) m
  where
    maxTask Delete _ = Delete
    maxTask (MoveCached Nothing) b = b
    maxTask a _ = a

translateEvents :: RenderSettings -> [TrackName] -> [Event] -> [(TrackName, ModuleTask)]
translateEvents (SingleRender mtdt _ _) trks = toList . foldr f (fromList [])
  where
    f (Track trk evt) m = insertTask trk (case evt of
          New -> Rerender
          Removed -> Delete
          Reordered -> if mtdt then Rerender else MoveCached Nothing
          (RenameFrom prev) -> if mtdt then Rerender else MoveCached (Just prev)) m
    f AlbumNameChanged m = if mtdt then foldr ((flip insert Rerender)) m trks else m
    f CoverChanged m = if mtdt then foldr ((flip insert Rerender)) m trks else m

-- Restore interal state, if checksums don't match

-- Move changed cache Files to junk and request Rerender
repairCache :: (TrackName -> FilePath) -> [TrackName] -> IO [(TrackName, ModuleTask)]
repairCache outPath = mapM (liftM2 (>>) (moveJunk . outPath)  (return . flip (,) Rerender))


moduleOutput :: String -> String -> [TrackName] -> TrackName -> FilePath
moduleOutput modName fmt order tn@(TrackName track) = modName </> (show . head . (elemIndices tn) $ order) ++ "." ++ replaceExtension track fmt

filesChanged :: (TrackName -> FilePath) -> [(TrackName, Checksum)] -> IO [TrackName]
filesChanged outPath = fmap (map fst) . filterM (liftA2 (fmap) ((/=) . snd) (readMD5 . outPath . fst))


execute :: RenderSettings -> (TrackName -> FilePath) -> (TrackName -> FilePath) -> (TrackName, ModuleTask) -> IO ()
execute _ prevOut newOut (track, (MoveCached prev)) = renameFile (prevOut . fromMaybe track $ prev) (newOut track)
execute (SingleRender _ _ _) _ newOut (tn@(TrackName track), Rerender) = putStrLn $ "ffmpeg -i " ++ track ++ " ... "  ++ newOut tn

runModule :: String -> LocalState -> IO ()
runModule modName state = do

  modState <- loadModule modName
  newCfg <- readConfig modName

  let prevOut = moduleOutput modName (format . config $ modState) (trackOrder . current $ modState)
  cacheEvents <- repairCache prevOut =<< filesChanged prevOut (cacheChecksums modState)

  let evts = translateEvents (config modState) (trackOrder . current $ modState) $ getEvents (current modState) state
      missingTracks = map (flip (,) $ Rerender) $ filter (`elem` map fst (cacheChecksums modState)) (trackOrder . current $ modState)
      tasks = foldr (uncurry insertTask) (fromList cacheEvents) $ concat [evts, missingTracks]
      newOut = moduleOutput modName (format newCfg) (trackOrder . current $ modState)

  mapM_ (execute (config modState) prevOut newOut) (toList tasks)
