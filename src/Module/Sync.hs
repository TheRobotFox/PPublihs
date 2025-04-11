-- |

module Module.Sync where
import Module.Env (Env(..))
import Data.Maybe (catMaybes)
import Render (render, supported, Task (..), Update (..))
import Module.Env (getTrack, ModuleConfig (..))
import Control.Monad.Trans.Reader (ReaderT, ask)
import Module.Folder (getCached, bundleTracks, matchBundles, getOutput, getTask, getCache, metadataChanged)
import Track (Track(..), sortTracks, matchSource, Metadata (..), Attr (..), File (..))
import Control.Monad.Trans.Class (lift)
import Data.Map (member, toList, (!))
import Data.Function (on)
import System.FilePath (combine)
import Control.Monad (when)
import Data.List (intersect)


sync :: ModuleConfig -> ReaderT Env IO ([(String, FilePath)])
sync (Folder cfg minLength) = do
  trkList <- fmap trackList ask
  cachedBundles <- getCached
  lift . putStrLn . show $ cachedBundles
  newBundles <- lift . bundleTracks minLength . map (fmap path) . sortTracks $ trkList
  lift . putStrLn . show $ newBundles

  matched <- matchBundles cachedBundles newBundles
  tasks <- fmap catMaybes . mapM (uncurry $ getTask (supported . fst $ cfg)) $ matched

  outputs <- mapM (uncurry renderBundle) $ tasks

  fmap catMaybes . mapM (uncurry (getCache outputs)) $ matched

  where renderBundle :: Task -> [String] -> ReaderT Env IO ([String], FilePath)
        renderBundle task bundle = do
          trks <- mapM (getTrack trackList) bundle
          name <- getOutput bundle
          output <- lift $ render cfg task trks name
          return (bundle, output)


sync (Concat cfg) = do
  trks <- fmap trackList ask
  prevtrks <- fmap previousTrackList ask

  hasChanged <- fmap or . mapM (uncurry changed) $ on matchSource (map (fmap cksm) . toList) prevtrks trks

  when hasChanged $ lift . putStrLn $ "Album Changed rerender Required!"

  c <- fmap cache ask

  if (not (member "result" c) || hasChanged) then do
    let sorted = sortTracks trks
    modName <- fmap moduleName ask
    let name = combine modName . flip (!) (Attr Album) . metadata . snd . head $ sorted
    resOut <- lift $ render cfg Render (map snd sorted) name
    return [("result", resOut)]
  else return . toList $ c


  where mtdt = Attr Nr:(intersect [File Cover, File Video] . supported . fst $ cfg)
        changed (Just a) (Just b) = fmap ((==) (Just Metadata)) . metadataChanged mtdt a $ b
        changed _ _ = return True
