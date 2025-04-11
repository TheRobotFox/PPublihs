-- |

module Module.Sync where
import Module.Env (Env(..))
import Data.Maybe (catMaybes)
import Render (render, supported, Task)
import Module.Env (getTrack, ModuleConfig (..))
import Control.Monad.Trans.Reader (ReaderT, ask)
import Module.Folder (getCached, bundleTracks, matchBundles, getOutput, getTask, getCache)
import Track (Track(..), sortTracks)
import Control.Monad.Trans.Class (lift)


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
          modName <- fmap moduleName ask

          output <- lift . render cfg task trks . getOutput modName $ trks
          return (bundle, output)
