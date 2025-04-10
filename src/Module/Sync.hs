-- |

module Module.Sync where
import Module.Env (Env(..))
import Data.Maybe (catMaybes)
import Render (render, supported)
import Module.Env (getTrack, ModuleConfig (..))
import Control.Monad.Trans.Reader (ReaderT, ask)
import Module.Folder (getCached, bundleTracks, matchBundles, getOutput, getTask, getCache)
import Track (Track(..))
import qualified Data.Map as Map
import Control.Monad.Trans.Class (lift)


sync :: ModuleConfig -> ReaderT Env IO ([(String, FilePath)])
sync (Folder cfg minLength) = do
  trkList <- fmap trackList ask
  cachedBundles <- getCached
  newBundles <- lift . bundleTracks minLength . Map.map path $ trkList

  matched <- matchBundles cachedBundles newBundles
  tasks' <- fmap catMaybes . mapM (uncurry $ getTask (supported . fst $ cfg)) $ matched
  tasks <- mapM (sequence . fmap (mapM (getTrack trackList))) tasks'

  modName <- fmap moduleName ask
  lift . mapM_ ((uncurry $ render cfg) <*> getOutput modName . snd) $ tasks

  fmap catMaybes . mapM (uncurry getCache) $ matched
