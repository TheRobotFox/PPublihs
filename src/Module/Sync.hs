-- |

module Module.Sync where
import Module.Env
import Module.Folder

sync :: ModuleConfig -> ReaderT Env IO (Map String FilePath)
sync (Folder cfg minLength) = do

  matched <- fmap (liftA2 matchSource previousTrackList trackList) ask
