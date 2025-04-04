-- |

module Module.Env (Env (prevTrackList, trackList, config), ModuleConfig (..)) where
import Data.Aeson
import Render (RenderSettings)

data ModuleConfig = Folder RenderSettings | Whole RenderSettings | Custom
instance FromJSON ModuleConfig
instance ToJSON ModuleConfig

data Env = Env{prevTrackList :: Map String (Track Checksum),
               trackList :: Map String (Track Checksum),
               config :: ModuleConfig}
