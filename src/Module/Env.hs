-- |

module Module.Env (Env (..), ModuleConfig (..)) where
import Data.Aeson
import Render (RenderSettings)

data ModuleConfig = Folder RenderSettings Float | Whole RenderSettings | Custom
instance FromJSON ModuleConfig
instance ToJSON ModuleConfig

data Env = Env{trackList :: Map String (Track String),
               previousTrackList :: Map String (Track String),
               cache :: Map String FilePath,
               moduleName :: String}
