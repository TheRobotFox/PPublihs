{-# LANGUAGE DeriveGeneric #-}
-- |

module Module.Env (Env (..), ModuleConfig (..), getTrack) where
import Data.Aeson
import Render (RenderSettings)
import Track
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Map (Map, (!))
import GHC.Generics (Generic)

data ModuleConfig = Folder RenderSettings Float | Concat RenderSettings | Custom | None deriving Generic

instance FromJSON ModuleConfig
instance ToJSON ModuleConfig

data Env = Env{trackList :: TrackList,
               previousTrackList :: TrackList,
               cache :: Map String FilePath,
               moduleName :: String}


getTrack :: (Env -> TrackList ) -> String -> ReaderT Env IO Track
getTrack from trk = fmap (flip (!) trk . from) ask
