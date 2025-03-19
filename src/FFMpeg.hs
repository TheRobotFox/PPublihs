{-# LANGUAGE DeriveGeneric #-}
-- | FFMpeg Wrapper

module FFMpeg where
import GHC.Generics ( Generic )
import Data.Aeson (FromJSON, ToJSON)
import Files (TrackName)
import Settings (Field, Fields)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.Class

data RenderSettings = MergedRender{ -- TODO No seperation
    metadata :: [Field],
    filters :: [String],
    format :: String
  } | SingleRender{
    metadata :: [Field],
    filters :: [String],
    format :: String
  } deriving (Generic, Eq)

instance FromJSON RenderSettings
instance ToJSON RenderSettings

-- concatFilter :: [File] -> String
-- concatFilter = flip (++) ":a=1[audio]"("concat=n="++ show . length)



-- render_cmd :: File -> RenderSettings -> [File] -> String
-- render_cmd cover settings tracks = "ffmpeg" ++

ffrender :: FilePath -> TrackName -> FilePath -> RenderSettings -> ReaderT (Fields String) IO ()
ffrender source trk out rcfg = do
    fields <- ask
    lift $ do
      putStrLn $ "ffmpeg -i " ++ source ++ " -c copy -s " ++ show fields ++ " -> " ++ out ++ "; delete old"
