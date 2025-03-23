{-# LANGUAGE DeriveGeneric #-}
-- | FFMpeg Wrapper

module FFMpeg where
import GHC.Generics ( Generic )
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.Class
import Track (Track (..), Metadata)
import Data.Map (Map, (!))

data RenderSettings = MergedRender{ -- TODO No seperation
    supported :: [Metadata],
    filters :: [String],
    format :: String
  } | SingleRender{
    supported :: [Metadata],
    filters :: [String],
    format :: String
  } deriving (Generic, Eq)

instance FromJSON RenderSettings
instance ToJSON RenderSettings

-- concatFilter :: [File] -> String
-- concatFilter = flip (++) ":a=1[audio]"("concat=n="++ show . length)



-- render_cmd :: File -> RenderSettings -> [File] -> String
-- render_cmd cover settings tracks = "ffmpeg" ++

ffrender :: Track String -> FilePath -> RenderSettings -> IO ()
ffrender track out rcfg = do
    lift $ do
      putStrLn $ "ffmpeg -i " ++ source track ++ " -c copy -s " ++ show (metadata track) ++ " -> " ++ out ++ "; delete old"
