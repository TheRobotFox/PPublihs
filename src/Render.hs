{-# LANGUAGE DeriveGeneric #-}
-- | FFMpeg Wrapper

module Render where
import GHC.Generics ( Generic )
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.Class
import Track (Track (..), Metadata (..), Attr (..))
import Data.Map (Map, (!))
import System.FilePath (combine)
import System.Directory (removeFile)

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

data Task = Render String | UpdateMetadata FilePath String | Move FilePath String


-- concatFilter :: [File] -> String
-- concatFilter = flip (++) ":a=1[audio]"("concat=n="++ show . length)



-- render_cmd :: File -> RenderSettings -> [File] -> String
-- render_cmd cover settings tracks = "ffmpeg" ++

-- ffrender :: Track String -> FilePath -> RenderSettings -> IO FilePath
-- ffrender track out rcfg = do
--     lift $ do
--       putStrLn $ "ffmpeg -i " ++ source track ++ " -c copy -s " ++ show (metadata track) ++ " -> " ++ out ++ "; delete old"

getOutput :: FilePath -> Track String -> FilePath
getOutput moduleName (Track _ mt) = combine moduleName $ (show $ mt!(Attr Nr)) ++ ". " ++ mt!(Attr Title)

render :: Map String (Track String) -> FilePath -> RenderSettings -> Task -> IO FilePath
render trkList outDir cfg task = exec task
  where exec (Render trk) = ffmpeg (trkList!trk) outDir
        exec (UpdateMetadata from trk) = ffmpeg (trkList!trk) outDir
