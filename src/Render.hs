{-# LANGUAGE DeriveGeneric #-}
-- | FFMpeg Wrapper

module Render where
import GHC.Generics ( Generic )
import Data.Aeson (FromJSON, ToJSON)
import Track (Track (..), Metadata (..), Attr (..))
import Data.Map ((!), Map)
import System.FilePath (combine)
import System.Process (callCommand)

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

-- cover :: String -> Reader (Track String) String
-- cover = ffmpeg -i in.mp3 -i test.png -map 0:0 -map 1:0 -c copy -id3v2_version 3 -metadata:s:v title="Album cover" -metadata:s:v comment="Cover (front)" out.mp3

-- getSource :: RenderSettings -> Reader (Track String) String
-- getSource (SingleRender _ _ _) =
--   src <- fmap source ask
--   return "-i " ++ src ++ " -map 0:0"

ffrender :: RenderSettings -> Track String -> FilePath -> IO FilePath
ffrender rcfg track out = do
  callCommand $ "ffmpeg -i " ++ source track ++ " -c copy -s " ++ show (metadata track) ++ " -> " ++ out
  return out

ffupdate :: RenderSettings -> FilePath -> Track String -> FilePath -> IO FilePath
ffupdate rcfg from track out = do
  putStrLn $ "ffmpeg -i " ++ from ++ " -c copy -s " ++ show (metadata track) ++ " -> " ++ out
  putStrLn $ "delete " ++ from
  return out

move :: FilePath -> FilePath -> IO FilePath
move from out = do
  putStrLn $ "move " ++ from ++ " " ++ out
  return out

getOutput :: FilePath -> Track String -> FilePath
getOutput moduleName (Track _ mt) = combine moduleName $ (show $ mt!(Attr Nr)) ++ ". " ++ mt!(Attr Title)

render :: Map String (Track String) -> FilePath -> RenderSettings -> [Task] -> IO FilePath
render trkList outDir cfg task = exec task
  where exec (Render trk) = ffrender cfg <*> (getOutput outDir) $ (trkList!trk)
        exec (UpdateMetadata from trk) = ffupdate cfg from <*> (getOutput outDir) $ (trkList!trk)
        exec (Move from trk) = move from . getOutput outDir $ (trkList!trk)
