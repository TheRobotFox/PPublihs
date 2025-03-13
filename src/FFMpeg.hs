{-# LANGUAGE DeriveGeneric #-}
-- | FFMpeg Wrapper

module FFMpeg where
import GHC.Generics ( Generic )
import Data.Aeson (eitherDecode, FromJSON, ToJSON)

data RenderSettings = MergedRender{
    video :: Bool,
    metadata :: Bool,
    filters :: [String],
    format :: String
  } | SingleRender{
    metadata :: Bool,
    filters :: [String],
    format :: String
  } deriving (Generic, Eq)

instance FromJSON RenderSettings
instance ToJSON RenderSettings

-- concatFilter :: [File] -> String
-- concatFilter = flip (++) ":a=1[audio]"("concat=n="++ show . length)



-- render_cmd :: File -> RenderSettings -> [File] -> String
-- render_cmd cover settings tracks = "ffmpeg" ++
