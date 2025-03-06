{-# LANGUAGE DeriveGeneric #-}
-- | FFMpeg Wrapper

module FFMpeg where
import GHC.Generics ( Generic )
import Data.Aeson (eitherDecode, FromJSON, ToJSON)

data RenderSettings = RenderSettings{
  concat :: Bool,
  video :: Bool,
  metadata :: Bool,
  filters :: [String],
  format :: String
                                } deriving (Generic)

instance FromJSON RenderSettings
instance ToJSON RenderSettings

render ::
