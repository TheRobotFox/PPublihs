{-# LANGUAGE DeriveGeneric #-}
-- | Tracks

module Track where
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, FromJSONKey, ToJSON, ToJSONKey )
import Data.Map (Map, filterWithKey)
import Data.Function (on)

data Attr = Year | Artist | Album | Genre | Title | Nr
           deriving (Generic, Show, Eq, Ord)
instance ToJSON Attr
instance FromJSON Attr
data File = Cover | Video | Description
           deriving (Generic, Show, Eq, Ord)
instance ToJSON File
instance FromJSON File

data Metadata = File File | Attr Attr
           deriving (Generic, Show, Eq, Ord)
instance ToJSONKey Metadata
instance FromJSONKey Metadata
instance ToJSON Metadata
instance FromJSON Metadata

data Track a = Track{source :: a, metadata :: Map Metadata a} deriving (Generic)

instance ToJSON a => ToJSON (Track a)
instance FromJSON a => FromJSON (Track a)


metadataValid :: Eq c => [Metadata] -> Track c -> Track c -> Bool
metadataValid testFor = on ((==) . filterWithKey (const . (`elem` testFor))) metadata


-- getChecksum :: Field -> String -> IO Checksum
-- getChecksum (Attr _) = return . md5Str . fromString
-- getChecksum (File _) = fmap md5Str . BS.readFile
