{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Tracks

module Track where
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, FromJSONKey, ToJSON, ToJSONKey )
import Data.Map (Map, filterWithKey, (!), keys, fromList)
import Data.Function (on)
import Files (Checksum (Checksum), md5Str)
import qualified Data.ByteString as BS
import Control.Exception (IOException, throwIO, Exception, catch)
import System.Process (readProcess)
import Text.Read (readMaybe)

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


getChecksum :: Track String -> IO (Track Checksum)
getChecksum (Track src mtdt) = do
  s <- fmap md5Str . BS.readFile $ src
  m <- fmap fromList . mapM (sequence . liftA2 (,) id cksm) . keys $ mtdt
  return $ Track s m
  where cksm a@(Attr _) = return . Checksum $ mtdt!a
        cksm a@(File _) = fmap md5Str . BS.readFile $ mtdt!a

data FFException = ReadAudioLength String | RunFFProbe String deriving (Show)

instance Exception FFException

getAudioLength:: FilePath -> IO Float
getAudioLength filepath = (do probe <- readProcess "ffprobe" ["-i", filepath, "-show_entries", "format=duration", "-v", "quiet", "-of", "csv=p=0"] []
                              case readMaybe probe of
                                Just l -> return l
                                Nothing -> throwIO $ ReadAudioLength probe)
                          `catch` \(e :: IOException) -> throwIO (RunFFProbe (show e))
