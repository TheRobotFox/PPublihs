{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Tracks

module Track where
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, FromJSONKey, ToJSON, ToJSONKey )
import Data.Map (Map, filterWithKey, (!), keys, fromList, toList)
import Data.Function (on)
import Files (Checksum (Checksum), md5Str)
import qualified Data.ByteString as BS
import Control.Exception (IOException, throwIO, Exception, catch)
import System.Process (readProcess)
import Text.Read (readMaybe)
import Data.List (find, sortOn)

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

data Track = Track{path :: FilePath, cksm :: Checksum, metadata :: Map Metadata String} deriving (Generic)

instance ToJSON Track
instance FromJSON Track

type TrackList = Map String Track

metadataValid :: [Metadata] -> Track -> Track -> Bool
metadataValid testFor = on ((==) . filterWithKey (const . (`elem` testFor))) metadata


data FFException = ReadAudioLength String | RunFFProbe String deriving (Show)

instance Exception FFException

getAudioLength:: FilePath -> IO Float
getAudioLength filepath = do
  probe <- readProcess "ffprobe" ["-i", filepath, "-show_entries", "format=duration", "-v", "quiet", "-of", "csv=p=0"] []
              `catch` \(e :: IOException) -> throwIO (RunFFProbe (show e))
  case readMaybe probe of
    Just l -> return l
    Nothing -> throwIO $ ReadAudioLength probe

trackCacheEntry :: FilePath -> IO (FilePath, Checksum)
trackCacheEntry = sequence . liftA2 (,) id (fmap md5Str . BS.readFile)

matchBackOn :: Eq a => ([a] -> a -> Maybe a) -> [a] -> [a] -> [(Maybe a, Maybe a)]
matchBackOn _ [] x =  map ((,) Nothing . Just) x
matchBackOn _ x [] =  map (flip (,) Nothing . Just) x
matchBackOn f prev (x:xs) = (match, Just x) : case match of
                       Just rm -> matchBackOn f [t | t<-prev, t/=rm] xs
                       Nothing -> matchBackOn f prev xs
  where match = f prev x


-- Try to match Tracks to previous State
matchSource :: [(String, Checksum)] -> [(String, Checksum)] -> [(Maybe String, Maybe String)]
matchSource = (map (liftA2 (on (,) $ fmap fst) fst snd) .) . matchBackOn matchBy
                -- (map (fmap cksm) . toList)
  where matchBy prev x = find (on (==) snd x) prev

sortTracks :: TrackList -> [(String, Track)]
sortTracks = sortOn ((read :: String -> Int) . flip (!) (Attr Nr) . metadata . snd) . toList
