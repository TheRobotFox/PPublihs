{-# LANGUAGE DeriveGeneric #-}
-- | Tracks

module Track where
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, FromJSONKey, ToJSON, ToJSONKey )
import Data.Map (Map, filterWithKey)
import Data.Function (on)
import Control.Monad.Trans.Reader

data Attr = Cover | Video | Year | Artist | Album | Description | Genre
           deriving (Generic, Show, Eq, Ord)
instance ToJSON Attr
instance FromJSON Attr

data Metadata = File FilePath | Tag Attr
           deriving (Generic, Show, Eq, Ord)
instance ToJSONKey Metadata
instance FromJSONKey Metadata
instance ToJSON Metadata
instance FromJSON Metadata

data Track a = Track{source :: a, metadata :: Map Metadata a}

metadataValid :: Eq c => [Metadata] -> Track c -> Track c -> Bool
metadataValid testFor = on ((==) . filterWithKey (const . (`elem` testFor))) metadata

duplicateTracks :: [FilePath] -> [[(String, FilePath)]]
duplicateTracks = filter ((> 1) .length)
                . groupBy (on (==) fst)
                . sortOn fst
                . map (flip (,) <*> (TrackName . takeBaseName))

getOrder :: FilePath -> [TrackName] -> IO [TrackName]
getOrder file tracks' = do
  ord <- (fmap (map TrackName . lines) . readFile $ file) `catch` (\(_ :: IOException)->return [])

  let invalid = ord \\ tracks'
  when (invalid /= empty) $ throwIO (UnknownTrackName $ "Invalid Tracks in '"++file++"': " ++ show invalid)

  let res = nubOrd $ ord ++ tracks'

  writeFile file . unlines . map (\(TrackName s)->s) $ res
  return res

loadTracks :: [FilePath] -> Map Metadata String
