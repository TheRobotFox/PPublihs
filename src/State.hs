{-# LANGUAGE DeriveGeneric #-}
-- | PPublihs State

module State (LocalState(..)) where

import GHC.Generics ( Generic )
import Files (File, filterFiles, FileType (AudioFile), loadFile, tryLoad, path)
import Settings (Settings (trackDirs, fields), CorruptedConfig (CorruptedConfig))
import System.Directory (getCurrentDirectory, listDirectory)
import Data.List (nub)
import Data.Map ((!), Map, toList)
import Prelude hiding (lookup)
import GHC.IO (throwIO)
import Data.Aeson (eitherDecode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as BS
import Control.Exception

data LocalState = LocalState {
    tracks :: [File],
    trackOrder :: [FilePath],
    albumName:: String,
    cover :: Maybe File,
    video :: Maybe File,
    description :: Maybe File,
    metadata :: [(String,String)]
           }
  deriving (Generic, Show)

instance FromJSON LocalState
instance ToJSON LocalState

generateState :: Settings -> IO LocalState
generateState settings = do
  dir <- getCurrentDirectory
  dirFiles <- listDirectory dir
  trackPaths <- nub . (filterFiles AudioFile . concat . (dirFiles:)) <$> mapM listDirectory (trackDirs settings)
  trks <- mapM loadFile trackPaths

  coverPath <- tryLoad $ attr!"cover"
  videoPath <- tryLoad $ attr!"video"
  desc  <- tryLoad $ attr!"desc"

  return $ LocalState
                trks
                (map path trks)
                (attr!"album") coverPath desc videoPath $ filter (not . (`elem` ["cover", "video", "desc"]) . fst) (toList attr)
    where attr = fields settings

loadStates :: FilePath -> IO (Map String LocalState)
loadStates filePath = do
  d <- BS.readFile filePath
  case eitherDecode d of
    Right states -> return states
    Left e -> throwIO $ CorruptedConfig ("PPublihs cache is damaged, please fix or remove 'ppcache.json'! Error: " ++ e)
