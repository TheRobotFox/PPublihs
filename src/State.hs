{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | PPublihs State

module State where

import GHC.Generics ( Generic )
import Files (File, filterFiles, FileType (AudioFile), loadFile, tryLoad)
import Settings (Settings (trackDirs, fields), CorruptedConfig (CorruptedConfig))
import System.Directory (getCurrentDirectory, listDirectory)
import Data.List (nub)
import Data.Map ((!), lookup, Map, toList)
import Prelude hiding (lookup)
import GHC.IO (throwIO)
import Data.Aeson (eitherDecode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as BS

data LocalState where
  LocalState :: {
    tracks :: [File],
    albumName:: String,
    cover :: Maybe File,
    video :: Maybe File,
    description :: Maybe File,
    metadata :: [(String,String)]
           } -> LocalState
  deriving (Generic, Show)

instance FromJSON LocalState
instance ToJSON LocalState

generateState :: Settings -> IO LocalState
generateState settings = do
  dir <- getCurrentDirectory
  dirFiles <- listDirectory dir
  trackPaths <- nub . (filterFiles AudioFile . concat . (dirFiles:)) <$> mapM listDirectory (trackDirs settings)
  trks <- mapM loadFile trackPaths

  cover <- tryLoad $ attr!"cover"
  video <- tryLoad $ attr!"video"
  desc  <- tryLoad $ attr!"desc"

  return $ LocalState trks (attr!"album") cover desc video
    $ filter (not . (`elem` ["cover", "video", "desc"]) . fst) (toList attr)
    where attr = fields settings

loadStates :: FilePath -> IO (Map String LocalState)
loadStates path = do
  d <- BS.readFile path
  case eitherDecode d of
    Right states -> return states
    Left e -> throwIO $ CorruptedConfig ("PPublihs cache is damaged, please fix or remove 'ppcache.json'! Error: " ++ e)
