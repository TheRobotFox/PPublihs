{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | PPublihs State

module State where

import GHC.Generics ( Generic )
import System.Directory ( getCurrentDirectory, listDirectory)
import Files (File, FileType (..), loadFile, filterFiles, tryLoad, searchFile)
import System.FilePath (takeFileName)
import Data.List (nub)
import Control.Monad (msum)
import Data.Maybe (maybeToList)

data LocalState where
  LocalState :: {
    tracks :: [File],
    albumName:: String,
    cover :: Maybe File,
    video :: Maybe File,
    description :: Maybe String,
    metadata :: [(String, String)] -- TODO Hashmap
           } -> LocalState
  deriving (Generic, Show)


loadAny:: FileType -> String -> [FilePath] -> IO (Maybe File)
loadAny t prefered alternatives = msum . map tryLoad $
  concat [(maybeToList . searchFile t prefered) alternatives, filterFiles t alternatives]

generateState:: Settings -> IO LocalState
scanEnvironment ctx = do
  dir <- getCurrentDirectory
  dirFiles <- listDirectory dir

  trackPaths <- nub . (filterFiles AudioFile . concat . (dirFiles:)) <$> mapM listDirectory (trackDirs ctx)
  trks <- mapM loadFile trackPaths

  cover <- loadAny ImageFile "cover" dirFiles
  video <- loadAny VideoFile "video" dirFiles
  descr <- loadAny TextFile "desc" dirFiles
  return $ State trks (takeFileName dir) cover video descr []
