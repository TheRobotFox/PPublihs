{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | PPublihs State

module State (LocalState(..), getState) where

import GHC.Generics ( Generic )
import Files (File, filterFiles, FileType (AudioFile), loadFile, tryLoad, path, TrackName(..), Checksum)
import Settings (Settings (trackDirs, fields), CorruptedConfig (CorruptedConfig), getSettings, askAll)
import System.Directory (getCurrentDirectory, listDirectory)
import Data.List (nub)
import Data.Map ((!), toList, Map)
import Prelude hiding (lookup)
import Data.Aeson (eitherDecode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as BS
import Control.Exception
import Data.Map (lookup)

data Field = Track String
           | AlbumName
           | Cover
           | Video
           | Description
           | Metadata deriving (Generic, Show)


type LocalState = Map Field Checksum



promoteIO :: Maybe a -> (a-> IO (Maybe b)) -> IO (Maybe b)
promoteIO (Just r) f = f r
promoteIO Nothing _ = return Nothing

generateState :: Settings -> IO LocalState
generateState settings = do
  dir <- getCurrentDirectory
  dirFiles <- listDirectory dir
  trackPaths <- nub . (filterFiles AudioFile . concat . (dirFiles:)) <$> mapM listDirectory (trackDirs settings)
  trks <- mapM loadFile trackPaths

  coverPath <- loadOpt "cover"
  videoPath <- loadOpt "video"
  desc  <- loadOpt "desc"

  return $ LocalState
                trks
                (map (TrackName . path) trks)
                (attr!"album") coverPath desc videoPath $ filter (not . (`elem` ["cover", "video", "desc"]) . fst) (toList attr)
    where attr = fields settings
          loadOpt field = promoteIO (lookup field attr) tryLoad

loadState :: FilePath -> IO LocalState
loadState filePath = do
  d <- BS.readFile filePath
  case eitherDecode d of
    Right states -> return states
    Left e -> throwIO $ CorruptedConfig ("PPublihs cache is damaged, please fix or remove 'ppcache.json'! Error: " ++ e)

stateFile :: String
stateFile = "ppcache.json"

getState :: IO LocalState
getState = loadState stateFile `catch` (\(e :: IOException)->putStrLn (show e) >>
                                                             putStrLn "generating New State" >>
                                                             getSettings askAll >>= generateState)
