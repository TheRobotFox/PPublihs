{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | PPublihs State

module State (LocalState(..), getState) where

import GHC.Generics ( Generic )
import System.Directory (listDirectory)
import Data.List (nub)
import Data.Map ((!), toList)
import Prelude hiding (lookup)
import Data.Aeson (eitherDecode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as BS
import Control.Exception
import Data.Map (lookup)
import Settings (Settings (..), Fields, Field (..))
import Files (TrackName (..), Checksum, filterFiles, FileType (..), loadFile, readMD5)
import Data.Maybe (catMaybes)
import Control.Monad.Trans.Writer (WriterT(runWriterT))

data LocalState = LocalState {
  tracks :: [(TrackName, Checksum)],
  metadata :: Fields Checksum
                             } deriving (Show, Generic)

instance FromJSON LocalState
instance ToJSON LocalState


promoteIO :: Maybe a -> (a-> IO (Maybe b)) -> IO (Maybe b)
promoteIO (Just r) f = f r
promoteIO Nothing _ = return Nothing

generateState :: Settings -> IO LocalState
generateState settings = do
  trackPaths <- fmap (nub . filterFiles AudioFile . concat) $ mapM listDirectory (trackDirs settings)
  trks <- runWriterT . fmap catMaybes . mapM loadFile $ trackPaths
  mapM_ putStrLn . snd $ trks

  coverPath <- loadOpt Cover
  videoPath <- loadOpt Video
  desc  <- loadOpt Description
  optFiles <- fmap loadOpt [Cover, Video, Description]

  return $ LocalState
                trks
                (map (TrackName . path) trks)
                (attr!"album") coverPath desc videoPath $ filter (not . (`elem` ["cover", "video", "desc"]) . fst) (toList attr)
    where attr = fields settings
          loadOpt field = promoteIO (lookup field attr) readMD5

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
