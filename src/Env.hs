{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | PPublihs Environment

module Env (LocalState(..), Env(..), loadEnv, EnvironmentException) where

import GHC.Generics ( Generic )
import System.Directory (listDirectory)
import Data.List (sortOn, groupBy, elemIndex, sortBy, (\\), find)
import Prelude hiding (lookup)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import FFMpeg (RenderSettings, ffrender)
import Data.String (fromString)
import Files (FileType(AudioFile), Checksum, TrackName(..), filterFiles, md5Str)
import Data.Map (mapWithKey)
import System.FilePath (takeBaseName)
import Data.Function (on)
import Control.Exception (Exception, throw, catch, throwIO)
import Control.Monad (when)
import GHC.Base (empty)
import Data.Containers.ListUtils ( nubOrd )
import GHC.IO.Exception (IOException)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class (liftIO)
import Track (Metadata)
import Settings (Settings)

data LocalState = LocalState {
  tracks :: [(TrackName, Checksum)],
  metadata :: Metadata Checksum
                             } deriving (Show, Generic)
instance FromJSON LocalState
instance ToJSON LocalState

data EnvironmentException = DuplicateTracksError String | UnknownTrackName String deriving Show
instance Exception EnvironmentException


data Env = Env{state :: LocalState, render :: TrackName -> FilePath -> RenderSettings -> ReaderT (Metadata String) IO ()}

duplicateTracks :: [FilePath] -> [[(TrackName, FilePath)]]
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


loadEnv :: Settings -> IO Env
loadEnv cfg = do
  trackPaths <- fmap (filterFiles AudioFile . concat) $ mapM listDirectory (trackDirs cfg)

  let dups = duplicateTracks trackPaths
  when (dups /= empty) $ throw (DuplicateTracksError $ "Found mutiple Tracks with same Name: " ++ show dups)

  trkcksm <- mapM (sequence . liftA2 (,) (TrackName . takeBaseName) (fmap md5Str . BS.readFile)) trackPaths

  order <- getOrder "order.txt" . map fst $ trkcksm
  let orderedTracks = sortBy (on (compare) ((`elemIndex` order) . fst)) trkcksm

  mtdt <- sequence . mapWithKey getChecksum $ fields cfg
  return $ Env (LocalState orderedTracks mtdt)
               (\(TrackName t) ->case find ((==) t . takeBaseName) trackPaths of
                   Just source -> ffrender source (TrackName t)
                   Nothing -> const . const $ liftIO . throwIO . UnknownTrackName $ "Trying to Render unknown Track: " ++ show t)

getChecksum :: Field -> String -> IO Checksum
getChecksum (Attr _) = return . md5Str . fromString
getChecksum (File _) = fmap md5Str . BS.readFile
