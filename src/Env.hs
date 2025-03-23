{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
-- | PPublihs Environment

module Env (loadEnv, EnvironmentException, loadTracks) where

import System.Directory (listDirectory, doesFileExist, getCurrentDirectory, getXdgDirectory, XdgDirectory (XdgConfig))
import Data.List (sortOn, groupBy, elemIndex, (\\))
import Files (FileType(..), searchFile)
import Data.Map (Map, filterWithKey, fromList, union)
import System.FilePath (takeBaseName, takeFileName, combine)
import Data.Function (on)
import Control.Exception (Exception, throwIO, IOException, catch)
import Control.Monad (when, unless)
import GHC.Base (empty)
import Data.Containers.ListUtils ( nubOrd )
import Track (Metadata (..), Track(..), Attr (..), File (..))
import ConfigDialog (getConfig, askMissing, Dialog (Dialog))
import Data.Aeson (ToJSONKey, FromJSONKey, ToJSON, FromJSON)
import Data.Time (getCurrentTime, UTCTime (utctDay))
import Data.Time.Calendar (toGregorian)
import Control.Monad.Trans.Reader ( ReaderT(runReaderT) )


data EnvField = MD Metadata | TrackDirs | Order

instance ToJSONKey EnvField
instance FromJSONKey EnvField
instance ToJSON EnvField
instance FromJSON EnvField


-- Field, Description, Default
envFields :: Map EnvField (String, IO (Maybe String))
envFields = fromList
  [(MD . File $ Cover      , ("Image File used as Album Cover"    , findFile ImageFile "cover"                   )),
   (MD . File $ Video      , ("Album video used for Video render" , findFile VideoFile "video"                   )),
   (MD . File $ Description, ("Youtube Video Description Textfile", findFile TextFile "description"              )),
   (MD . Attr $ Year       , ("Year of Album publication"         , Just <$> getCurrentYear                      )),
   (MD . Attr $ Album      , ("Name of the Album"                 , Just <$> takeFileName <$> getCurrentDirectory)),
   (MD . Attr $ Artist     , ("Artist Name"                       , return Nothing                               )),
   (MD . Attr $ Genre      , ("Genre"                             , return Nothing                               )),
   (TrackDirs              , ("Directories to search for Tracks"  , return . Just $ "."                          )),
   (Order                  , ("File with Track Order"             , findFile TextFile "order"                    ))]
  where
    findFile:: Files.FileType -> String -> IO (Maybe FilePath)
    findFile t n = getCurrentDirectory >>= listDirectory >>= return . searchFile t n
    getCurrentYear = fmap (show . (\(y,_,_)->y) . toGregorian . utctDay) getCurrentTime

globalFields :: forall a. Map EnvField a -> Map EnvField a
globalFields = filterWithKey (const . (`elem` select))
  where select = [MD . Attr $ Artist,
                  MD . Attr $ Genre]



appName :: String
appName = "ppublihs"

loadEnv :: IO (Map EnvField String)
loadEnv = do
  fields <- fmap (mapMaybe id) . sequence . Data.Map.map snd $ envFields
  appDir <- getXdgDirectory XdgConfig appName

  let globalConfPath = combine appDir "defaults.json"
      localConfPath = "ppconf.json"

  globalExists <- doesFileExist globalConfPath

  unless globalExists . putStrLn $
      "This seems to be the First time you start PPublish!\n\
        \PPublihs Setup Dialog!\n\
        \Please enter default Global values now, you can change all settings later\n\
        \in " ++ show globalConfPath ++ " or via settings command\n\
        \Select Default by leaving empty\n"

  globalConf <- runReaderT (getConfig globalConfPath askMissing) $ on Dialog globalFields questions fields
  runReaderT (getConfig localConfPath askMissing) $ Dialog questions globalConf
    where questions = (Data.Map.map fst envFields)




-- Track Loading

data EnvironmentException = DuplicateTracksError String | UnknownTrackName String deriving Show
instance Exception EnvironmentException

duplicateTracks :: [FilePath] -> [[(String, FilePath)]]
duplicateTracks = filter ((> 1) .length)
                . groupBy (on (==) fst)
                . sortOn fst
                . Prelude.map (flip (,) <*> takeBaseName)


getTracks :: [FilePath] -> IO [FilePath]
getTracks dirs = do
  tracks <- fmap concat . sequence . map listDirectory $ dirs
  let duplicateNames = duplicateTracks tracks
  unless (null duplicateNames) . throwIO . DuplicateTracksError $ "Multiple Tracks have the same name: " ++ show duplicateNames
  return tracks

-- make total order for tracks
getOrder :: FilePath -> [String] -> IO [String]
getOrder ordFile tracks = do
  ord <- (fmap (lines) . readFile $ ordFile) `catch` \(e :: IOException)->putStrLn ("Could not read Track order from "++ordFile) >> return []

  let invalid = ord \\ tracks
  when (invalid /= empty) $ throwIO (UnknownTrackName $ "Invalid Tracks in '"++ordFile++"': " ++ show invalid)

  let res = nubOrd $ ord ++ tracks

  writeFile ordFile . unlines $ res
  return res

loadMetadata :: Map Metadata String -> [String] -> [FilePath] -> Map String (Track String)
loadMetadata mtdt ord = fromList . map (liftA2 (,) takeBaseName fn)
  where fn src = Track src $ union mtdt . fromList $ [(Attr Title, takeBaseName src),
                                                      (Attr Nr, show . (+1) . (`elemIndex` ord) . takeBaseName $ src)]

loadTracks :: Map EnvField String -> IO (Map String (Track String))
loadTracks env = do
  tracks <- getTracks (env!TrackDirs)
  order <- getOrder (env!Order) tracks
  return $ loadMetadata mtdt order tracks
  where mtdt = fromList . mapMaybe (\case (MD a, b)->Just (a,b); _ -> Nothing) . toList $ env
