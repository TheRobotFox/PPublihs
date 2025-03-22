{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | PPublihs Environment

module Env (LocalState(..), Env(..), loadEnv, EnvironmentException) where

import GHC.Generics ( Generic )
import System.Directory (listDirectory, doesFileExist)
import Data.List (sortOn, groupBy, elemIndex, sortBy, (\\), find)
import Prelude hiding (lookup)
import FFMpeg (RenderSettings, ffrender)
import Data.String (fromString)
import Files (FileType(AudioFile), Checksum, TrackName(..), filterFiles, md5Str)
import Data.Map (mapWithKey, Map, adjust)
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
import ConfigDialog (getConfig)

data EnvironmentException = DuplicateTracksError String | UnknownTrackName String deriving Show
instance Exception EnvironmentException


data EnvField = MD Metadata | TrackDirs | Order

instance ToJSONKey EnvField
instance FromJSONKey EnvField
instance ToJSON EnvField
instance FromJSON EnvField


getCurrentYear = fmap (show . (\(y,_,_)->y) . toGregorian . utctDay) getCurrentTime


-- Field, Description, Default
envFields :: Map EnvField (String, IO (Maybe String))
envFields = fromList
  [(MD . File . Cover      , ("Image File used as Album Cover"    , findFile ImageFile "cover"))                   ,
   (MD . File . Video      , ("Album video used for Video render" , findFile VideoFile "video"))                   ,
   (MD . File . Description, ("Youtube Video Description Textfile", findFile TextFile "description"))              ,
   (MD . Tag . Year        , ("Year of Album publication"         , Just . getCurrentYear))                        ,
   (MD . Tag . Album       , ("Name of the Album"                 , Just <$> takeFileName <$> getCurrentDirectory)),
   (MD . Tag . Artist      , ("Artist Name"                       , Nothing))                                      ,
   (MD . Tag . Genre       , ("Genre"                             , Nothing))                                      ,
   (TrackDirs              , ("Directories to search for Tracks"  , return . Just $ "."))                          ,
   (Order                  , ("Track Order"                       , findFile TextFile "order"))]
  where
    findFile:: FileType -> String -> IO (Maybe FilePath)
    findFile t n = getCurrentDirectory >>= listDirectory >>= return . searchFile t n

persistentFields :: Set EnvField
persistentFields = fromList [Artist, Genre]



appName :: String
appName = "ppublihs"

loadEnv :: IO (Map EnvField String)
loadEnv = do
  fields <- sequence . map snd . envFields
  appDir <- getXdgDirectory XdgConfig appName

  let globalConf = combine appDir "defaults.json"
      localConf = "ppconf.json"

  globalExists <- doesFileExist globalConf
  localExists <- doesFileExist globalConf

  unless globalExists putStrLn $
      "This seems to be the First time you start PPublish!\n\
        \PPublihs Setup Dialog!\n\
        \Please enter default Global values now, you can change all settings later\n\
        \in " ++ show globalConfig ++ " or via settings command\n\
        \Select Default by leaving empty\n"

  globalConf <- runReaderT (getConfig globalConf askNew) $ on Dialog (map fst) (map snd) fields
  runReaderT (getConfig localConf askNew) $ on Dialog (map fst fields) globalConf



getChecksum :: Field -> String -> IO Checksum
getChecksum (Attr _) = return . md5Str . fromString
getChecksum (File _) = fmap md5Str . BS.readFile
