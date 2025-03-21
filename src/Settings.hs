{-# LANGUAGE DeriveGeneric #-}
-- |

module Settings where
import Data.Aeson (ToJSONKey, FromJSONKey, ToJSON, FromJSON)
import System.Directory (getCurrentDirectory, listDirectory, getXdgDirectory, XdgDirectory (..))
import Data.Time (UTCTime (utctDay), getCurrentTime)
import Data.Time.Calendar (toGregorian)
import GHC.Generics (Generic)
import Data.Map (Map, fromList, (!), toList)
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import ConfigDialog (Asking, getConfig, Dialog (Dialog), askMissing)
import Track (Metadata, Field (..))
import Files (FileType(..), searchFile)
import Data.Maybe (catMaybes, mapMaybe)
import System.FilePath (takeFileName, combine)
import Data.List.Split (splitOn)

data Option = MTDT Field | Conf String deriving (Ord, Eq, Show, Generic)

instance ToJSONKey Option
instance FromJSONKey Option
instance ToJSON Option
instance FromJSON Option

questions :: [(Option, String)]
questions =
  [(MTDT $ Attr "artist"      , "Artist Name"                 ),
   (MTDT $ Attr "genre"       , "Common Genre"                ),
   (MTDT $ Attr "album"       , "Album Name"                  ),
   (MTDT $ File "description" , "YouTube Description"         ),
   (MTDT $ File "cover"       , "Cover Image"                 ),
   (MTDT $ File "video"       , "Album Video"                 ),
   (Conf "trackDirs", "Directories with Tracks"     )]

appName :: String
appName = "ppublihs"

getDefaults :: IO (Map Option FilePath)
getDefaults = fmap (fromList . catMaybes) . sequence $
                [md (File "cover"       ) $ findFile ImageFile "cover"     ,
                 md (File "video"       ) $ findFile VideoFile "video"     ,
                 md (File "description" ) $ findFile TextFile "description",
                 md (Attr "year"        ) $  fmap (Just . show . (\(y,_,_)->y) . toGregorian . utctDay) getCurrentTime,
                 md (Attr "album"       ) $ Just <$> takeFileName <$> getCurrentDirectory,

                 return . Just $ (Conf "trackDirs", ".")]
  where
    md field dflt = fmap (fmap ((,) (MTDT field))) dflt
    findFile:: FileType -> String -> IO (Maybe FilePath)
    findFile t n = getCurrentDirectory >>= listDirectory >>= return . searchFile t n


getAppSettings :: Asking Option -> IO (Map Option String)
getAppSettings askFor = do
  dir <- getXdgDirectory XdgConfig appName
  defaults <- getDefaults

  let configFile = combine dir "defaults.json"
  putStrLn $ "PPublihs Setup Dialog!\n\
        \Please enter default Global values now, you can change all settings later\n\
        \in " ++ show configFile ++ " or via settings command\n\
        \Select Default by leaving empty\n"

  runReaderT (getConfig configFile askFor) $
    Dialog (fromList . take 2 $ questions) defaults


data Settings = Settings{trackDirs :: [String], metadata :: Metadata String}

getSettings :: Asking Option -> IO Settings
getSettings askFor = do
  global <- getAppSettings askMissing
  answers <- runReaderT (getConfig "ppconf.json" askFor) $ Dialog (fromList questions) global
  return $ Settings (splitOn "," $ answers!(Conf "trackDirs")) (fromList . mapMaybe collectMtdt . toList $ answers)

  where collectMtdt (key, val) = case key of
          Conf _ -> Nothing
          MTDT m -> Just (m, val)
