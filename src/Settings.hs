{-# LANGUAGE DeriveGeneric #-}

-- | PPublihs Settings
module Settings (configDir, Settings(..), Fields, Field(..), getSettings, askAll, askNew, askOverwrite) where
import Files (searchFile, FileType(..), loadOrCreate)
import Data.Map (Map, member, fromList, empty, union, lookup, (!))
import System.FilePath (combine, takeFileName)
import Data.Maybe (catMaybes)
import Prelude hiding (lookup)
import GHC.Generics (Generic)
import System.IO (hFlush, stdout)
import Data.Aeson (ToJSONKey, FromJSONKey, ToJSON, FromJSON)
import System.Directory (getCurrentDirectory, listDirectory, getXdgDirectory, XdgDirectory (..))
import Data.Time (UTCTime (utctDay), getCurrentTime)
import Data.Time.Calendar (toGregorian)

-- Definitions

data Field = File String | Attr String
           deriving (Generic, Show, Eq, Ord)
type Fields a = Map Field a

data Settings = Settings{trackDirs :: [FilePath], fields :: Fields String} deriving (Show, Generic)


instance ToJSONKey Field
instance FromJSONKey Field
instance ToJSON Field
instance FromJSON Field
instance ToJSON Settings
instance FromJSON Settings



-- Fields and Defaults

dialogFields :: [(Field, String)]
dialogFields =
  [(Attr "artist"     , "Artist Name"                 ),
   (Attr "genre"      , "Common Genre"                ),
   (Attr "album"      , "Album Name"                  ),
   (File "description", "YouTube Description"         ),
   (File "cover"      , "Cover Image"                 ),
   (File "video"      , "Album Video"                 )]

findDefaults :: IO (Fields String)
findDefaults = do
  dir <- configDir
  let configFile = combine dir "defaults.json"

  global <- loadOrCreate configFile (putStrLn ("PPublihs Setup Dialog!\n\
               \Please enter default Global values now, you can change all settings later\n\
               \in " ++ show configFile ++ " or via settings command\n\
               \Select Default by leaving empty\n") >> settingsDialog (askAll empty) (take 2 dialogFields))

  gen <- fmap (fromList . catMaybes) . sequence $
                [def (File "cover"      ) $ findFile ImageFile "cover"     ,
                 def (File "video"      ) $ findFile VideoFile "video"     ,
                 def (File "description") $ findFile TextFile "description",
                 def (Attr "year"       ) $  fmap (Just . show . (\(y,_,_)->y) . toGregorian . utctDay) getCurrentTime,
                 def (Attr "album"      ) $ Just <$> takeFileName <$> getCurrentDirectory]

  return $ union global gen
        where def field dflt = fmap (fmap ((,) field)) dflt


configDir :: IO FilePath
configDir = getXdgDirectory XdgConfig "ppublihs"


-- Asking Callbacks

type Dialog = Fields String -> (Field, String) -> IO (Maybe (Field, String))

askAll :: Dialog
askAll answered (name, desc) =
  do putStr $ desc ++ " (default: "++ show (lookup name answered) ++ ")" ++ ": "
     hFlush stdout
     answer <- getLine
     putStrLn $ "Ok!"
     return $ if null answer then Nothing else Just (name, answer)

askOverwrite :: Dialog
askOverwrite answered (name, desc) =
        if member name answered then return $ Just (name, answered!name)
        else askAll answered (name, desc)

askNew :: Dialog
askNew answered (name, desc) =
        if member name answered then return Nothing
        else askAll answered (name, desc)



-- Implementation

findFile:: FileType -> String -> IO (Maybe FilePath)
findFile t n = getCurrentDirectory >>= listDirectory >>= return . searchFile t n

settingsDialog :: ((Field, String) -> IO (Maybe (Field, String))) -> [(Field, String)] -> IO (Fields String)
settingsDialog ask questions =
  sequence (map ask questions) >>= return . fromList . catMaybes

getSettings :: Dialog -> IO Settings
getSettings askFor = do
     defaults  <- findDefaults
     local       <- loadOrCreate "ppconf.json" (putStrLn "Configure Album!\n" >>
                                                settingsDialog (askFor defaults) dialogFields)
     return       $ Settings ["."] (union local defaults)
