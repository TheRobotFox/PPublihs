{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | PPublihs Settings
module Settings where
import Files (searchFile, FileType(..), loadOrCreate)
import Data.Map (Map, member, fromList, empty, union, lookup, (!), toList)
import System.FilePath (combine, takeFileName)
import Data.Maybe (catMaybes)
import Prelude hiding (lookup)
import Control.Exception (Exception)
import GHC.Generics (Generic)
import System.IO (hFlush, stdout)
import Data.Aeson (ToJSONKey, FromJSONKey, ToJSON, FromJSON)
import System.Directory (getCurrentDirectory, listDirectory, getXdgDirectory, XdgDirectory (..))

data Field = AlbumName | Cover | Video | Description | Attr String
           deriving (Generic, Show, Eq, Ord)

instance ToJSONKey Field
instance FromJSONKey Field
instance ToJSON Field
instance FromJSON Field

type Fields = Map Field String

data Settings = Settings{trackDirs :: [FilePath], fields :: [(Field, String)]} deriving (Show, Generic)
instance ToJSON Settings
instance FromJSON Settings

newtype CorruptedConfig = CorruptedConfig String deriving (Show)
instance Exception CorruptedConfig


type Dialog = Fields -> (Field, String) -> IO (Maybe (Field, String))



dialogFields :: [(Field, String)]
dialogFields =
  [(Attr "artist", "Artist Name"                 ),
   (Attr "genre" , "Common Genre"                ),
   (AlbumName    , "Album Name"                  ),
   (Description  , "YouTube Description Textfile"),
   (Cover        , "Cover Image"                 ),
   (Video        , "Album Video"                 )]


findDefaults :: IO Fields
findDefaults = fmap (fromList . catMaybes) . sequence $
                [def Cover       $ findFile ImageFile "cover"     ,
                 def Video       $ findFile VideoFile "video"     ,
                 def Description $ findFile TextFile "description",
                 def AlbumName   $ Just <$> takeFileName <$> getCurrentDirectory]
  where
    def field dflt = fmap (fmap ((,) field)) dflt

findFile:: FileType -> String -> IO (Maybe FilePath)
findFile t n = getCurrentDirectory >>= listDirectory >>= return . searchFile t n

settingsDialog :: ((Field, String) -> IO (Maybe (Field, String))) -> [(Field, String)] -> IO Fields
settingsDialog ask questions =
  sequence (map ask questions) >>= return . fromList . catMaybes

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


configDir :: IO FilePath
configDir = getXdgDirectory XdgConfig "ppublihs"

getSettings :: Dialog -> IO Settings
getSettings askFor =
  do configFile  <- (flip combine) "defaults.json" <$> configDir
     global      <- loadOrCreate configFile (putStrLn
                                             ("PPublihs Setup Dialog!\n\
               \Please enter default Global values now, you can change all settings later\n\
               \in " ++ show configFile ++ " or via settings command\n\
               \Select Default by leaving empty\n") >> settingsDialog (askAll empty) (take 2 dialogFields))
     autofields  <- findDefaults
     let defaults = union global autofields
     local       <- loadOrCreate "ppconf.json" (putStrLn "Configure Album!\n" >>
                                                settingsDialog (askFor defaults) dialogFields)
     return       $ Settings ["."] (toList . union local $ defaults)
