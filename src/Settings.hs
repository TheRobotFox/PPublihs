{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | PPublihs Settings
module Settings where
import Files
import Data.Map (Map, member, fromList, empty, union, lookup, (!))
import System.Directory (getXdgDirectory, XdgDirectory (XdgConfig), doesFileExist, createDirectoryIfMissing, getCurrentDirectory, listDirectory)
import Data.Aeson (encode, eitherDecode, ToJSON, FromJSON)
import System.FilePath (takeDirectory, combine, takeFileName)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (catMaybes)
import GHC.IO (throwIO)
import Prelude hiding (lookup)
import Control.Exception (Exception)
import GHC.Generics (Generic)

type Fields = Map String String
data Settings = Settings{trackDirs :: [FilePath], fields :: Fields} deriving (Show, Generic)
type Dialog = Fields -> (String, String) -> IO (Maybe (String, String))

newtype CorruptedConfig = CorruptedConfig String deriving (Show)

instance Exception CorruptedConfig

instance ToJSON Settings
instance FromJSON Settings

findFile:: FileType -> String -> IO (Maybe FilePath)
findFile t n = do dir   <- getCurrentDirectory
                  files <- listDirectory dir
                  return $ searchFile files t n

settingsDialog :: String -> ((String, String) -> IO (Maybe (String, String)))
                -> [(String, String)] -> IO (Fields)
settingsDialog msg ask questions =
  do putStrLn msg
     res <- sequence $ map ask questions
     return . fromList $ catMaybes res

getMakeSettings :: FilePath -> IO Fields -> IO Fields
getMakeSettings file dialog =
  do exists <- doesFileExist file
     if exists then
       do json <- BS.readFile file
          case eitherDecode json of
            Right settings -> return settings
            Left e -> throwIO $ CorruptedConfig $ "Failed to Read " ++ file ++ ": " ++ e
     else
        -- run Dialog
        do createDirectoryIfMissing True (takeDirectory file)
           content <- dialog
           BS.writeFile file $ encode content
           return content

askAll :: Dialog
askAll answered (name, desc) =
  do putStr $ desc ++ " (default: "++ show (lookup name answered) ++ ")" ++ ": "
     answer <- getLine
     putStrLn $ "."
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

filterPair :: [(String, IO (Maybe String))] -> IO [(String, String)]
filterPair l = catMaybes <$> (sequence . map f) l
  where
        f (a, b) = do
          p <- b
          case p of
                Just e -> return $ Just (a,e)
                Nothing -> return Nothing

globalDialog :: FilePath -> IO Fields

globalDialog file = settingsDialog
                ("PPublihs Setup Dialog!\n\
                \ Please enter default Global values now, you can change all settings later\n\
                \ in " ++ show file ++ " or via settings command\n\
                \Select Default by leaving empty\n") (askAll empty) (take 2 dialogFields)


dialogFields :: [(String, String)]
dialogFields =
  [("artist"     , "Artist Name"                 ),
   ("genre"      , "Common Genre"                ),
   ("album"      , "Album Name"                  ),
   ("description", "YouTube Description Textfile"),
   ("cover"      , "Cover Image"                 ),
   ("video"      , "Album Video"                 )]

findDefaults :: IO Fields
findDefaults = fromList <$>
     filterPair [("cover", findFile ImageFile "cover")     ,
                 ("video", findFile VideoFile "video")     ,
                 ("desc" , findFile TextFile "description"),
                 ("album" , Just <$> takeFileName <$> getCurrentDirectory )]

getSettings :: Dialog -> IO Settings
getSettings askFor =
  do configFile <- (flip combine) "defaults.json" <$> configDir
     global     <- getMakeSettings configFile (globalDialog configFile)
     autofields <- findDefaults
     let defaults = union global autofields

     local      <- getMakeSettings "ppconf.json" (settingsDialog "Configure Album!\n" (askFor defaults) dialogFields)
     return $ Settings ["."] (union local defaults)
