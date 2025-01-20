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
import Data.Aeson (FromJSON, ToJSON, decode, encode, eitherDecode)
import Files (FileType(ImageFile), searchFile)
import System.FilePath (takeDirectory, combine)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (catMaybes, isJust, mapMaybe)
import GHC.IO (throwIO)
import Prelude hiding (lookup)
import Control.Exception (Exception)

type Settings = Map String String

newtype CorruptedConfig = CorruptedConfig String deriving (Show)

instance Exception CorruptedConfig

-- deriving instance Generic Settings

-- instance ToJSON Settings
-- instance FromJSON Settings

findFile:: FileType -> String -> IO (Maybe FilePath)
findFile t n = do dir   <- getCurrentDirectory
                  files <- listDirectory dir
                  return $ searchFile files t n

fields :: [(String, String)]
fields =
  [("artist"     , "Artist Name"                   ),
   ("genre"      , "Common Genre"                  ),
   ("description", "Description for Youtube Upload"),
   ("cover"      , "Cover Image"                   ),
   ("video"      , "Album Video"                   )]



settingsDialog :: String -> ((String, String) -> IO (Maybe (String, String)))
                -> [(String, String)] -> IO (Settings)
settingsDialog msg ask fields =
  do putStrLn msg
     res <- sequence $ map ask fields
     return . fromList $ catMaybes res


getMakeSettings :: FilePath -> IO Settings -> IO Settings
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

askAll :: Settings -> (String, String) -> IO (Maybe (String, String))
askAll answered (name, desc) =
  do putStr $ desc ++ " (default: "++ show (lookup name answered) ++ ")" ++ ": "
     answer <- getLine
     putStrLn $ "."
     return $ if null answer then Nothing else Just (name, answer)

askNew :: Settings  -> (String, String) -> IO (Maybe (String, String))
askNew answered (name, desc) =
        if member name answered then return $ Just (name, answered!name)
        else askAll answered (name, desc)


configDir :: IO FilePath
configDir = getXdgDirectory XdgConfig "ppublihs"

filterTuples :: [(a, Maybe b)] -> [(a,b)]
filterTuples = mapMaybe f
  where f (a, Just b) = Just (a, b)
        f (_, Nothing) = Nothing

getSettings :: IO Settings
getSettings =
  do configFile <- (flip combine) "defaults.json" <$> configDir
     global <- getMakeSettings configFile (globalDialog configFile)
     cover <- findFile ImageFile "cover"
     video <- findFile VideoFile "video"
     let defaults = union global (fromList $ ("trackDirs", "."):
                                  (filterTuples [("cover", cover), ("video", video)]))
     local <- getMakeSettings "ppconf.json" (localDialog defaults)
     return local

        where globalDialog file = settingsDialog
                ("PPublihs Setup Dialog!\n\
                \ Please enter default Global values now, you can change all settings later\n\
                \ in " ++ show file ++ " or via settings command\n\
                \Select Default by leaving empty\n") (askAll empty) (take 3 fields)

              localDialog defaults = settingsDialog "Configure Album!\n" (askNew defaults) fields
