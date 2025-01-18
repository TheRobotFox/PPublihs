{-# LANGUAGE DeriveGeneric #-}

-- | PPublihs Context
module Context where
import GHC.Generics (Generic)
import System.Dire
import Files (File)
import Data.Map (Map)
import System.Directory (getXdgDirectory, XdgDirectory (XdgConfig))
import Data.ByteString (readFile, getLine)
import Data.Aeson (FromJSON, ToJSON, decode, encode)

instance ToJson (File,State,Context)
instance FromJson (File,State,Context)

data LocalSettings where
  LocalSettings :: {
    albumName:: Maybe String,
    cover :: Maybe FilePath,
    video :: Maybe FilePath,
    description :: Maybe String,
    metadata :: Map String (Maybe String), -- TODO Hashmap
    trackDirs::[FilePath],
    trackFiles::[FilePath]
                } -> LocalSettings
  deriving (Generic, Show)

data GlobalSettings where
  GlobalSettings :: {
    artistName :: String,
    genre :: Maybe String,
    defaultMetadata :: Map String String,
    defaultDescription :: Maybe String
    -- Render Jobs
                    } -> GlobalSettings
  deriving (Generic, Show)

configDir = getXdgDirectory XdgConfig "ppublihs"
defualtsFile = configDir </> "defaults.json"

askDefaults :: IO Settings
askDefaults = do putStrLn "PPublihs has not been Setup!\n"
                          "Please enter default values now, you can change all settings\n"
                          "in " ++ defaultsFile ++ "\n"
                          "You can skip (optional Fields) with Empty Lines"
                 putStr "Artist Name: "
                 artist <- getLine
                 putStr "(common Genre): "
                 artist <- getLine

getGlobalSettings::IO Settings
getGlobalSettings = decode . readFile settingsPath

-- load global Settings
-- overwrite with local
