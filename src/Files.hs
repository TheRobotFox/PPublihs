{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
-- | PPublihs File Definition

module Files (FileType (..), filterFiles, searchFile, loadOrCreate, moveJunk, TrackName(..), Checksum(..), md5Str) where

import System.FilePath (takeExtension, takeFileName, combine, takeDirectory)
import System.FilePath.Posix (dropExtension)
import Control.Exception (IOException, Exception)
import Data.Data (Typeable)
import GHC.Generics (Generic)
import Control.Monad (liftM2)
import Data.List (find)
import Data.Char (toLower)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode, ToJSONKey, FromJSONKey)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Bits ( Bits(shiftR, (.&.)) )
import System.Directory (createDirectoryIfMissing, renameFile)
import Control.Monad.Catch (catch)

data ContentException = UnknownExtension | ReadAudioLength String | RunFFProbe String deriving (Show, Typeable)

instance Exception ContentException


newtype TrackName = TrackName String deriving (Show, Eq, Generic, Ord)
newtype Checksum = Checksum String  deriving (Show, Eq, Generic)

data FileType = TextFile | VideoFile | ImageFile | AudioFile deriving (Show,Eq)

instance FromJSON Checksum
instance ToJSON Checksum

instance ToJSONKey TrackName
instance FromJSONKey TrackName
instance FromJSON TrackName
instance ToJSON TrackName

getTypeExts :: FileType -> [String]
getTypeExts VideoFile = [".mp4", ".avi", ".mpeg", ".mkv"]
getTypeExts ImageFile = [".png", ".jpg", ".tiff", ".jpeg", ".webp", ".bmp", ".gif"]
getTypeExts AudioFile = [".mp3", ".flac", ".wav", ".ogg"]
getTypeExts TextFile = [".txt"]

-- getAudioLength:: FilePath -> IO Float
-- getAudioLength filepath = (do probe <- readProcess "ffprobe" ["-i", filepath, "-show_entries", "format=duration", "-v", "quiet", "-of", "csv=p=0"] []
--                               case readMaybe probe of
--                                 Just l -> return l
--                                 Nothing -> throwIO $ ReadAudioLength probe)
--                           `catch` \(e :: IOException) -> throwIO (RunFFProbe (show e))

fileType:: FilePath -> Maybe FileType
fileType filepath = find (((takeExtension filepath) `elem`) . getTypeExts)
        [TextFile, AudioFile, VideoFile, ImageFile]

filterFiles :: FileType -> [FilePath] -> [FilePath]
filterFiles ft = filter ((==Just ft) . fileType)

searchFile :: FileType -> String -> [FilePath] -> Maybe FilePath
searchFile t name = find (liftM2 (&&) ((==name) . map toLower . dropExtension . takeFileName)
                       ((==Just t) . fileType))

md5Str :: BS.ByteString -> Checksum
md5Str = Checksum . concatMap f . BS.unpack . MD5.hash
  where
    f = liftM2 (++) (digit . fromIntegral . (.&. 15)) (digit . fromIntegral . (`shiftR` 4))
    digit i = [(concat [['0'..'9'],['a'..]])!!i]

moveJunk :: FilePath -> IO ()
moveJunk file = createDirectoryIfMissing True "junk" >> renameFile file (combine "junk" file)

createFile :: ToJSON e => FilePath -> IO e -> IO e
createFile file insert = do
  createDirectoryIfMissing True (takeDirectory file)
  res <- insert
  BSL.writeFile file . encode $ res
  return res

loadOrCreate :: (ToJSON e, FromJSON e) => FilePath -> IO e -> IO e
loadOrCreate file insert = (do
 load <- BSL.readFile file
 case eitherDecode load of
   Right r -> return r
   Left err -> (putStrLn $ "File '"++file++"' is corrupted: "++err) >> putStrLn "Regenerating..." >> createFile file insert)
  `catch` (\(e :: IOException)->putStrLn ("Failed to open '"++file++"': " ++ show e) >> createFile file insert)
