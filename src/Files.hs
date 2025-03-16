{-# LANGUAGE ScopedTypeVariables, BinaryLiterals #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -XDeriveGeneric -XDefaultSignatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
-- | PPublihs File Definition

module Files (File(..), FileType (..), loadFile, filterFiles, searchFile, loadOrCreate, readMD5, moveJunk, TrackName(..), Checksum(..)) where

import System.FilePath (takeExtension, takeFileName, combine, takeDirectory)
-- import Text.Read (readMaybe)
-- import System.Process ( readProcess )
-- import Data.Maybe ( isJust )
-- import Data.List.Split (splitOn)
import System.FilePath.Posix (dropExtension)
import Control.Exception (catch, IOException, Exception)
import Data.Data (Typeable)
import GHC.Generics (Generic)
import Control.Monad (liftM2)
import Data.List (find)
import Data.Char (toLower)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Bits
import System.Directory (createDirectoryIfMissing, renameFile)

data ContentException = UnknownExtension | ReadAudioLength String | RunFFProbe String deriving (Show, Typeable)

instance Exception ContentException


newtype TrackName = TrackName String deriving (Show, Eq, Generic, Ord)
newtype Checksum = Checksum String  deriving (Show, Eq, Generic)

data FileType = TextFile | VideoFile | ImageFile | AudioFile deriving (Show,Eq)
data Content = Text String | Video | Image | Audio (Maybe Integer) String Float deriving (Show,Eq)
data File = File{path::FilePath, md5 :: Checksum} deriving (Show, Generic, Eq)

instance FromJSON Checksum
instance ToJSON Checksum

instance FromJSON TrackName
instance ToJSON TrackName

instance FromJSON File
instance ToJSON File

getTypeExts :: FileType -> [String]
getTypeExts VideoFile = [".mp4", ".avi", ".mpeg", ".mkv"]
getTypeExts ImageFile = [".png", ".jpg", ".tiff", ".jpeg", ".webp", ".bmp", ".gif"]
getTypeExts AudioFile = [".mp3", ".flac", ".wav", ".ogg"]
getTypeExts TextFile = [".txt"]

-- isType :: FileType -> FilePath -> Bool
-- isType filetype = (`elem`  (getTypeExts filetype)) . takeExtension

-- getAudioLength:: FilePath -> IO Float
-- getAudioLength filepath = (do probe <- readProcess "ffprobe" ["-i", filepath, "-show_entries", "format=duration", "-v", "quiet", "-of", "csv=p=0"] []
--                               case readMaybe probe of
--                                 Just l -> return l
--                                 Nothing -> throwIO $ ReadAudioLength probe)
--                           `catch` \(e :: IOException) -> throwIO (RunFFProbe (show e))

-- readTrackNum :: String -> Maybe Int
-- readTrackNum name = let splits = splitOn ". " name in
--   if length splits > 1 then readMaybe . head $ splits
--                         else Nothing

-- readAudioName::String -> Float -> Content
-- readAudioName filename = let splits = splitOn ". " filename
--                              num    = readMaybe . head $ splits in
--                 if Prelude.length splits > 1 && isJust num then
--                          Audio num (concat . tail $ splits) else Audio Nothing filename

fileType:: FilePath -> Maybe FileType
fileType filepath = find (((takeExtension filepath) `elem`) . getTypeExts)
        [TextFile, AudioFile, VideoFile, ImageFile]

-- loadContent:: FileType -> FilePath -> IO Content
-- loadContent TextFile filepath = do
--   text <- readFile filepath
--   return $ Text text

-- loadContent AudioFile filepath = do
--   len <- getAudioLength filepath
--   return $ (readAudioName . takeFileName . dropExtension) filepath len

-- loadContent VideoFile _ = return Video
-- loadContent ImageFile _ = return Image

filterFiles :: FileType -> [FilePath] -> [FilePath]
filterFiles ft = filter ((==Just ft) . fileType)

searchFile :: FileType -> String -> [FilePath] -> Maybe FilePath
searchFile t name = find (liftM2 (&&) ((==name) . map toLower . dropExtension . takeFileName)
                       ((==Just t) . fileType))

md5Str :: BS.ByteString -> Checksum
md5Str = Checksum . concatMap f . BS.unpack
  where
    f = liftM2 (++) (digit . fromIntegral . (.&. 15)) (digit . fromIntegral . (`shiftR` 4))
    digit i = [['a'..]!!i]

readMD5::FilePath -> IO Checksum
readMD5 filepath = do -- time <- getModificationTime filepath
                       content <- BS.readFile filepath
                       return (md5Str . MD5.hash $ content)

loadFile :: FilePath -> IO (Maybe File)
loadFile filePath = (fmap (Just . File filePath) . readMD5 $ filePath)
  `catch` \(_ :: IOException)->return Nothing

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
  `catch` (\(e :: IOException)->createFile file insert)
