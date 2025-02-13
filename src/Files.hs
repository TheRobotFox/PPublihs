{-# LANGUAGE ScopedTypeVariables, BinaryLiterals #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -XDeriveGeneric -XDefaultSignatures #-}
-- | PPublihs File Definition

module Files where

import System.FilePath (takeExtension, takeFileName)
import Text.Read (readMaybe)
import System.Process ( readProcess )
import Data.Maybe ( isJust )
import Data.List.Split (splitOn)
import System.FilePath.Posix (dropExtension)
import Control.Exception (throwIO, catch, IOException, Exception)
import Data.Data (Typeable)
import GHC.Generics (Generic)
import System.Directory (getModificationTime, listDirectory)
import Data.Time.Clock (UTCTime)
import Control.Monad (liftM, liftM2)
import Data.List (find)
import Data.Char (toLower)
import Data.Aeson (FromJSON, ToJSON)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import Data.Bits
import Data.ByteString (ByteString)

data ContentException = UnknownExtension | ReadAudioLength String | RunFFProbe String deriving (Show, Typeable)

instance Exception ContentException

data FileType = TextFile | VideoFile | ImageFile | AudioFile deriving (Show,Eq)
data Content = Text String | Video | Image | Audio (Maybe Integer) String Float deriving (Show,Eq)
data File = File{path::FilePath, md5 :: String} deriving (Show, Generic)

instance FromJSON File
instance ToJSON File

getTypeExts :: FileType -> [String]
getTypeExts TextFile = [".txt"]
getTypeExts VideoFile = [".mp4", ".avi", ".mpeg", ".mkv"]
getTypeExts ImageFile = [".png", ".jpg", ".tiff", ".jpeg", ".webp", ".bmp", ".gif"]
getTypeExts AudioFile = [".mp3", ".flac", ".wav", ".ogg"]

isType :: FileType -> FilePath -> Bool
isType filetype = (`elem`  (getTypeExts filetype)) . takeExtension

getAudioLength:: FilePath -> IO Float
getAudioLength filepath = (do probe <- readProcess "ffprobe" ["-i", filepath, "-show_entries", "format=duration", "-v", "quiet", "-of", "csv=p=0"] []
                              case readMaybe probe of
                                Just l -> return l
                                Nothing -> throwIO $ ReadAudioLength probe)
                          `catch` \(e :: IOException) -> throwIO (RunFFProbe (show e))

readTrackNum :: String -> Maybe Int
readTrackNum name = let splits = splitOn ". " name in
  if length splits > 1 then readMaybe . head $ splits
                        else Nothing

readAudioName::String -> Float -> Content
readAudioName filename = let splits = splitOn ". " filename
                             num    = readMaybe . head $ splits in
                if Prelude.length splits > 1 && isJust num then
                         Audio num (concat . tail $ splits) else Audio Nothing filename

fileType:: FilePath -> Maybe FileType
fileType filepath = find (((takeExtension filepath) `elem`) . getTypeExts)
        [TextFile, AudioFile, VideoFile, ImageFile]

loadContent:: FileType -> FilePath -> IO Content
loadContent TextFile filepath = do
  text <- readFile filepath
  return $ Text text

loadContent AudioFile filepath = do
  len <- getAudioLength filepath
  return $ (readAudioName . takeFileName . dropExtension) filepath len

loadContent VideoFile _ = return Video
loadContent ImageFile _ = return Image

filterFiles :: FileType -> [FilePath] -> [FilePath]
filterFiles ft = filter ((==Just ft) . fileType)

searchFile :: [FilePath] -> FileType -> String -> Maybe FilePath
searchFile dir t name = find (liftM2 (&&) ((==name) . map toLower . dropExtension . takeFileName)
                       ((==Just t) . fileType)) dir

md5Str :: ByteString -> String
md5Str = concatMap f . BS.unpack
  where
    f = liftM2 (++) (digit . fromIntegral . (.&. 15)) (digit . fromIntegral . (`shiftR` 4))
    digit i = [['a'..]!!i]

loadFile::FilePath -> IO File
loadFile filepath = do -- time <- getModificationTime filepath
                       content <- BS.readFile filepath
                       return $ File filepath (md5Str $ MD5.hash content)

tryLoad :: FilePath -> IO (Maybe File)
tryLoad path = (Just <$> loadFile path)
  `catch` \(_ :: IOException)->return Nothing
