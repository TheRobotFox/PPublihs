{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -XDeriveGeneric -XDefaultSignatures #-}
-- | PPublihs File Definition

module Files where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import System.FilePath (takeExtension, takeFileName)
import Text.Read (readMaybe)
import System.Process ( readProcess )
import Data.Maybe ( isJust )
import Data.List.Split (splitOn)
import System.FilePath.Posix (dropExtension)
import Control.Exception (throwIO, catch, IOException, Exception)
import Data.Data (Typeable)
import GHC.Generics (Generic)

data ContentException = UnknownExtension | ReadAudioLength String | RunFFProbe String deriving (Show, Typeable)

instance Exception ContentException

data FileType = TextFile | VideoFile | ImageFile | AudioFile deriving Show
data Content = Text String | Video | Image | Audio (Maybe Integer) String Float deriving (Show,Eq)
data File = File{path::FilePath, md5::BS.ByteString} deriving (Show, Generic)

textExt = [".txt"]
textExt :: [String]
videoExt = [".mp4", ".avi", ".mpeg", ".mkv"]
imageExt = [".png", ".jpg", ".tiff", ".jpeg", ".webp", ".bmp", ".gif"]
audioExt = [".mp3", ".flac", ".wav", ".ogg"]

hasExt :: Foldable t => t String -> FilePath -> Bool
hasExt exts = (`elem` exts) . takeExtension

getAudioLength:: FilePath -> IO Float
getAudioLength filepath = (do probe <- readProcess "ffprobe" ["-i", filepath, "-show_entries", "format=duration", "-v", "quiet", "-of", "csv=p=0"] []
                              case readMaybe probe of
                                Just l -> return l
                                Nothing -> throwIO $ ReadAudioLength probe)
                          `catch` \(e :: IOException) -> throwIO (RunFFProbe (show e))


readAudioName::String -> Float -> Content
readAudioName filename = let splits = splitOn ". " filename
                             num    = readMaybe . head $ splits in
                if Prelude.length splits > 1 && isJust num then
                         Audio num (concat . tail $ splits) else Audio Nothing filename

fileType:: FilePath -> Maybe FileType
fileType filepath
 | ext `elem` textExt  = Just TextFile
 | ext `elem` audioExt = Just AudioFile
 | ext `elem` videoExt = Just VideoFile
 | ext `elem` imageExt = Just ImageFile
 | otherwise = Nothing
 where ext = takeExtension filepath

loadContent:: FileType -> FilePath -> IO Content
loadContent TextFile filepath = do
  text <- readFile filepath
  return $ Text text

loadContent AudioFile filepath = do
  len <- getAudioLength filepath
  return $ (readAudioName . takeFileName . dropExtension) filepath len

loadContent VideoFile _ = return Video
loadContent ImageFile _ = return Image

loadFile::FilePath -> IO File
loadFile filepath = do filedata <- BS.readFile filepath
                       return $ File filepath (MD5.hash filedata)
