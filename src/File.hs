-- | PPublihs File Definition

module File where

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString (ByteString)

data File = File{path::FilePath, md5::ByteString} deriving Show

loadFile::FilePath -> Maybe File
loadFile path = do
                  contents <- readFile path
                  return File(path, MD5.hash contents)
