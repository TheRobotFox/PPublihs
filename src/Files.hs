-- | PPublihs File Definition

module Files where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS



data Content = Text String | Video | Audio track id stuff
data File = File{path::FilePath, md5::BS.ByteString} deriving Show

loadFile::FilePath -> IO File
loadFile filepath = do contents <- BS.readFile filepath
                       return $ File filepath (MD5.hash contents)
