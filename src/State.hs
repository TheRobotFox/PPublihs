-- | PPublihs State

module State where

import GHC.Generics
import System.Directory

data StateData = SString String | SFile File | SInt Integer | SFloat Float
data State = State{tracks::[File], extra::[(String, StateData)]} deriving (Generic, Show)

scanTracks::FilePath -> IO [File]
scanTracks path = fmap loadFile . filter ((==AudioFile) . fileType) . listDirectory

getAlbumName::IO String
getAlbumName = getCurrentDirectory

generateState::Context -> IO State
generateState path =
