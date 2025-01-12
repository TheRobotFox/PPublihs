{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | PPublihs State

module State where

import GHC.Generics ( Generic )
import System.Directory ( getCurrentDirectory, listDirectory)
import Files (File, FileType (..), loadFile, filterFiles, tryLoad, searchFile)
import System.FilePath (takeFileName)
import Data.Serialize (Serialize)
import Context ( Context(trackDirs) )
import Data.List (nub)
import Data.Map (Map, fromList)
import Control.Monad (msum)
import Data.Maybe (maybeToList, catMaybes)

data StateData  where
  SStr   :: String  -> StateData
  SFile  :: File    -> StateData
  SInt   :: Integer -> StateData
  SFloat :: Float   -> StateData
  deriving (Show, Generic)

instance Serialize StateData

data State = State{tracks::[File], extra::Map String StateData} deriving (Generic, Show)
instance Serialize State

loadAny:: FileType -> String -> [FilePath] -> IO (Maybe File)
loadAny t prefered alternatives = msum . map tryLoad $
  concat [(maybeToList . searchFile t prefered) alternatives, filterFiles t alternatives]

tryOpt :: (a -> StateData) -> String -> Maybe a -> Maybe (String, StateData)
tryOpt constr name (Just a) = Just (name, constr a)
tryOpt _ _ Nothing = Nothing

generateState:: Context -> IO State
generateState ctx = do
  dir <- getCurrentDirectory
  dirFiles <- listDirectory dir

  trackPaths <- nub . (filterFiles AudioFile . concat . (dirFiles:)) <$> mapM listDirectory (trackDirs ctx)
  trks <- mapM loadFile trackPaths

  cover <- loadAny ImageFile "cover" dirFiles
  video <- loadAny VideoFile "video" dirFiles
  descr <- loadAny TextFile "desc" dirFiles
  return $ State trks (fromList $ catMaybes
                       [Just ("albumName", SStr $ takeFileName dir),
                        tryFile "cover" cover,
                        tryFile "video" video,
                        tryFile "desc" descr
                        ])
    where tryFile = tryOpt SFile
