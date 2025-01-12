{-# LANGUAGE DeriveGeneric #-}

-- | PPublihs Context
module Context where

import Files ( File )
import Data.Serialize (Serialize)
import GHC.Generics ( Generic )

instance Serialize File

data Context = Context{trackDirs::[FilePath], trackFiles::[FilePath]} deriving (Generic, Show)

instance Serialize Context
