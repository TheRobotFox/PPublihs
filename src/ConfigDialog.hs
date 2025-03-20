{-# LANGUAGE DeriveGeneric #-}

-- | Generic Configuration Dialogs
module ConfigDialog (Dialog(..), getConfig, askAll, askMissing, Key, Asking) where
import Data.Map (Map, member, lookup, (!), union, mapWithKey, mapMaybe)
import Prelude hiding (lookup)
import System.IO (hFlush, stdout)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.Maybe (fromMaybe)
import Files (tryLoad)

-- Definitions
class (Eq r, FromJSONKey r, ToJSONKey r, Ord r) => Key r

type Fields r = Map r String
data Key r => Dialog r = Dialog{questions :: Fields r,
                                defaults :: Fields r}

-- Asking Callbacks
type Asking r = Fields r -> r -> String -> IO (Maybe String)

askAll :: Ord r =>Asking r
askAll answered name desc =
  do putStr $ desc ++ " (default: "++ show (lookup name answered) ++ ")" ++ ": "
     hFlush stdout
     answer <- getLine
     putStrLn $ "Ok!"
     return $ if null answer then Nothing else Just answer

askMissing :: Ord r => Asking r
askMissing answered name desc =
        if member name answered then return $ Just (answered!name)
        else askAll answered name desc

-- Implementation

configDialog :: (r -> String -> IO (Maybe String)) -> Fields r -> IO (Fields r)
configDialog askFor qust = do
  ans <- sequence . mapWithKey askFor $ qust
  return . mapMaybe id $ ans

getConfig :: Key r => String -> Asking r -> ReaderT (Dialog r) IO (Fields r)
getConfig file askFor = do
  dialog <- ask
  current <- lift . fmap (fromMaybe mempty) . tryLoad $ file

  let present = union current . defaults $ dialog

  res <- lift $ configDialog (askFor present) . questions $ dialog
  return . union res $ present
