{-# LANGUAGE DeriveGeneric #-}

-- | Generic Configuration Dialogs
module ConfigDialog (Dialog(..), getConfig, AskFor (..)) where
import Data.Map (Map, member, lookup, (!), union, mapWithKey, mapMaybe, keys, elems, fromList)
import Prelude hiding (lookup)
import System.IO (hFlush, stdout)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Data.Aeson (FromJSONKey, ToJSONKey, ToJSON, FromJSON)
import Data.Maybe (fromMaybe)
import Files (tryLoad, createFile, Checksum (Checksum), md5Str)
import GHC.Generics (Generic)
import qualified Crypto.Hash.MD5 as MD5
import Data.Function (on)
import qualified Data.ByteString.Char8 as BC
import Control.Monad (guard)
import Paths_PPublihs (version)

-- Definitions
type Fields r = Map r String
data (Eq r, FromJSONKey r, ToJSONKey r, Ord r) => Dialog r = Dialog{
  configPath :: FilePath,
  questions :: Fields r,
  defaults :: Fields r
                                }

data AskFor = AskMissing | AskAll | AskStartup deriving Eq
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

dialogHash :: (FromJSONKey r, ToJSONKey r, Ord r, Show r) => ReaderT (Dialog r) IO Checksum
dialogHash = do
  dialog <- ask
  let k = foldl MD5.update MD5.init . map (BC.pack . show) $ liftA2 (on (++) keys) questions defaults dialog
      q = foldl MD5.update k . map BC.pack $ liftA2 (on (++) elems) questions defaults dialog
  return . md5Str . MD5.finalize $ q


loadStored :: (FromJSONKey r, ToJSONKey r, Ord r) => ReaderT (Dialog r) IO (Fields r, Checksum)
loadStored = do
  file <- fmap configPath ask
  loaded <- lift . tryLoad $ file
  return . fromMaybe (mempty, Checksum "") $ loaded

askFn :: Ord r => AskFor -> Asking r
askFn AskStartup = askMissing
askFn AskMissing = askMissing
askFn AskAll = askAll

getConfig :: (FromJSONKey r, ToJSONKey r, Ord r, Show r) => AskFor -> ReaderT (Dialog r) IO (Fields r)
getConfig askFor = do
  dialog <- ask

  (stored, ver) <- loadStored
  vHash <- dialogHash

  let present = union stored . defaults $ dialog
  if ver==vHash && askFor == AskStartup then
    return present
  else lift $ do
    answers <- configDialog (askFn askFor present) . questions $ dialog
    let res = union answers $ present
    createFile (configPath dialog) (res, vHash)
    return res
