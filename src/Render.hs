{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
-- | FFMpeg Wrapper

module Render where
import GHC.Generics ( Generic )
import Data.Aeson (FromJSON, ToJSON)
import Track (Track (..), Metadata (..), Attr (..), File (..))
import Data.Map ((!), Map, lookup)
import System.FilePath (combine)
import System.Process (callCommand)
import Control.Monad.Trans.Reader ( ReaderT (runReaderT), ask )
import Control.Monad.Trans.Class (lift)
import Data.List (intercalate)
import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import System.Directory (removeFile, renameFile, createDirectoryIfMissing)

data RenderSettings = MergedRender{ -- TODO No seperation
    supported :: [Metadata],
    filters :: [String],
    format :: String
  } | SingleRender{
    supported :: [Metadata],
    filters :: [String],
    format :: String
  } deriving (Generic, Eq)

instance FromJSON RenderSettings
instance ToJSON RenderSettings

data Task = Render String | UpdateMetadata FilePath String | Move FilePath String

data Env = Env{settings :: RenderSettings, outDir :: FilePath, tracks :: [Track String]}

getSource :: ReaderT Env IO String
getSource = do
  trks <- fmap tracks ask
  return $ case trks of
    (trk:[]) -> "-i " ++ show (source trk) ++ " -map 0:0"
    _ -> concatMap (flip (++) " " . (++) "-i " . show . source) trks ++
          concatMap (flip (++) ":a:0]" . (++) "[" . show) [0..length trks] ++
          "concat=n="++show (length trks)++":v=0:a=1[outa] -map \"[outa]\""


getOutput :: ReaderT Env IO FilePath
getOutput = do
  dir <- fmap outDir ask
  cfg <- fmap settings ask
  trks <- fmap tracks ask

  lift $ createDirectoryIfMissing True dir
  let name = case cfg of
              (MergedRender _ _ _) -> flip (!) (Attr Album) . metadata . head $ trks
              _ -> liftA2 (++) (concatMap (flip (++) ". " . flip (!) (Attr Nr)))
                              (intercalate "_" . Prelude.map (flip (!) (Attr Title))) . Prelude.map metadata $ trks
  return . combine dir $ name ++ "." ++ format cfg


getMetadata :: Metadata -> ReaderT Env IO FilePath
getMetadata (File Cover) = do
  trks <- fmap tracks ask
  return $ case lookup (File Cover) . metadata . head $ trks of
    (Just cover ) -> "-i " ++ cover ++ " -map " ++ show (length trks)
         ++ ":0 -id3v2_version 3 -metadata:s:v title=\"Album cover\" -metadata:s:v comment=\"Cover (front)\" "
    Nothing -> ""
getMetadata (File Video) = do
  trks <- fmap tracks ask
  return $ case lookup (File Video) . metadata . head $ trks of
    (Just video ) -> "-i " ++ video ++ " -map " ++ show (length trks)
         ++ ":v:0  "
    Nothing -> ""
getMetadata (Attr a) = do
  mtdt <- fmap (metadata . head . tracks) ask
  let res = \attr -> "-metadata:s " ++ show a ++ "=" ++ show attr ++ " "
  return . fromMaybe "" . fmap res . Data.Map.lookup (Attr a) $ mtdt
  

ffrender :: ReaderT Env IO FilePath
ffrender = do
  src <- getSource
  mtdt <- (=<<) (fmap concat . mapM getMetadata) . fmap (supported . settings) $ ask
  out <- getOutput
  lift . liftA2 (>>) putStrLn callCommand $ "ffmpeg " ++ src ++ " " ++ mtdt ++ " " ++ show out
  return out

ffupdate :: FilePath -> ReaderT Env IO FilePath
ffupdate from = do
  out <- fmap outDir ask
  mtdt <- (=<<) (fmap concat . mapM getMetadata) . fmap (supported . settings) $ ask
  lift $ do
    liftA2 (>>) putStrLn callCommand $ "ffmpeg -i " ++ show from ++ " -c copy " ++ mtdt ++ " " ++ show out
    removeFile from
    return out

move :: FilePath -> ReaderT Env IO FilePath
move from = do
  to <- fmap outDir ask
  lift $ do
    putStrLn $ "Move " ++ from ++ " to " ++ to
    renameFile from to
    return to

render :: Map String (Track String) -> FilePath -> RenderSettings -> [Task] -> IO [(String, FilePath)]
render trkList out cfg@(SingleRender _ _ _) tasks =
  mapM ((\(trk, path)->sequence (trk, runReaderT path $ Env cfg out [trkList!trk])) . exec) tasks
 
  where exec (Render trk) = (trk, ffrender)
        exec (UpdateMetadata from trk) = (trk, ffupdate from)
        exec (Move from trk) = (trk, move from)
