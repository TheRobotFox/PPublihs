{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
-- | FFMpeg Wrapper

module Render where
import GHC.Generics ( Generic )
import Data.Aeson (FromJSON, ToJSON)
import Track (Track (..), Metadata (..), Attr (..), File (..))
import Data.Map ((!), Map, lookup, mapWithKey, toList)
import System.FilePath (combine, replaceBaseName)
import System.Process (callCommand)
import Control.Monad.Trans.Reader ( ReaderT (runReaderT), ask )
import Control.Monad.Trans.Class (lift)
import Data.List (intercalate)
import Prelude hiding (lookup)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (removeFile, renameFile, createDirectoryIfMissing)
import Data.Char (toLower)
import Data.Tuple (swap)

data Format = Mp3 | Wav | Flac | Mp4 deriving (Generic)

type RenderSettings = (Format, [(String, String)])

supported :: Format -> [Metadata]
supported Mp3 = [File Cover, Attr Artist, Attr Album, Attr Year, Attr Title, Attr Genre, Attr Nr]
supported Wav = []
supported Flac = [File Cover, Attr Artist, Attr Album, Attr Year, Attr Title, Attr Genre, Attr Nr]
supported Mp4 = [File Video, Attr Artist, Attr Album, Attr Year, Attr Title, Attr Genre, Attr Nr]

data Task = Render [String] | UpdateMetadata FilePath [String] | Move FilePath [String]

data Env = Env{settings :: RenderSettings, outDir :: FilePath, tracks :: [Track String]}

getAdditionalSources :: ReaderT Env IO [(String, String)]
getAdditionalSources = do
  mtdt <- fmap (metadata . head . tracks) ask
  md <- fmap (supported . settings) ask
  let mdFiles = mapMaybe (\case (File a, b)-> if (File a) `elem` md then Just (a,b) else Nothing;_->Nothing) . toList $ mtdt

  return . liftA2 (zipWith (,)) (map (\x-> "-i \""++x++"\" ") . snd) (map (uncurry getAdditional) . flip zip [0..] . fst) . unzip $ mdFiles

-- getAdditional :: File -> Int -> ReaderT Env IO String
getAdditional :: File -> Int -> [Char]
getAdditional Cover i = " -map " ++ show i ++ ":0 -id3v2_version 3 -metadata:s:v title=\"Album cover\" -metadata:s:v comment=\"Cover (front)\" "
getAdditional Video i = " -map " ++ show i ++ ":v:0 "
getAdditional _ _ = error "Not Implemented"

getSource :: Int -> ReaderT Env IO String
getSource offset = do
  trks <- fmap tracks ask

  return $ case trks of
    (trk:[]) -> "-i \"" ++ source trk ++ "\" -map "++ show offset ++":0"
    _ -> concatMap (flip (++) "\" " . (++) "-i \"" . source) trks ++ "-filter_complex \"" ++
          concatMap (flip (++) ":a:0]" . (++) "[" . show . (+ offset)) [0..length trks] ++
          "concat=n="++show (length trks)++":v=0:a=1[outa]\" -map \"[outa]\""


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

getAttrName :: Attr -> String
getAttrName Nr = "track"
getAttrName Year = "date"
getAttrName a = map toLower . show $ a

getAttr :: Attr -> ReaderT Env IO FilePath
getAttr a = do
  mtdt <- fmap (metadata . head . tracks) ask
  let res = \attr -> "-metadata " ++ getAttrName a ++ "=\"" ++ attr ++ "\" "
  return . fromMaybe "" . fmap res . Data.Map.lookup (Attr a) $ mtdt

ffrender :: ReaderT Env IO FilePath
ffrender = do
  mdFiles <- getAdditionalSources
  src <- getSource $ length mdFiles
  attrs <- (=<<) (fmap concat . mapM \case Attr a -> getAttr a;_-> return "") . fmap (supported . settings) $ ask

  out <- getOutput
  lift . liftA2 (>>) putStrLn callCommand $ "ffmpeg " ++ concatMap fst mdFiles ++ src ++ " " ++ concatMap snd mdFiles ++ attrs ++ "\"" ++ out ++ "\""
  return out

ffupdate :: FilePath -> ReaderT Env IO FilePath
ffupdate from = do
  mdFiles <- getAdditionalSources
  out <- getOutput
  attrs <- (=<<) (fmap concat . mapM \case Attr a -> getAttr a;_-> return "") . fmap (supported . settings) $ ask
  let tmp = replaceBaseName out "tmp"
      audioIdx = show . length $ mdFiles
  lift $ do
    liftA2 (>>) putStrLn callCommand $
      "ffmpeg " ++ concatMap fst mdFiles ++ "-i \"" ++ from ++ "\" "
      ++ concatMap snd mdFiles ++ attrs ++ "-map "++ audioIdx ++":a:0 -map "++ audioIdx ++ ":v:0 -c copy \"" ++ tmp ++ "\" -y"
    removeFile from
  move tmp

move :: FilePath -> ReaderT Env IO FilePath
move from = do
  to <- getOutput
  lift $ do
    putStrLn $ "Move " ++ from ++ " to " ++ to
    renameFile from to
    return to

render :: Map String (Track String) -> FilePath -> RenderSettings -> Task -> IO (String, FilePath)
render trkList out cfg task =
  (\(trk, path)->sequence . runReaderT path $ Env cfg out [trkList!trk]) . exec $ task
 
  where exec (Render trk) = (trk, ffrender)
        exec (UpdateMetadata from trk) = (trk, ffupdate from)
        exec (Move from trk) = (trk, move from)
