{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
-- | FFMpeg Wrapper

module Render where
import GHC.Generics ( Generic )
import Track (Track (..), Metadata (..), Attr (..), File (..))
import Data.Map (lookup, toList)
import System.FilePath (replaceExtension, takeDirectory, combine, takeFileName)
import System.Process (callCommand)
import Control.Monad.Trans.Reader ( ReaderT (runReaderT), ask )
import Control.Monad.Trans.Class (lift)
import Prelude hiding (lookup)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (createDirectoryIfMissing, renameFile, removeFile)
import Data.Char (toLower)
import Data.Aeson (FromJSON, ToJSON)

data Format = Mp3 | Wav | Flac | Mp4 deriving (Generic, Show, Eq)
instance FromJSON Format
instance ToJSON Format

type RenderSettings = (Format, [(String, String)])

supported :: Format -> [Metadata]
supported Mp3 = [File Cover, Attr Artist, Attr Album, Attr Year, Attr Title, Attr Genre, Attr Nr]
supported Wav = []
supported Flac = [File Cover, Attr Artist, Attr Album, Attr Year, Attr Title, Attr Genre, Attr Nr]
supported Mp4 = [File Video, Attr Artist, Attr Album, Attr Year, Attr Title, Attr Genre, Attr Nr]

data Update = Path | Metadata deriving (Ord, Eq)

data Task = Render | Update Update FilePath
data Env = Env{settings :: RenderSettings, tracks :: [Track], out :: FilePath}

getAdditionalSources :: ReaderT Env IO [(String, String)]
getAdditionalSources = do
  mtdt <- fmap (metadata . head . tracks) ask
  md <- fmap (supported . fst . settings) ask
  let mdFiles = mapMaybe (\case (File a, b)-> if (File a) `elem` md then Just (a,b) else Nothing;_->Nothing) . toList $ mtdt

  return . liftA2 (zipWith (,)) (map (\x-> "-i \""++x++"\" ") . snd) (map (uncurry getAdditional) . flip zip [0..] . fst) . unzip $ mdFiles

-- getAdditional :: File -> Int -> ReaderT Env IO String
getAdditional :: File -> Int -> [Char]
getAdditional Cover i = "-map " ++ show i ++ ":0 -id3v2_version 3 -metadata:s:v title=\"Album cover\" -metadata:s:v comment=\"Cover (front)\" "
getAdditional Video i = "-map " ++ show i ++ ":v:0 "
getAdditional _ _ = error "Not Implemented"

getSource :: Int -> ReaderT Env IO String
getSource offset = do
  trks <- fmap tracks ask
  flags <- getFlags

  return . flip (++) flags $ case trks of
    (trk:[]) -> "-i \"" ++ path trk ++ "\" -map "++ show offset ++":0 "
    _ -> concatMap (flip (++) "\" " . (++) "-i \"" . path) trks ++ "-filter_complex \"" ++
          concatMap (flip (++) ":a:0]" . (++) "[" . show . (+ offset)) [0..length trks-1] ++
          "concat=n="++show (length trks)++":v=0:a=1[outa]\" -map \"[outa]\" "

getOutput :: ReaderT Env IO FilePath
getOutput = do
  name <- fmap out ask
  fmt <- fmap (map toLower . show . fst . settings) ask
  return $ name ++ "." ++ fmt

getFlags :: ReaderT Env IO String
getFlags = do
  cfg <- fmap settings ask
  return . concatMap (uncurry makeFlag) . snd $ cfg
  where makeFlag p v = "-"++p++" \"" ++v ++"\" "


getAttrName :: Attr -> String
getAttrName Nr = "track"
getAttrName Year = "date"
getAttrName a = map toLower . show $ a

getAttr :: Attr -> ReaderT Env IO FilePath
getAttr a = do
  mtdt <- fmap (metadata . head . tracks) ask
  let res = \attr -> "-metadata " ++ getAttrName a ++ "=\"" ++ attr ++ "\" "
  return . fromMaybe "" . fmap res . Data.Map.lookup (Attr a) $ mtdt

xattrs :: [Metadata] -> [Attr]
xattrs = mapMaybe (\case Attr a -> Just a;_-> Nothing)

render :: RenderSettings -> Task -> [Track] -> FilePath -> IO FilePath
render a (Update Path from) c d = flip runReaderT (Env a c d) $ do
  output <- getOutput
  lift . putStrLn $ "Renameing: " ++ from ++ " -> " ++ output
  lift $ renameFile from output
  return output

render a (Update Metadata from) c d = flip runReaderT (Env a c d) $ do
  let tmp = liftA2 combine takeDirectory ((++) "_tmp_" . takeFileName) from
  lift $ renameFile from tmp
  additional <- getAdditionalSources
  attrs <- (=<<) (fmap concat . mapM getAttr) . fmap (xattrs . supported . fst . settings) $ ask
  outPath <- getOutput
  let cmd = "ffmpeg "++ concatMap fst additional ++ "-i \""++ tmp ++ "\" -c:a copy"
          ++ concatMap ((++) $ " -map " ++ show (length additional) ++ ":" ) ["a", "v"] ++ " "
          ++ concatMap snd additional ++ attrs ++ " -y " ++ "\"" ++ outPath ++ "\""

  lift . createDirectoryIfMissing True . takeDirectory $ outPath
  lift . liftA2 (>>) putStrLn callCommand $ cmd
  lift $ removeFile tmp
  return outPath

render a Render c d = flip runReaderT (Env a c d) $ do
  attrs <- (=<<) (fmap concat . mapM getAttr) . fmap (xattrs . supported . fst . settings) $ ask
  outPath <- getOutput
  additional <- getAdditionalSources
  source <- getSource . length $ additional
  let cmd = "ffmpeg " ++ concatMap fst additional ++ source
          ++ concatMap snd additional ++ attrs ++ " -y " ++ "\"" ++ outPath ++ "\""

  lift . createDirectoryIfMissing True . takeDirectory $ outPath
  lift . liftA2 (>>) putStrLn callCommand $ cmd
  return outPath
