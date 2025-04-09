{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
-- | FFMpeg Wrapper

module Render where
import GHC.Generics ( Generic )
import Track (Track (..), Metadata (..), Attr (..), File (..))
import Data.Map (lookup, toList)
import System.FilePath (replaceExtension, takeDirectory)
import System.Process (callCommand)
import Control.Monad.Trans.Reader ( ReaderT (runReaderT), ask )
import Control.Monad.Trans.Class (lift)
import Prelude hiding (lookup)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (createDirectoryIfMissing)
import Data.Char (toLower)
import Data.Aeson (FromJSON, ToJSON)

data Format = Mp3 | Wav | Flac | Mp4 deriving (Generic, Show)
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
data Env = Env{settings :: RenderSettings, task :: Task, tracks :: [Track], out :: FilePath}

getAdditionalSources :: ReaderT Env IO [(String, String)]
getAdditionalSources = do
  mtdt <- fmap (metadata . head . tracks) ask
  md <- fmap (supported . fst . settings) ask
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
    (trk:[]) -> "-i \"" ++ path trk ++ "\" -map "++ show offset ++":0"
    _ -> concatMap (flip (++) "\" " . (++) "-i \"" . path) trks ++ "-filter_complex \"" ++
          concatMap (flip (++) ":a:0]" . (++) "[" . show . (+ offset)) [0..length trks] ++
          "concat=n="++show (length trks)++":v=0:a=1[outa]\" -map \"[outa]\""

getOutput :: ReaderT Env IO FilePath
getOutput = do
  name <- fmap out ask
  fmt <- fmap (map toLower . show . fst . settings) ask
  return $ replaceExtension name fmt

-- getOutput :: ReaderT Env IO FilePath
-- getOutput = do
--   dir <- fmap outDir ask
--   cfg <- fmap settings ask
--   trks <- fmap tracks ask

--   lift $ createDirectoryIfMissing True dir
--   let name = case cfg of
--               (MergedRender _ _ _) -> flip (!) (Attr Album) . metadata . head $ trks
--               _ -> liftA2 (++) (concatMap (flip (++) ". " . flip (!) (Attr Nr)))
--                               (intercalate "_" . Prelude.map (flip (!) (Attr Title))) . Prelude.map metadata $ trks
--   return . combine dir $ name ++ "." ++ format cfg


getAttrName :: Attr -> String
getAttrName Nr = "track"
getAttrName Year = "date"
getAttrName a = map toLower . show $ a

getAttr :: Attr -> ReaderT Env IO FilePath
getAttr a = do
  mtdt <- fmap (metadata . head . tracks) ask
  let res = \attr -> "-metadata " ++ getAttrName a ++ "=\"" ++ attr ++ "\" "
  return . fromMaybe "" . fmap res . Data.Map.lookup (Attr a) $ mtdt

-- ffrender :: ReaderT Env IO FilePath
-- ffrender = do
--   mdFiles <- getAdditionalSources
--   src <- getSource $ length mdFiles
--   attrs <- (=<<) (fmap concat . mapM \case Attr a -> getAttr a;_-> return "") . fmap (supported . fst . settings) $ ask

--   out <- getOutput
--   lift . liftA2 (>>) putStrLn callCommand $ "ffmpeg " ++ concatMap fst mdFiles ++ src ++ " " ++ concatMap snd mdFiles ++ attrs ++ "\"" ++ out ++ "\""
--   return out

-- ffupdate :: FilePath -> ReaderT Env IO FilePath
-- ffupdate from = do
--   mdFiles <- getAdditionalSources
--   out <- getOutput
--   attrs <- (=<<) (fmap concat . mapM \case Attr a -> getAttr a;_-> return "") . fmap (supported . settings) $ ask
--   let tmp = replaceBaseName out "tmp"
--       audioIdx = show . length $ mdFiles
--   lift $ do
--     liftA2 (>>) putStrLn callCommand $
--       "ffmpeg " ++ concatMap fst mdFiles ++ "-i \"" ++ from ++ "\" "
--       ++ concatMap snd mdFiles ++ attrs ++ "-map "++ audioIdx ++":a:0 -map "++ audioIdx ++ ":v:0 -c copy \"" ++ tmp ++ "\" -y"
--     removeFile from
--   move tmp

-- move :: FilePath -> ReaderT Env IO FilePath
-- move from = do
--   to <- getOutput
--   lift $ do
--     putStrLn $ "Move " ++ from ++ " to " ++ to
--     renameFile from to
--     return to

render :: RenderSettings -> Task -> [Track] -> FilePath -> IO ()
render a b c d= flip runReaderT (Env a b c d) $ do
  additional <- getAdditionalSources
  source <- getSource . length $ additional
  outPath <- getOutput
  attrs <- (=<<) (fmap concat . mapM \case Attr a -> getAttr a;_-> return "") . fmap (supported . fst . settings) $ ask
  lift . createDirectoryIfMissing True . takeDirectory $ outPath

  lift . liftA2 (>>) putStrLn callCommand $ "ffmpeg "
    ++ concatMap fst additional ++ source ++ concatMap snd additional
    ++ attrs ++ "\"" ++ outPath ++ "\"" ++ " -y"
