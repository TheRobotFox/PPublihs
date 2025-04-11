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
import System.Directory (createDirectoryIfMissing, renameFile)
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
data Env = Env{settings :: RenderSettings, task :: Task, tracks :: [Track], out :: FilePath}

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
  task <- fmap task ask
  case task of
    Update Metadata from -> return $ "-i \""++ from ++ "\" -c:a copy "
      ++ concatMap ((++) (" -map " ++ show offset ++ ":") . return) "av" ++ " "
    Render -> do
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


render :: RenderSettings -> Task -> [Track] -> FilePath -> IO FilePath
render a b@(Update Path from) c d = flip runReaderT (Env a b c d) $ do
  output <- getOutput
  lift $ renameFile from output
  return output

render a b c d = flip runReaderT (Env a b c d) $ do
  additional <- getAdditionalSources
  source <- getSource . length $ additional
  outPath <- getOutput
  attrs <- (=<<) (fmap concat . mapM \case Attr a -> getAttr a;_-> return "") . fmap (supported . fst . settings) $ ask
  lift . createDirectoryIfMissing True . takeDirectory $ outPath

  lift . liftA2 (>>) putStrLn callCommand $ "ffmpeg "
    ++ concatMap fst additional ++ source ++ concatMap snd additional
    ++ attrs ++ "\"" ++ outPath ++ "\"" ++ " -y"
  return outPath
