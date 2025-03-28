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
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Data.List (intercalate)
import Prelude hiding (lookup)
import Data.Set

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

data FFCtx = FFCtx{counter :: Int, selected :: [String], flags :: Set String, command :: String}
data FFBuilder m = FFBuilder{_runBuilder :: (FFCtx) -> (FFCtx, m)}

instance Functor FFBuilder where
    fmap :: (a -> b) -> FFBuilder a -> FFBuilder b
    fmap f (FFBuilder g) = FFBuilder $ \s -> fmap f . g $ s

instance Monad FFBuilder where
  (>>=) :: FFBuilder a -> (a -> FFBuilder b) -> FFBuilder b
  f >>= g = FFBuilder $ \s -> let (is, iv) = _runBuilder f s in _runBuilder (g iv) is

addCmd :: String -> FFBuilder ()
addCmd append = FFBuilder $ \c -> (c{command= command c ++ " " ++ append}, ())
addSrc :: FilePath -> FFBuilder ()
addSrc src = FFBuilder $ \c -> (c{counter=counter c + 1,
                                  command=command c ++" -i \""++ src ++"\""}, ())
select :: FFBuilder ()
select = FFBuilder $ \c-> (c{selected = show (counter c -1) : selected c }, ())
setFlag :: String -> FFBuilder ()
setFlag flag = FFBuilder $ \c ->
  let f = flags c
      a = if flag `elem` f then " --" ++ flag else "" in
    (c{flags = insert flag f, command = command c ++ a},())
   
concat :: FFBuilder ()
concat = FFBuilder $
  \(FFCtx i s f c) -> let nOut = "[o"++show i ++ "]" in
    (FFCtx (i+1) [nOut] f (c ++" \""++ fiterCat s ++ nOut++"\""),())
  where fiterCat inps = concatMap (flip (++) ":a:0]" . (++) "[" . show) inps ++
          "concat=n="++show (length inps)++":v=0:a=1"
mapSelected :: FFBuilder ()
mapSelected = FFBuilder $
  \c -> (c{selected = [], command = command c ++ mapOut (selected c)},())
  where mapOut = concatMap (flip (++) "\"" . (++) " -map \"")
    
runFFBuilder :: FilePath -> FFBuilder a -> String
runFFBuilder out b = command . fst . flip _runBuilder (FFCtx 0 [] empty "") $ b>>mapSelected>>addCmd ("\"" ++ out ++ "\"")

getOutput :: ReaderT Env IO FilePath
getOutput = do
  dir <- fmap outDir ask
  cfg <- fmap settings ask
  trks <- fmap tracks ask

  let name = case cfg of
        (MergedRender _ _ _) -> flip (!) (Attr Album) . metadata . head $ trks;
        _ -> liftA2 (++) (concatMap (flip (++) ". " . flip (!) (Attr Nr) . metadata))
                         (intercalate "_" . Prelude.map (flip (!) (Attr Title) . metadata)) trks;

  return . combine dir $ name ++ "." ++ format cfg


getMetadata :: Metadata -> ReaderT Env FFBuilder ()
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
-- getMetadata (File Video) = do
  

ffrender :: ReaderT Env IO FilePath
ffrender = do
  env <- ask
  out <- getOutput
  let cmd = runFFBuilder out . flip runReaderT env . sequence $
            [lift . mapM (flip select >> addSrc . source) . tracks $ env, -- insert Tracks
             
             ]
  
    
  src <- getSource
  lift . callCommand $ "ffmpeg " ++ src ++ out
  return out

ffupdate :: FilePath -> ReaderT Env IO FilePath
ffupdate from = do
  out <- fmap outDir ask
  src <- getSource
  lift $ do
    putStrLn $ "ffmpeg -i " ++ from ++ " -c copy -s " ++ show (metadata trk) ++ " -> " ++ out
    putStrLn $ "delete " ++ from
    return out

move :: FilePath -> ReaderT Env IO FilePath
move from = do
  out <- fmap outDir ask
  lift $ do
    putStrLn $ "move " ++ from ++ " " ++ out
    return out

render :: Map String (Track String) -> FilePath -> RenderSettings -> [Task] -> IO [(String, FilePath)]
render trkList outDir cfg@(SingleRender _ _ _) tasks =
  mapM ((sequence) (Env cfg outDir . return . (!) trkList . fst) . exec) tasks
 
  where exec (Render trk) = (trk, ffrender)
        exec (UpdateMetadata from trk) = (trk, ffupdate from)
        exec (Move from trk) = (trk, move from)
