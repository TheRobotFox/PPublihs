{-# LANGUAGE MultiWayIf #-}
-- |

module Module.Folder where
import Control.Monad.Trans.Reader (ReaderT, ask)
import Module.Env
import Track (matchBackOn, Metadata (..), Track (..), getAudioLength, Attr (..), matchSource)
import Data.Tuple (uncurry)
import Render (Task (..), Update (..))
import Data.Map (Map, (!), keys, toList, filterWithKey)
import qualified Data.Map as Map
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT), hoistMaybe)
import Control.Monad.Trans.Class (lift)
import System.Directory (removeFile)
import Data.List (mapAccumL, groupBy, find, intercalate)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Control.Monad (join)
import System.FilePath (combine)

bundleTracks :: Float -> Map String FilePath -> IO [[String]]
bundleTracks minLength trks = do
  relTime <- mapM (sequence . fmap getAudioLength) . toList $ trks

  let absTime = snd . mapAccumL (\s (trk, len)-> liftA2 (,) (+len) ((,) trk) s) 0 $ relTime
  return . map (map fst) . groupBy (\a b -> (>=) minLength . on (-) snd a $ b) $ absTime

getCached :: ReaderT Env IO [[String]]
getCached = fmap (map lines . keys . cache) ask

getCachedFile :: [String] -> ReaderT Env IO FilePath
getCachedFile bundle = do
  c <- fmap cache ask
  return $ c!unlines bundle

metadataChanged :: [Metadata] -> String -> String -> ReaderT Env IO (Maybe Update)
metadataChanged supportedMtdt prev new = do
  trkA <- getTrack previousTrackList prev
  trkB <- getTrack trackList new

  return $ if | matches supportedMtdt trkA trkB -> Just Metadata
              | matches [Attr Title, Attr Nr] trkA trkB -> Just Path
              | otherwise -> Nothing
 where matches mtdt = on (/=) (filterWithKey (const . (`elem` mtdt)). metadata)
  
getTask :: [Metadata] -> Maybe [String] -> Maybe [String] -> ReaderT Env IO (Maybe (Task, [String]))
getTask mtdt (Just a) (Just b) = runMaybeT $ do
  update' <- lift . fmap maximum . mapM (uncurry $ metadataChanged mtdt) $ zip a b
  update <- hoistMaybe update'
  source <- lift $ getCachedFile a
  return (Update update source, b)
getTask _ Nothing (Just b) = return $ Just (Render, b)
getTask _ (Just a) Nothing = do
  file <- getCachedFile a
  lift . removeFile $ file
  return Nothing
getTask _ Nothing Nothing = error "Absurd Match"

matchBundles :: [[String]] -> [[String]] -> ReaderT Env IO [(Maybe [String], Maybe [String])]
matchBundles old new = do
  matched <- fmap (mapMaybe sequence . liftA2 (on matchSource (toList . Map.map cksm)) previousTrackList trackList) ask

  let translate :: [String] -> Maybe [String]
      translate = mapM (\trk -> join . fmap fst $ find ((== trk) . snd) matched)
      matchBy :: [[String]] -> [String] -> Maybe [String]
      matchBy prev b = do
        tb <- translate b
        find ((==) tb) prev

  return . matchBackOn matchBy old $ new

getOutput :: String -> [Track] -> FilePath
getOutput modName trks = combine modName . liftA2 (++) (concatMap (flip (++) ". " . flip (!) (Attr Nr)))
                                (intercalate "_" . map (flip (!) (Attr Title))) . map metadata $ trks

getCache :: Maybe [String] -> Maybe [String] -> ReaderT Env IO (Maybe (String, FilePath))
getCache _ (Just a) = do
  trks <- mapM (getTrack trackList) a
  modName <- fmap moduleName ask
  return $ Just (unlines a, getOutput modName trks)
getCache _ _ = return Nothing
