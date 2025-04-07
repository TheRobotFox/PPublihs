{-# LANGUAGE MultiWayIf #-}
-- |

module Module.Folder where
import Control.Monad.Trans.Reader (ReaderT)
import Module.Env

bundleTracks :: Float -> Map String FilePath -> IO [[String]]
bundleTracks minLength trks =
  let relTime = mapM (fmap $ getAudioLength . source) . toList $ tracks
      absTime = mapAccumL (\s (trk, len)-> (s+len, trk)) in
  return . map (map snd) . groupBy ((> minLength) . on (-) fst) $ absTime

getCached :: ReaderT Env IO [[String]]
getCached = fmap (map lines . keys . cache) ask


metadataChanged :: String -> String -> ReaderT Env IO (Maybe Task)
metadataChanged prev new = do
  supportedMtdt <- fmap (supported . newconfig) ask
  trkA          <- fmap (flip (!) prev . prevTrkList . state) ask
  trkB          <- fmap (flip (!) new . trackList) ask
  modCache         <- fmap (cache . state) ask

  let prevPath = fst $ modCache!prev

  return $ if | matches supportedMtdt trkA trkB -> Just $ UpdateMetadata prevPath new
              | matches [Attr Title, Attr Nr] trkA trkB -> Just $ Move prevPath new
              | otherwise -> Nothing

 where matches mtdt = on (/=) (filterWithKey (const . (`elem` mtdt)). metadata)
  
getTask :: Maybe String -> Maybe String -> ReaderT Env IO (Maybe Task)
getTask (Just a) (Just b) = metadataChanged a b
getTask Nothing (Just b) = return . Just . Render $ b
getTask (Just a) Nothing = do
  file <- fmap (fst . flip (!) a . cache . state) ask
  lift . removeFile $ file
  return Nothing

