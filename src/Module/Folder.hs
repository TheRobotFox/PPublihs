-- |

module Module.Folder where

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

bundleTracks :: ReaderT TrackList IO [[Track String]]
bundleTracks = do
  let track/relTime = map (fmap $ getAudioLength . source) . toList $ tracks
      track/absTime = mapAccumL (\s (trk, len)-> (s+len, trk))




sync :: ([Task] -> IO [(String,FilePath)]) -> ReaderT Env IO (Map String (FilePath, Checksum))
sync (SingleRender _ _ _) render = do
  matches  <- matchCached
  tasks    <- fmap catMaybes . mapM (uncurry getTask) $ matches
  outputs  <- lift $ render tasks
  cacheNew <- lift . fmap fromList . mapM (sequence . fmap trackCacheEntry) $ outputs
  keep <- fmap (map (fromMaybe (error "") . fst)) . filterM (uncurry $ isunchanged) $ matches
  cacheKeep <- fmap (filterWithKey (const . (`elem` keep)) . cache . state) ask
  return $ union cacheNew cacheKeep

  where
        getTask :: Maybe String -> Maybe String -> ReaderT Env IO (Maybe Task)
        getTask (Just a) (Just b) = metadataChanged a b
        getTask Nothing (Just b) = return . Just . Render $ b
        getTask (Just a) Nothing = do
          file <- fmap (fst . flip (!) a . cache . state) ask
          lift . removeFile $ file
          return Nothing

        isunchanged (Just a) (Just b) = fmap isNothing . metadataChanged a $ b
        isunchanged _ _ = return False
