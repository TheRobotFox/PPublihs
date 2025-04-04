{-# LANGUAGE ScopedTypeVariables #-}
-- | Manage Module Cache

module Module.Cache where
import Control.Exception (IOException)
import Control.Monad.Trans.Reader (ReaderT)

type CacheEntry = (FilePath, Checksum)

getCached :: ReaderT Env IO [String]
getCached = do
  modCache <- fmap (cache . state) ask
  oldCfg <- fmap (config . state) ask
  newCfg <- fmap newconfig ask

  dirty <- if oldCfg /= newCfg then do
      lift . putStrLn $ "Module Config has Changed, invalidating Cache!"
      return $ keys modCache
    else
      lift . fmap (keys . Map.filter (id)) . mapM (uncurry verifyCache) $ modCache

  when (length dirty > 0) $ lift . putStrLn $ "Cleaning dirty Cache Files: " ++ show dirty

  lift . mapM_ (moveJunk . fst . (modCache!)) $ dirty -- move Invalid File to Junk
  return . Data.List.filter (not . (`elem` dirty)) . keys $ modCache

  where verifyCache path cksm = (fmap ((/= cksm) . md5Str) . BS.readFile $ path)
            `catch` \(_ :: IOException)->putStrLn ("Could not read Track from Modcache") >> return True


matchCached :: ReaderT Env IO [(Maybe String, Maybe String)]
matchCached = do
  cached   <- getCached
  trkListPrev <- fmap (prevTrkList . state) ask
  trkList     <- fmap trackList ask

  let cachedTrksSrcs = filterWithKey (const . (`elem` cached)) trkListPrev
  return $ on matchSource (toList . Map.map source) cachedTrksSrcs trkList


getCacheEntry :: FilePath -> IO CacheEntry
getCacheEntry = sequence . liftA2 (,) id (fmap md5Str . BS.readFile)
