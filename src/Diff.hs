-- | Diff States


module Diff (getEvents) where
import State (LocalState (tracks, albumName, description, trackOrder), cover, video)
import Files (File (..))
import Data.List (find, elemIndex)
import Control.Monad (liftM2)
import Data.Function (on)
import Data.Foldable (msum)
import Data.Maybe (catMaybes, mapMaybe)

-- Modules get current State + Events (from diffing with last State)
-- After events have been applied

data TrackEvent = Changed | New | Removed | RenameFrom FilePath | ReorderedFrom Int deriving Show;
data Event = Track FilePath TrackEvent
           | AlbumNameChanged
           | CoverChanged
           | DescriptionChanged
           | VideoChanged
           deriving Show


matchTrack :: [File] -> File -> Maybe File
matchTrack trackList track = do
  msum $ map matchBy [path,md5]
        where matchBy fn = find (on (==) fn track) trackList

trackEvent :: File -> File -> TrackEvent
trackEvent old new =
        if on (==) path old new then Changed else RenameFrom $ path new

tracksEvents :: [File] -> [File] -> [(FilePath, TrackEvent)]
tracksEvents [] added = map (flip (,) New . path) added
tracksEvents rmd [] = map (flip (,) Removed. path) rmd
tracksEvents (old:os) new =
  case matchTrack new old of
        Just track -> liftM2 (,) path (trackEvent old) track:tracksEvents os (filter (== track) new)
        Nothing -> (path old, Removed):tracksEvents os new

trackListReorder :: [FilePath] -> [FilePath] -> [Event]
trackListReorder old new = mapMaybe changed $ zip [1..] new
  where
    changed :: (Int, FilePath) -> Maybe Event
    changed (idx_now, name) = do
          prev <- name `elemIndex` old
          if prev/=idx_now then return $ Track name (ReorderedFrom prev)
            else Nothing


getEvents :: LocalState -> LocalState -> [Event]
getEvents old new = liftM2 (++) (map (uncurry Track) . uncurry (on tracksEvents tracks))
                                (uncurry (on trackListReorder trackOrder)) (old, new)
                 ++ on trackListReorder trackOrder old new
                 ++ catMaybes [(changed albumName AlbumNameChanged),
                               (changed cover CoverChanged),
                               (changed video VideoChanged),
                               (changed description DescriptionChanged)]

        where changed field event = if on (==) field old new then Just event else Nothing
