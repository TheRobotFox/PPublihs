{-# LANGUAGE MultiWayIf #-}
-- | Diff States

module Diff where
import State (LocalState)
import Files (File (md5, path))
import Data.List (elemIndex, findIndex)
import Control.Monad (liftM2)
import Data.Set (intersection)

-- Modules get current State + Events (from diffing with last State)
-- After events have been applied

data TrackEvent = Changed | New | Removed | Reorder Int | Rename FilePath;
data Event = Tracks [Maybe TrackEvent]
           | AlbumNameChanged
           | CoverChanged
           deriving Show

-- TODO
-- old <- Current ( new tracks are right ordered)
-- append unresolved from current as New Tracks / delete olds if not in current
diffTracks :: [File] -> [File] -> [(File, [TrackEvent])]

diffTracks :: [File] -> [File] -> [(File, Maybe TrackEvent)]
diffTracks from to = let sort = sortBy \(t) ->
  do same <- intersection from to
                        return same
  where fn (idx, file) =
          if lookup (liftM2 (,) path md5) == Just idx then Nothing -- exact file
          else case (lookup path, lookup md5) of
                (Just name, Just hash) -> if | hash==name -> Just $ Reorder name
                                             | hash==idx -> Just . Rename . path $ file
                                             | name==idx -> Just $ Changed
                (Just name, Nothing) -> Just . Reorder $ name
                (Nothing, Just n) -> TrackReorder n idx
                (Nothing, Nothing) -> New
            where lookup by = ((== by file) . by) `findIndex` from

-- diffState :: LocalState -> LocalState -> [Event]
-- diffState from to =
