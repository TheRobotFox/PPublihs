-- | Diff States

module Diff where
import State (LocalState)
import Files (File)
import Data.List (elemIndex)

data Event = TrackChanged FilePath
           | TrackNew FilePath
           | TrackRemoved Integer FilePath
           | TrackReorder Integer Integer
           | TrackRename FilePath FilePath
           | AlbumNameChanged
           | CoverChanged
           deriving Show



diffTracks :: [File] -> [File] -> [Event]
diffTracks from to = mapMaybe fn $ zip [1..] to
  where fn (idx, file) =
          case (lookup path, lookup md5) of
          (Just name, Just hash) ->
            if name==idx then Nothing
                         else if  $ path file

          (Nothing, Just n) -> TrackReorder n idx
          (Nothing, Nothing) -> TrackReorder n idx
          None -> a
            where lookup by = ((== by file) . by) `findIndex` from

-- diffState :: LocalState -> LocalState -> [Event]
-- diffState from to =
