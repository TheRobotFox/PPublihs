{-# LANGUAGE DeriveGeneric #-}
-- | Tracks

module Track where
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, FromJSONKey, ToJSON, ToJSONKey )
import Data.Map (Map)


data Field = File String | Attr String
           deriving (Generic, Show, Eq, Ord)
instance ToJSONKey Field
instance FromJSONKey Field
instance ToJSON Field
instance FromJSON Field

type Metadata a = Map Field a


-- data Track = Track{name :: TrackName, metadata :: Fields String}


-- -- Settings Dialog Questions

-- -- Gloal SettingsDialog
-- globalFields :: [(Field, String)]
-- globalFields = take 2 dialogFields

-- getMetadata :: LocalState -> TrackName -> Fields String
-- getMetadata state trk = union (Env.metadata . state) . fromList $ [(Attr "track", trk `elemIndex` (tracks state)), -- TODO get metadata from Env
--                                                   (Attr "title", trk)]

-- metadataValid :: LocalState -> LocalState -> [Field] -> Bool
-- metadataValid prev now = and . map (on (liftA2 (==)) (flip lookup . Env.metadata) prev now)
