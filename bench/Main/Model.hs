module Main.Model
(
  Metadata(..),
  Geo(..),
  Story(..),
  Result(..)
)
where

import Prelude hiding (id)

data Metadata =
  Metadata {
    result_type :: Text
  }
  deriving (Eq, Show, Typeable, Data, Generic)

instance NFData Metadata

data Geo =
  Geo {
    type_       :: Text,
    coordinates :: (Double, Double)
  }
  deriving (Eq, Show, Typeable, Data, Generic)

instance NFData Geo

data Story =
  Story {
    from_user_id_str  :: Text,
    profile_image_url :: Text,
    created_at        :: Text,
    from_user         :: Text,
    id_str            :: Text,
    metadata          :: Metadata,
    to_user_id        :: Maybe Int64,
    text              :: Text,
    id                :: Int64,
    from_user_id      :: Int64,
    geo               :: Maybe Geo,
    iso_language_code :: Text,
    to_user_id_str    :: Maybe Text,
    source            :: Text
  }
  deriving (Show, Typeable, Data, Generic)

instance NFData Story

data Result =
  Result {
    results          :: [Story],
    max_id           :: Int64,
    since_id         :: Int64,
    refresh_url      :: Text,
    next_page        :: Text,
    results_per_page :: Int,
    page             :: Int,
    completed_in     :: Double,
    since_id_str     :: Text,
    max_id_str       :: Text,
    query            :: Text
  }
  deriving (Show, Typeable, Data, Generic)

instance NFData Result
