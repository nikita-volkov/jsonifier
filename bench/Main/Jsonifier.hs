module Main.Jsonifier where

import Prelude hiding (null, bool)
import Jsonifier
import qualified Main.Model as A


resultJson :: A.Result -> Json
resultJson A.Result{..} =
  object [
    ("results", array (fmap storyJson results)),
    ("max_id", int64Number max_id),
    ("since_id", int64Number since_id),
    ("refresh_url", textString refresh_url),
    ("next_page", textString next_page),
    ("results_per_page", int64Number results_per_page),
    ("page", int64Number page),
    ("completed_in", doubleNumber completed_in),
    ("since_id_str", textString since_id_str),
    ("max_id_str", textString max_id_str),
    ("query", textString query)
    ]

storyJson :: A.Story -> Json
storyJson A.Story{..} =
  object [
    ("from_user_id_str", textString from_user_id_str),
    ("profile_image_url", textString profile_image_url),
    ("created_at", textString created_at),
    ("from_user", textString from_user),
    ("id_str", textString id_str),
    ("metadata", metadataJson metadata),
    ("to_user_id", maybe null int64Number to_user_id),
    ("text", textString text),
    ("id", int64Number id),
    ("from_user_id", int64Number from_user_id),
    ("geo", maybe null geoJson geo),
    ("iso_language_code", textString iso_language_code),
    ("to_user_id_str", maybe null textString to_user_id_str),
    ("source", textString source)
    ]

geoJson :: A.Geo -> Json
geoJson A.Geo{..} =
  object [
    ("type_", textString type_),
    ("coordinates", coordinatesJson coordinates)
    ]

coordinatesJson :: (Double, Double) -> Json
coordinatesJson (x, y) =
  array [
    doubleNumber x,
    doubleNumber y
    ]

metadataJson :: A.Metadata -> Json
metadataJson A.Metadata{..} =
  object [
    ("result_type", textString result_type)
    ]
