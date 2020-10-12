module Main.JsonLego where

import Prelude hiding (null, bool)
import JsonLego
import qualified Main.Model as A


resultValue :: A.Result -> Value
resultValue A.Result{..} =
  object $
  row "results" (array (elements (fmap storyValue results))) <>
  row "max_id" (int64Number max_id) <>
  row "since_id" (int64Number since_id) <>
  row "refresh_url" (string refresh_url) <>
  row "next_page" (string next_page) <>
  row "results_per_page" (int64Number results_per_page) <>
  row "page" (int64Number page) <>
  row "completed_in" (doubleNumber completed_in) <>
  row "since_id_str" (string since_id_str) <>
  row "max_id_str" (string max_id_str) <>
  row "query" (string query)

storyValue :: A.Story -> Value
storyValue A.Story{..} =
  object $ rows [
    ("from_user_id_str", string from_user_id_str),
    ("profile_image_url", string profile_image_url),
    ("created_at", string created_at),
    ("from_user", string from_user),
    ("id_str", string id_str),
    ("metadata", metadataValue metadata),
    ("to_user_id", maybe null int64Number to_user_id),
    ("text", string text),
    ("id", int64Number id),
    ("from_user_id", int64Number from_user_id),
    ("geo", maybe null geoValue geo),
    ("iso_language_code", string iso_language_code),
    ("to_user_id_str", maybe null string to_user_id_str),
    ("source", string source)
    ]

geoValue :: A.Geo -> Value
geoValue A.Geo{..} =
  object $
  row "type_" (string type_) <>
  row "coordinates" (coordinatesValue coordinates)

coordinatesValue :: (Double, Double) -> Value
coordinatesValue (x, y) =
  array $
  element (doubleNumber x) <>
  element (doubleNumber y)

metadataValue :: A.Metadata -> Value
metadataValue A.Metadata{..} =
  object $
  row "result_type" (string result_type)
