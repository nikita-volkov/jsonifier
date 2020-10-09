module Main.JsonLego where

import Prelude hiding (null, bool)
import JsonLego
import qualified Main.Model as A


resultValue :: A.Result -> Value
resultValue A.Result{..} =
  object $
  row "results" (array (elements (fmap storyValue results))) <>
  row "max_id" ((intNumber . fromIntegral) max_id) <>
  row "since_id" ((intNumber . fromIntegral) since_id) <>
  row "refresh_url" (string refresh_url) <>
  row "next_page" (string next_page) <>
  row "results_per_page" ((intNumber . fromIntegral) results_per_page) <>
  row "page" ((intNumber . fromIntegral) page) <>
  row "completed_in" (doubleNumber completed_in) <>
  row "since_id_str" (string since_id_str) <>
  row "max_id_str" (string max_id_str) <>
  row "query" (string query)

storyValue :: A.Story -> Value
storyValue A.Story{..} =
  object $
  row "from_user_id_str" (string from_user_id_str) <>
  row "profile_image_url" (string profile_image_url) <>
  row "created_at" (string created_at) <>
  row "from_user" (string from_user) <>
  row "id_str" (string id_str) <>
  row "metadata" (metadataValue metadata) <>
  row "to_user_id" (maybe null (intNumber . fromIntegral) to_user_id) <>
  row "text" (string text) <>
  row "id" ((intNumber . fromIntegral) id) <>
  row "from_user_id" ((intNumber . fromIntegral) from_user_id) <>
  row "geo" (maybe null geoValue geo) <>
  row "iso_language_code" (string iso_language_code) <>
  row "to_user_id_str" (maybe null string to_user_id_str) <>
  row "source" (string source)

geoValue :: A.Geo -> Value
geoValue A.Geo{..} =
  object $
  row "type_" (string type_) <>
  row "coordinates" (coordinatesValue coordinates)

coordinatesValue :: (Double, Double) -> Value
coordinatesValue (x, y) =
  array (elements [doubleNumber x, doubleNumber y])

metadataValue :: A.Metadata -> Value
metadataValue A.Metadata{..} =
  object $
  row "result_type" (string result_type)
