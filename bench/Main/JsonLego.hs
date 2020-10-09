module Main.JsonLego where

import Prelude hiding (null, bool)
import JsonLego
import qualified Main.Model as A


result :: A.Result -> Value
result x =
  object $
  row "results" (array (foldMap (element . story) (A.results x))) <>
  row "max_id" ((intNumber . fromIntegral) (A.max_id x)) <>
  row "since_id" ((intNumber . fromIntegral) (A.since_id x)) <>
  row "refresh_url" (string (A.refresh_url x)) <>
  row "next_page" (string (A.next_page x)) <>
  row "results_per_page" ((intNumber . fromIntegral) (A.results_per_page x)) <>
  row "page" ((intNumber . fromIntegral) (A.page x)) <>
  row "completed_in" (doubleNumber (A.completed_in x)) <>
  row "since_id_str" (string (A.since_id_str x)) <>
  row "max_id_str" (string (A.max_id_str x)) <>
  row "query" (string (A.query x))

story :: A.Story -> Value
story x =
  object $
  row "from_user_id_str" (string (A.from_user_id_str x)) <>
  row "profile_image_url" (string (A.profile_image_url x)) <>
  row "created_at" (string (A.created_at x)) <>
  row "from_user" (string (A.from_user x)) <>
  row "id_str" (string (A.id_str x)) <>
  row "metadata" (metadata (A.metadata x)) <>
  row "to_user_id" (maybe null (intNumber . fromIntegral) (A.to_user_id x)) <>
  row "text" (string (A.text x)) <>
  row "id" ((intNumber . fromIntegral) (A.id x)) <>
  row "from_user_id" ((intNumber . fromIntegral) (A.from_user_id x)) <>
  row "geo" (maybe null geo (A.geo x)) <>
  row "iso_language_code" (string (A.iso_language_code x)) <>
  row "to_user_id_str" (maybe null string (A.to_user_id_str x)) <>
  row "source" (string (A.source x))

geo :: A.Geo -> Value
geo x =
  object $
  row "type_" (string (A.type_ x)) <>
  row "coordinates" (coordinates (A.coordinates x))

coordinates :: (Double, Double) -> Value
coordinates (x, y) =
  array (element (doubleNumber x) <> element (doubleNumber y))

metadata :: A.Metadata -> Value
metadata x =
  object $
  row "result_type" (string (A.result_type x))
