module Main.JsonBytesBuilder where

import Prelude hiding (null)
import JSONBytesBuilder.Builder
import qualified Main.Model as A


result :: A.Result -> Literal
result x =
  object $
  row "results" (array (foldMap (element . story) (A.results x))) <>
  row "max_id" ((numberFromInt . fromIntegral) (A.max_id x)) <>
  row "since_id" ((numberFromInt . fromIntegral) (A.since_id x)) <>
  row "refresh_url" (stringFromText (A.refresh_url x)) <>
  row "next_page" (stringFromText (A.next_page x)) <>
  row "results_per_page" ((numberFromInt . fromIntegral) (A.results_per_page x)) <>
  row "page" ((numberFromInt . fromIntegral) (A.page x)) <>
  row "completed_in" (numberFromDouble (A.completed_in x)) <>
  row "since_id_str" (stringFromText (A.since_id_str x)) <>
  row "max_id_str" (stringFromText (A.max_id_str x)) <>
  row "query" (stringFromText (A.query x))

story :: A.Story -> Literal
story x =
  object $
  row "from_user_id_str" (stringFromText (A.from_user_id_str x)) <>
  row "profile_image_url" (stringFromText (A.profile_image_url x)) <>
  row "created_at" (stringFromText (A.created_at x)) <>
  row "from_user" (stringFromText (A.from_user x)) <>
  row "id_str" (stringFromText (A.id_str x)) <>
  row "metadata" (metadata (A.metadata x)) <>
  row "to_user_id" (maybe null (numberFromInt . fromIntegral) (A.to_user_id x)) <>
  row "text" (stringFromText (A.text x)) <>
  row "id" ((numberFromInt . fromIntegral) (A.id x)) <>
  row "from_user_id" ((numberFromInt . fromIntegral) (A.from_user_id x)) <>
  row "geo" (maybe null geo (A.geo x)) <>
  row "iso_language_code" (stringFromText (A.iso_language_code x)) <>
  row "to_user_id_str" (maybe null stringFromText (A.to_user_id_str x)) <>
  row "source" (stringFromText (A.source x))

geo :: A.Geo -> Literal
geo x =
  object $
  row "type_" (stringFromText (A.type_ x)) <>
  row "coordinates" (coordinates (A.coordinates x))

coordinates :: (Double, Double) -> Literal
coordinates (x, y) =
  array (element (numberFromDouble x) <> element (numberFromDouble y))

metadata :: A.Metadata -> Literal
metadata x =
  object $
  row "result_type" (stringFromText (A.result_type x))
