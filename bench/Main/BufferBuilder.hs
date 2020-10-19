module Main.BufferBuilder where

import Prelude
import Data.BufferBuilder.Json
import qualified Main.Model as M


encodeResult :: M.Result -> ByteString
encodeResult =
  encodeJson

instance ToJson M.Geo where
  toJson M.Geo{..} =
    toJson $
      "type_"# .=# toJson type_ <>
      "coordinates"# .=# toJson coordinates

instance ToJson (Double, Double) where
  toJson (a, b) =
    toJson [toJson a, toJson b]

instance ToJson M.Story where
  toJson M.Story{..} =
    toJson $
      "from_user_id_str"# .=# toJson from_user_id_str <>
      "profile_image_url"# .=# toJson profile_image_url <>
      "created_at"# .=# toJson created_at <>
      "from_user"# .=# toJson from_user <>
      "id_str"# .=# toJson id_str <>
      "metadata"# .=# toJson metadata <>
      "to_user_id"# .=# toJson to_user_id <>
      "text"# .=# toJson text <>
      "id"# .=# toJson id <>
      "from_user_id"# .=# toJson from_user_id <>
      "geo"# .=# toJson geo <>
      "iso_language_code"# .=# toJson iso_language_code <>
      "to_user_id_str"# .=# toJson to_user_id_str <>
      "source"# .=# toJson source

instance ToJson M.Metadata where
  toJson M.Metadata{..} =
    toJson $
      "result_type"# .=# toJson result_type

instance ToJson M.Result where
  toJson M.Result{..} =
    toJson $
      "results"# .=# toJson results <>
      "max_id"# .=# toJson max_id <>
      "since_id"# .=# toJson since_id <>
      "refresh_url"# .=# toJson refresh_url <>
      "next_page"# .=# toJson next_page <>
      "results_per_page"# .=# toJson results_per_page <>
      "page"# .=# toJson page <>
      "completed_in"# .=# toJson completed_in <>
      "since_id_str"# .=# toJson since_id_str <>
      "max_id_str"# .=# toJson max_id_str <>
      "query"# .=# toJson query
