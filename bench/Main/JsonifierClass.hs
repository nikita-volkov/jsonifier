module Main.JsonifierClass where

import Prelude hiding (null, bool)
import Jsonifier
import qualified Main.Model as A


instance ToJSON A.Result where
  toJson A.Result{..} =
    object [
      "results"          &= results,
      "max_id"           &= max_id,
      "since_id"         &= since_id,
      "refresh_url"      &= refresh_url,
      "next_page"        &= next_page,
      "results_per_page" &= results_per_page,
      "page"             &= page,
      "completed_in"     &= completed_in,
      "since_id_str"     &= since_id_str,
      "max_id_str"       &= max_id_str,
      "query"            &= query
      ]

instance ToJSON A.Story where
  toJson A.Story{..} =
    object [
      "from_user_id_str"    &= from_user_id_str,
      "profile_image_url"   &= profile_image_url,
      "created_at"          &= created_at,
      "from_user"           &= from_user,
      "id_str"              &= id_str,
      "metadata"            &= metadata,
      "to_user_id"          &= to_user_id,
      "text"                &= text,
      "id"                  &= id,
      "from_user_id"        &= from_user_id,
      "geo"                 &= geo,
      "iso_language_code"   &= iso_language_code,
      "to_user_id_str"      &= to_user_id_str,
      "source"              &= source
      ]


instance ToJSON A.Geo where
  toJson A.Geo{..} =
    object [
      "type_"       &= type_,
      "coordinates" &= coordinates
      ]

-- instance ToJSON (Double, Double) where
--   toJson (x, y) =
--     array [
--       toJson x,
--       toJson y
--       ]


instance ToJSON A.Metadata where
  toJson A.Metadata{..} =
    object [ "result_type" &= result_type ]
