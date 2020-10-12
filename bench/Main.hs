module Main where

import Prelude
import Gauge.Main
import qualified Main.Model
import qualified Main.JsonLego
import qualified Main.Aeson
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Char8 as Char8ByteString
import qualified JsonLego


main =
  do
    twitter10000Data <- load "samples/twitter10000.json"
    twitter1000Data <- load "samples/twitter1000.json"
    twitter100Data <- load "samples/twitter100.json"
    twitter10Data <- load "samples/twitter10.json"

    -- Ensure that encoders are correct
    test "json-lego" encodeWithJsonLego twitter100Data
    test "aeson" encodeWithAeson twitter100Data

    deepseq (twitter10000Data, twitter1000Data, twitter100Data, twitter10Data) $ defaultMain [
      bgroup "twitter10000" [
        bench "json-lego" (nf encodeWithJsonLego twitter10000Data)
        ,
        bench "aeson" (nf encodeWithAeson twitter10000Data)
        ]
      ,
      bgroup "twitter1000" [
        bench "json-lego" (nf encodeWithJsonLego twitter1000Data)
        ,
        bench "aeson" (nf encodeWithAeson twitter1000Data)
        ]
      ,
      bgroup "twitter100" [
        bench "json-lego" (nf encodeWithJsonLego twitter100Data)
        ,
        bench "aeson" (nf encodeWithAeson twitter100Data)
        ]
      ,
      bgroup "twitter10" [
        bench "json-lego" (nf encodeWithJsonLego twitter10Data)
        ,
        bench "aeson" (nf encodeWithAeson twitter10Data)
        ]
      ]

load :: FilePath -> IO Main.Model.Result
load fileName =
  Data.Aeson.eitherDecodeFileStrict' fileName
    >>= either fail return

test name strictEncoder input =
  let encoding = strictEncoder input in
    case Data.Aeson.eitherDecodeStrict' encoding of
      Right decoding ->
        if decoding == input
          then
            return ()
          else
            fail ("Encoder " <> name <> " encodes incorrectly")
      Left err ->
        fail ("Encoder " <> name <> " failed: " <> err <> ".\nOutput:\n" <> Char8ByteString.unpack encoding)

encodeWithJsonLego =
  JsonLego.value . Main.JsonLego.resultValue

encodeWithAeson =
  Data.ByteString.Lazy.toStrict . Data.Aeson.encode
