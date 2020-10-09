module Main where

import Prelude
import Gauge.Main
import qualified Main.Model
import qualified Main.JsonBytesBuilder
import qualified Main.JsonLego
import qualified Main.Aeson
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified JSONBytesBuilder.ByteString.ByteString
import qualified JSONBytesBuilder.ByteString.LazyByteString
import qualified JsonLego


main =
  do
    input <- load "samples/twitter100.json"
    defaultMain [
      bgroup "strict" [
        bench "json-lego" (nf (JsonLego.value . Main.JsonLego.resultValue) input)
        ,
        bench "aeson" (nf (Data.ByteString.Lazy.toStrict . Data.Aeson.encode) input)
        ,
        bench "json-bytes-builder" (nf (JSONBytesBuilder.ByteString.ByteString.jsonLiteral . Main.JsonBytesBuilder.result) input)
        ]
      ,
      bgroup "lazy" [
        bench "json-lego" (nf (Data.ByteString.Lazy.fromStrict . JsonLego.value . Main.JsonLego.resultValue) input)
        ,
        bench "aeson" (nf (Data.Aeson.encode) input)
        ,
        bench "json-bytes-builder" (nf (JSONBytesBuilder.ByteString.LazyByteString.jsonLiteral . Main.JsonBytesBuilder.result) input)
        ]
      ]

load :: FilePath -> IO Main.Model.Result
load fileName =
  (=<<) (either fail return . Data.Aeson.eitherDecode') $
  Data.ByteString.Lazy.readFile fileName
