module Main where

import Prelude
import Gauge.Main
import qualified Main.Model
import qualified Main.JsonBytesBuilder
import qualified Main.JsonLego
import qualified Main.Aeson
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Char8 as Char8ByteString
import qualified JSONBytesBuilder.ByteString.ByteString
import qualified JSONBytesBuilder.ByteString.LazyByteString
import qualified JsonLego


main =
  do
    input <- load "samples/twitter100.json"

    -- Ensure that encoders are correct
    test "json-lego" encodeWithJsonLego input
    test "aeson" encodeWithAeson input

    deepseq input $ defaultMain [
      bgroup "strict" [
        bench "json-lego" (nf encodeWithJsonLego input)
        ,
        bench "aeson" (nf encodeWithAeson input)
        ]
      ,
      bgroup "lazy" [
        bench "json-lego" (nf (Data.ByteString.Lazy.fromStrict . encodeWithJsonLego) input)
        ,
        bench "aeson" (nf Data.Aeson.encode input)
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
