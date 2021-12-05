module Main where

import qualified Data.Aeson
import qualified Data.ByteString.Char8 as Char8ByteString
import qualified Data.ByteString.Lazy
import Gauge.Main
import qualified Jsonifier
import qualified Main.Aeson
import qualified Main.BufferBuilder as BufferBuilder
import qualified Main.Jsonifier
import qualified Main.Model as Model
import qualified Text.Builder as TextBuilder
import Prelude

main =
  do
    twitter1Data <- load "samples/twitter1.json"
    twitter10Data <- load "samples/twitter10.json"
    twitter100Data <- load "samples/twitter100.json"
    let twitter1000Data = mapResultsOfResult (concat . replicate 10) twitter100Data
        twitter10000Data = mapResultsOfResult (concat . replicate 10) twitter1000Data
        twitter100000Data = mapResultsOfResult (concat . replicate 10) twitter10000Data

    -- Ensure that encoders are correct
    test "jsonifier" encodeWithJsonifier twitter10Data
    test "buffer-builder" BufferBuilder.encodeResult twitter10Data
    test "aeson" encodeWithAeson twitter10Data

    -- Print out the data sizes of samples
    TextBuilder.putLnToStdOut $
      let sampleDataSize =
            TextBuilder.dataSizeInBytesInDecimal ','
              . Char8ByteString.length
              . encodeWithJsonifier
          sample sampleName sampleData =
            "- " <> TextBuilder.text sampleName <> ": "
              <> sampleDataSize sampleData
       in "Input data sizes report:\n"
            <> sample "twitter with 1 objects" twitter1Data
            <> "\n"
            <> sample "twitter with 10 objects" twitter10Data
            <> "\n"
            <> sample "twitter with 100 objects" twitter100Data
            <> "\n"
            <> sample "twitter with 1,000 objects" twitter1000Data
            <> "\n"
            <> sample "twitter with 10,000 objects" twitter10000Data
            <> "\n"
            <> sample "twitter with 100,000 objects" twitter100000Data

    let benchInput :: String -> Model.Result -> Benchmark
        benchInput name input =
          bgroup
            name
            [ bench "jsonifier" (nf encodeWithJsonifier input),
              bench "aeson" (nf encodeWithAeson input),
              bench "lazy-aeson" (nf encodeWithLazyAeson input),
              bench "lazy-aeson-untrimmed-32k" (nf Main.Aeson.resultToLazyByteStringWithUntrimmedStrategy input),
              bench "buffer-builder" (nf BufferBuilder.encodeResult input)
            ]
     in defaultMain
          [ benchInput "1kB" twitter1Data,
            benchInput "6kB" twitter10Data,
            benchInput "60kB" twitter100Data,
            benchInput "600kB" twitter1000Data,
            benchInput "6MB" twitter10000Data,
            benchInput "60MB" twitter100000Data
          ]

load :: FilePath -> IO Model.Result
load fileName =
  Data.Aeson.eitherDecodeFileStrict' fileName
    >>= either fail return

mapResultsOfResult :: ([Model.Story] -> [Model.Story]) -> Model.Result -> Model.Result
mapResultsOfResult f a =
  a {Model.results = f (Model.results a)}

test name strictEncoder input =
  let encoding = strictEncoder input
   in case Data.Aeson.eitherDecodeStrict' encoding of
        Right decoding ->
          if decoding == input
            then return ()
            else fail ("Encoder " <> name <> " encodes incorrectly.\nOutput:\n" <> Char8ByteString.unpack encoding)
        Left err ->
          fail ("Encoder " <> name <> " failed: " <> err <> ".\nOutput:\n" <> Char8ByteString.unpack encoding)

encodeWithJsonifier =
  Jsonifier.toByteString . Main.Jsonifier.resultJson

encodeWithAeson =
  Data.ByteString.Lazy.toStrict . Data.Aeson.encode

encodeWithLazyAeson =
  Data.Aeson.encode
