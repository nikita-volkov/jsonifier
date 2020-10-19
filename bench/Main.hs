module Main where

import Prelude
import Gauge.Main
import qualified Main.Model as Model
import qualified Main.Jsonifier
import qualified Main.Aeson
import qualified Main.BufferBuilder as BufferBuilder
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Char8 as Char8ByteString
import qualified Jsonifier
import qualified Text.Builder as TextBuilder


main =
  do
    twitter1Data <- load "samples/twitter1.json"
    twitter10Data <- load "samples/twitter10.json"
    twitter100Data <- load "samples/twitter100.json"
    let
      twitter1000Data = mapResultsOfResult (concat . replicate 10) twitter100Data
      twitter10000Data = mapResultsOfResult (concat . replicate 10) twitter1000Data
      twitter100000Data = mapResultsOfResult (concat . replicate 10) twitter10000Data

    -- Ensure that encoders are correct
    test "jsonifier" encodeWithJsonifier twitter10Data
    test "buffer-builder" BufferBuilder.encodeResult twitter10Data
    test "aeson" encodeWithAeson twitter10Data

    -- Print out the data sizes of samples
    TextBuilder.putLnToStdOut $ let
      sampleDataSize =
        TextBuilder.dataSizeInBytesInDecimal ',' .
        Char8ByteString.length .
        encodeWithJsonifier
      sample sampleName sampleData =
        "- " <> TextBuilder.text sampleName <> ": " <>
        sampleDataSize sampleData
      in 
        "Input data sizes report:\n" <>
        sample "twitter with 1 objects" twitter1Data <> "\n" <>
        sample "twitter with 10 objects" twitter10Data <> "\n" <>
        sample "twitter with 100 objects" twitter100Data <> "\n" <>
        sample "twitter with 1,000 objects" twitter1000Data <> "\n" <>
        sample "twitter with 10,000 objects" twitter10000Data <> "\n" <>
        sample "twitter with 100,000 objects" twitter100000Data

    let
      benchLib :: NFData a => String -> (Model.Result -> a) -> Benchmark
      benchLib name encode =
        bgroup name [
          bench "1kB" (nf encode twitter1Data)
          ,
          bench "6kB" (nf encode twitter10Data)
          ,
          bench "60kB" (nf encode twitter100Data)
          ,
          bench "600kB" (nf encode twitter1000Data)
          ,
          bench "6MB" (nf encode twitter10000Data)
          ,
          bench "60MB" (nf encode twitter100000Data)
          ]
      in
        defaultMain [
          benchLib "jsonifier" encodeWithJsonifier,
          benchLib "aeson" encodeWithAeson,
          benchLib "lazy-aeson" encodeWithLazyAeson,
          benchLib "buffer-builder" BufferBuilder.encodeResult
          ]


load :: FilePath -> IO Model.Result
load fileName =
  Data.Aeson.eitherDecodeFileStrict' fileName
    >>= either fail return

mapResultsOfResult :: ([Model.Story] -> [Model.Story]) -> Model.Result -> Model.Result
mapResultsOfResult f a =
  a {Model.results = f (Model.results a)}

test name strictEncoder input =
  let encoding = strictEncoder input in
    case Data.Aeson.eitherDecodeStrict' encoding of
      Right decoding ->
        if decoding == input
          then
            return ()
          else
            fail ("Encoder " <> name <> " encodes incorrectly.\nOutput:\n" <> Char8ByteString.unpack encoding)
      Left err ->
        fail ("Encoder " <> name <> " failed: " <> err <> ".\nOutput:\n" <> Char8ByteString.unpack encoding)

encodeWithJsonifier =
  Jsonifier.toByteString . Main.Jsonifier.resultJson

encodeWithAeson =
  Data.ByteString.Lazy.toStrict . Data.Aeson.encode

encodeWithLazyAeson =
  Data.Aeson.encode
