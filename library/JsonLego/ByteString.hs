module JsonLego.ByteString
where

import JsonLego.Prelude
import Data.ByteString.Builder.Prim
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Builder.Scientific as ScientificBuilder


builderWithStrategy strategy builder =
  builder
    & Builder.toLazyByteStringWith strategy Lazy.empty
    & Lazy.toStrict

scientific :: Scientific -> ByteString
scientific sci =
  sci
    & ScientificBuilder.scientificBuilder
    & builderWithStrategy (Builder.untrimmedStrategy 128 128)
