module JsonLego.ByteString
where

import JsonLego.Prelude
import Data.ByteString.Builder.Prim
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as Builder
import qualified JsonLego.BoundedPrim as BoundedPrim
import qualified Data.ByteString.Builder.Scientific as ScientificBuilder


builderWithStrategy strategy builder =
  builder
    & Builder.toLazyByteStringWith strategy Lazy.empty
    & Lazy.toStrict

jsonStringBody :: Text -> ByteString
jsonStringBody text =
  text
    & TextEncoding.encodeUtf8BuilderEscaped BoundedPrim.stringEncodedByte
    & Builder.toLazyByteStringWith strategy Lazy.empty
    & Lazy.toStrict
  where
    strategy =
      Builder.untrimmedStrategy bufferSize bufferSize
      where
        bufferSize =
          Text.length text * 2

scientific :: Scientific -> ByteString
scientific sci =
  sci
    & ScientificBuilder.scientificBuilder
    & builderWithStrategy (Builder.untrimmedStrategy 128 128)
