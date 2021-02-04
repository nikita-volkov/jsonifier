{-|
Simple DSL for mapping Haskell values into JSON representation and
rendering it into 'ByteString'.
-}
module Jsonifier
(
  -- * ByteString
  toByteString,
  -- * Json
  Json,
  -- ** Primitives
  null,
  bool,
  -- ** Numbers
  intNumber,
  wordNumber,
  doubleNumber,
  scientificNumber,
  -- ** Strings
  textString,
  scientificString,
  -- ** Composites
  array,
  object,
  -- ** Low-level
  writeJson,
)
where

import Jsonifier.Prelude hiding (null, bool)
import PtrPoker.Poke (Poke)
import PtrPoker.Write (Write)
import qualified Jsonifier.Size as Size
import qualified Jsonifier.Poke as Poke
import qualified Jsonifier.Write as Write
import qualified PtrPoker.Poke as Poke
import qualified PtrPoker.Write as Write
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString


{-|
Render a JSON value into strict bytestring.
-}
{-# INLINE toByteString #-}
toByteString :: Json -> ByteString
toByteString =
  Write.writeToByteString . coerce


-- * Json
-------------------------

{-|
Specification of how to render a JSON value to 'ByteString'.
Sort of a JSON-specialized 'ByteString' builder.

You can construct it from Haskell types
using the specialized conversion functions
like 'intNumber', 'textString' or 'object'.
After constructing, you can convert to strict 'ByteString'
using the 'toByteString' function.
-}
newtype Json =
  Json Write.Write

{-# INLINE write #-}
write :: Int -> Poke.Poke -> Json
write size poke =
  Json (Write.Write size poke)

{-|
JSON Null literal.
-}
{-# INLINE null #-}
null :: Json
null =
  write 4 Poke.null

{-|
JSON Boolean literal.
-}
{-# INLINE bool #-}
bool :: Bool -> Json
bool =
  \ case
    True ->
      write 4 Poke.true
    False ->
      write 5 Poke.false

{-|
JSON Number literal from @Int@.
-}
{-# INLINE intNumber #-}
intNumber :: Int -> Json
intNumber =
  Json . Write.intAsciiDec

{-|
JSON Number literal from @Word@.
-}
{-# INLINE wordNumber #-}
wordNumber :: Word -> Json
wordNumber =
  Json . Write.wordAsciiDec

{-|
JSON Number literal from @Double@.

Since JSON doesn\'t have support for them,
non-real values like @NaN@, @Infinity@, @-Infinity@ get rendered as @0@.
-}
{-# INLINE doubleNumber #-}
doubleNumber :: Double -> Json
doubleNumber =
  Json . Write.zeroNonRealDoubleAsciiDec

{-|
JSON Number literal from @Scientific@.
-}
{-# INLINE scientificNumber #-}
scientificNumber :: Scientific -> Json
scientificNumber =
  Json . Write.scientificAsciiDec

{-|
JSON String literal from @Text@.
-}
{-# INLINE textString #-}
textString :: Text -> Json
textString text =
  let
    size =
      2 + Size.stringBody text
    poke =
      Poke.string text
    in write size poke

{-|
JSON String literal from @Scientific@.

You may need this when the reader of your JSON
cannot handle large number literals.
-}
{-# INLINE scientificString #-}
scientificString :: Scientific -> Json
scientificString =
  Json . Write.scientificString

{-|
JSON Array literal from a foldable over element literals.

Don\'t be afraid to use 'fmap' to map the elements of the input datastructure,
it will all be optimized away.
-}
{-# INLINE array #-}
array :: Foldable f => f Json -> Json
array foldable =
  write size poke
  where
    size =
      foldr step finalize foldable 0 0
      where
        step (Json (Write.Write writeSize _)) next !count !size =
          next (succ count) (writeSize + size)
        finalize count size =
          Size.array count size
    poke =
      Poke.Poke $
        Poke.pokePtr Poke.openingSquareBracket >=>
        foldr step finalize foldable True
      where
        step (Json (Write.Write _ poke)) next first =
          if first
            then
              Poke.pokePtr poke >=>
              next False
            else
              Poke.pokePtr Poke.comma >=>
              Poke.pokePtr poke >=>
              next False
        finalize _ =
          Poke.pokePtr Poke.closingSquareBracket

{-|
JSON Object literal from a foldable over pairs of key to value literal.

Don\'t be afraid to use 'fmap' to map the elements of the input datastructure,
it will all be optimized away.
-}
{-# INLINE object #-}
object :: Foldable f => f (Text, Json) -> Json
object f =
  foldr step finalize f True 0 0 mempty
  where
    step (key, Json (Write.Write{..})) next first !count !size !poke =
      if first
        then
          next False 1 rowSize rowPoke
        else
          next False (succ count) (size + rowSize)
            (poke <> Poke.comma <> rowPoke)
      where
        rowSize =
          Size.stringBody key +
          writeSize
        rowPoke =
          Poke.objectRow key writePoke
    finalize _ count contentsSize bodyPoke =
      write size poke
      where
        size =
          Size.object count contentsSize
        poke =
          Poke.openingCurlyBracket <> bodyPoke <> Poke.closingCurlyBracket

{-|
Any JSON literal manually rendered as Write.

This is a low-level function allowing to avoid unnecessary processing
in cases where you already have a rendered JSON at hand.

You can think of Write as a specialized version of ByteString builder.
You can efficiently convert a ByteString to Write using 'PtrPoker.Write.byteString',
making it possible to have parts of the JSON value tree rendered using other libraries.
You can as well manually implement encoders for your custom types.

__Warning:__

It is your responsibility to ensure that the content is correct,
otherwise you may produce invalid JSON.
-}
writeJson :: Write.Write -> Json
writeJson = coerce
