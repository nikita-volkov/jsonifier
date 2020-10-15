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
)
where

import Jsonifier.Prelude hiding (null, bool)
import PtrPoker.Poke (Poke)
import PtrPoker.Write (Write)
import qualified Jsonifier.Allocation as Allocation
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
A sort of a JSON-specialized string 'ByteString' builder.

You can construct it by using the specialized
conversion functions from Haskell types.
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
    allocation =
      2 + Allocation.stringBody text
    poke =
      Poke.string text
    in write allocation poke

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
array list =
  foldr step finalize list True 0 0 mempty
  where
    step (Json (Write.Write{..})) next first !size !allocation !poke =
      if first
        then
          next False 1 writeSize writePoke
        else
          next False (succ size) (allocation + writeSize)
            (poke <> Poke.comma <> writePoke)
    finalize _ size contentsAllocation bodyPoke =
      write allocation poke
      where
        allocation =
          Allocation.array size contentsAllocation
        poke =
          Poke.openingSquareBracket <> bodyPoke <> Poke.closingSquareBracket

{-|
JSON Array literal from a foldable over pairs of key to value literal.

Don\'t be afraid to use 'fmap' to map the elements of the input datastructure,
it will all be optimized away.
-}
{-# INLINE object #-}
object :: Foldable f => f (Text, Json) -> Json
object f =
  foldr step finalize f True 0 0 mempty
  where
    step (key, Json (Write.Write{..})) next first !size !allocation !poke =
      if first
        then
          next False 1 rowAllocation rowPoke
        else
          next False (succ size) (allocation + rowAllocation)
            (poke <> Poke.comma <> rowPoke)
      where
        rowAllocation =
          Allocation.stringBody key +
          writeSize
        rowPoke =
          Poke.objectRow key writePoke
    finalize _ size contentsAllocation bodyPoke =
      write allocation poke
      where
        allocation =
          Allocation.object size contentsAllocation
        poke =
          Poke.openingCurlyBracket <> bodyPoke <> Poke.closingCurlyBracket
