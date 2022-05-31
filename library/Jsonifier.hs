-- |
-- Simple DSL for mapping Haskell values into JSON representation and
-- rendering it into 'ByteString'.
module Jsonifier
  ( -- * Execution
    toByteString,
    toWrite,

    -- * Json
    Json,

    -- ** Primitives
    null,
    true,
    false,
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
    fromByteString,
    fromWrite,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString
import qualified Jsonifier.Poke as Poke
import Jsonifier.Prelude hiding (bool, null)
import qualified Jsonifier.Size as Size
import qualified Jsonifier.Write as Write
import PtrPoker.Poke (Poke)
import qualified PtrPoker.Poke as Poke
import PtrPoker.Write (Write)
import qualified PtrPoker.Write as Write

-- |
-- Render a JSON value into strict bytestring.
{-# INLINE toByteString #-}
toByteString :: Json -> ByteString
toByteString =
  Write.writeToByteString . coerce

-- |
-- Render a JSON value into Write.
{-# INLINE toWrite #-}
toWrite :: Json -> Write
toWrite =
  coerce

-- * Json

-- |
-- Specification of how to render a JSON value to 'ByteString'.
-- Sort of a JSON-specialized 'ByteString' builder.
--
-- You can construct it from Haskell types
-- using the specialized conversion functions
-- like 'intNumber', 'textString' or 'object'.
-- After constructing, you can convert to strict 'ByteString'
-- using the 'toByteString' function.
newtype Json
  = Json Write.Write

{-# INLINE write #-}
write :: Int -> Poke.Poke -> Json
write size poke =
  Json (Write.Write size poke)

-- |
-- JSON Null literal.
{-# INLINE null #-}
null :: Json
null =
  write 4 Poke.null

-- |
-- JSON @true@ Boolean literal.
{-# INLINE true #-}
true :: Json
true =
  write 4 Poke.true

-- |
-- JSON @false@ Boolean literal.
{-# INLINE false #-}
false :: Json
false =
  write 5 Poke.false

-- |
-- JSON Boolean literal.
{-# INLINE bool #-}
bool :: Bool -> Json
bool =
  \case
    True -> true
    False -> false

-- |
-- JSON Number literal from @Int@.
{-# INLINE intNumber #-}
intNumber :: Int -> Json
intNumber =
  Json . Write.intAsciiDec

-- |
-- JSON Number literal from @Word@.
{-# INLINE wordNumber #-}
wordNumber :: Word -> Json
wordNumber =
  Json . Write.wordAsciiDec

-- |
-- JSON Number literal from @Double@.
--
-- Since JSON doesn\'t have support for them,
-- non-real values like @NaN@, @Infinity@, @-Infinity@ get rendered as @0@.
{-# INLINE doubleNumber #-}
doubleNumber :: Double -> Json
doubleNumber =
  Json . Write.zeroNonRealDoubleAsciiDec

-- |
-- JSON Number literal from @Scientific@.
{-# INLINE scientificNumber #-}
scientificNumber :: Scientific -> Json
scientificNumber =
  Json . Write.scientificAsciiDec

-- |
-- JSON String literal from @Text@.
{-# INLINE textString #-}
textString :: Text -> Json
textString text =
  let size =
        2 + Size.stringBody text
      poke =
        Poke.string text
   in write size poke

-- |
-- JSON String literal from @Scientific@.
--
-- You may need this when the reader of your JSON
-- cannot handle large number literals.
{-# INLINE scientificString #-}
scientificString :: Scientific -> Json
scientificString =
  Json . Write.scientificString

-- |
-- JSON Array literal from a foldable over element literals.
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
        Poke.pokePtr Poke.openingSquareBracket
          >=> foldr step finalize foldable True
      where
        step (Json (Write.Write _ poke)) next first =
          if first
            then
              Poke.pokePtr poke
                >=> next False
            else
              Poke.pokePtr Poke.comma
                >=> Poke.pokePtr poke
                >=> next False
        finalize _ =
          Poke.pokePtr Poke.closingSquareBracket

-- |
-- JSON Object literal from a foldable over pairs of key to value literal.
{-# INLINE object #-}
object :: Foldable f => f (Text, Json) -> Json
object f =
  foldr step finalize f True 0 0 mempty
  where
    step (key, Json (Write.Write {..})) next first !count !size !poke =
      if first
        then next False 1 rowSize rowPoke
        else
          next
            False
            (succ count)
            (size + rowSize)
            (poke <> Poke.comma <> rowPoke)
      where
        rowSize =
          Size.stringBody key
            + writeSize
        rowPoke =
          Poke.objectRow key writePoke
    finalize _ count contentsSize bodyPoke =
      write size poke
      where
        size =
          Size.object count contentsSize
        poke =
          Poke.openingCurlyBracket <> bodyPoke <> Poke.closingCurlyBracket

-- |
-- Any JSON literal manually rendered as ByteString.
--
-- This is a low-level function allowing to avoid unnecessary processing
-- in cases where you already have a rendered JSON at hand.
--
-- __Warning:__
--
-- It is your responsibility to ensure that the content is correct JSON.
fromByteString :: ByteString.ByteString -> Json
fromByteString = Json . Write.byteString

-- |
-- Any JSON literal manually rendered as Write.
--
-- This is a low-level function allowing to avoid unnecessary processing
-- in cases where you already have a rendered JSON at hand.
--
-- You can think of Write as a specialized version of ByteString builder.
-- You can efficiently convert a ByteString to Write using 'PtrPoker.Write.byteString',
-- making it possible to have parts of the JSON value tree rendered using other libraries.
-- You can as well manually implement encoders for your custom types.
--
-- __Warning:__
--
-- It is your responsibility to ensure that the content is correct,
-- otherwise you may produce invalid JSON.
fromWrite :: Write.Write -> Json
fromWrite = Json
