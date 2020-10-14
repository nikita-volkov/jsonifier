module Jsonifier
(
  -- * ByteString
  toByteString,
  -- * Json
  Json,
  null,
  bool,
  intNumber,
  wordNumber,
  doubleNumber,
  scientificNumber,
  textString,
  array,
  object,
)
where

import Jsonifier.Prelude hiding (null, bool)
import PtrPoker.Poke (Poke)
import PtrPoker.Write (Write)
import qualified Jsonifier.Allocation as Allocation
import qualified Jsonifier.Poke as Poke
import qualified PtrPoker.Poke as Poke
import qualified PtrPoker.Write as Write
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString


{-|
Render a JSON builder into strict bytestring.
-}
{-# INLINE toByteString #-}
toByteString :: Json -> ByteString
toByteString =
  Write.writeToByteString . coerce


-- * Json
-------------------------

newtype Json =
  Json Write.Write

{-# INLINE write #-}
write :: Int -> Poke.Poke -> Json
write size poke =
  Json (Write.Write size poke)

{-# INLINE null #-}
null :: Json
null =
  write 4 Poke.null

{-# INLINE bool #-}
bool :: Bool -> Json
bool =
  \ case
    True ->
      write 4 Poke.true
    False ->
      write 5 Poke.false

{-# INLINE intNumber #-}
intNumber :: Int -> Json
intNumber =
  Json . Write.intAsciiDec

{-# INLINE wordNumber #-}
wordNumber :: Word -> Json
wordNumber =
  Json . Write.wordAsciiDec

{-# INLINE doubleNumber #-}
doubleNumber :: Double -> Json
doubleNumber =
  Json . Write.zeroNonRealDoubleAsciiDec

{-# INLINE scientificNumber #-}
scientificNumber :: Scientific -> Json
scientificNumber =
  Json . Write.scientificAsciiDec

{-# INLINE textString #-}
textString :: Text -> Json
textString text =
  let
    allocation =
      2 + Allocation.stringBody text
    poke =
      Poke.string text
    in write allocation poke

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
