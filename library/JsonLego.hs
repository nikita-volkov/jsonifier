module JsonLego
(
  -- * ByteString
  value,
  -- * Value
  Value,
  null,
  bool,
  intNumber,
  int64Number,
  doubleNumber,
  scientificNumber,
  string,
  array,
  object,
)
where

import JsonLego.Prelude hiding (null, bool)
import PtrPoker (Poker)
import qualified JsonLego.Allocation as Allocation
import qualified JsonLego.Poker as Poker
import qualified PtrPoker as Poker
import qualified Data.NumberLength as NumberLength
import qualified JsonLego.ByteString as ByteString
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString


{-|
Render a value builder into strict bytestring.
-}
{-# INLINE value #-}
value :: Value -> ByteString
value (Value {..}) =
  ByteString.unsafeCreate valueAllocation (void . Poker.run valuePoker)


-- * Value
-------------------------

data Value =
  Value {
    valueAllocation :: Int,
    valuePoker :: Poker
    }

{-# INLINE null #-}
null :: Value
null =
  Value 4 Poker.null

{-# INLINE bool #-}
bool :: Bool -> Value
bool =
  \ case
    True ->
      Value 4 Poker.true
    False ->
      Value 5 Poker.false

{-# INLINE intNumber #-}
intNumber :: Int -> Value
intNumber a =
  Value
    (NumberLength.signedNumberLength a)
    (Poker.asciiDecInt a)

{-# INLINE int64Number #-}
int64Number :: Int64 -> Value
int64Number a =
  Value
    (NumberLength.signedNumberLength a)
    (Poker.asciiDecInt64 a)

{-# INLINE doubleNumber #-}
doubleNumber :: Double -> Value
doubleNumber =
  byteString . ByteString.double

{-# INLINE scientificNumber #-}
scientificNumber :: Scientific -> Value
scientificNumber =
  byteString . ByteString.scientific

{-# INLINE string #-}
string :: Text -> Value
string text =
  let
    allocation =
      2 + Allocation.stringBody text
    poker =
      Poker.string text
    in Value allocation poker

{-# INLINE array #-}
array :: Foldable f => f Value -> Value
array list =
  foldr step finalize list True 0 0 mempty
  where
    step (Value{..}) next first !size !allocation !poker =
      if first
        then
          next False 1 valueAllocation valuePoker
        else
          next False (succ size) (allocation + valueAllocation)
            (poker <> Poker.comma <> valuePoker)
    finalize _ size contentsAllocation bodyPoker =
      Value allocation poker
      where
        allocation =
          Allocation.array size contentsAllocation
        poker =
          Poker.openingSquareBracket <> bodyPoker <> Poker.closingSquareBracket

{-# INLINE object #-}
object :: Foldable f => f (Text, Value) -> Value
object f =
  foldr step finalize f True 0 0 mempty
  where
    step (key, Value{..}) next first !size !allocation !poker =
      if first
        then
          next False 1 rowAllocation rowPoker
        else
          next False (succ size) (allocation + rowAllocation)
            (poker <> Poker.comma <> rowPoker)
      where
        rowAllocation =
          Allocation.stringBody key +
          valueAllocation
        rowPoker =
          Poker.objectRow key valuePoker
    finalize _ size contentsAllocation bodyPoker =
      Value allocation poker
      where
        allocation =
          Allocation.object size contentsAllocation
        poker =
          Poker.openingCurlyBracket <> bodyPoker <> Poker.closingCurlyBracket

{-# INLINE byteString #-}
byteString :: ByteString -> Value
byteString a =
  Value (ByteString.length a) (Poker.byteString a)
