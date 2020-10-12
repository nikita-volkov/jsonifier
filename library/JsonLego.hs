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
  -- * Array
  Array,
  element,
  elements,
  -- * Object
  Object,
  row,
  rows,
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
doubleNumber a =
  let
    byteString =
      ByteString.double a
    in Value (ByteString.length byteString) (Poker.byteString byteString)

{-# INLINE scientificNumber #-}
scientificNumber :: Scientific -> Value
scientificNumber a =
  let
    byteString =
      ByteString.scientific a
    in Value (ByteString.length byteString) (Poker.byteString byteString)

{-# INLINE string #-}
string :: Text -> Value
string text =
  let
    allocation =
      2 + Allocation.stringBody text
    poker =
      Poker.string text
    in Value allocation poker

array :: Array -> Value
array (Array {..}) =
  Value allocation poker
  where
    allocation =
      Allocation.array arrayElements arrayAllocation
    poker =
      Poker.openingSquareBracket <> arrayPoker <> Poker.closingSquareBracket

object :: Object -> Value
object (Object {..}) =
  Value allocation poker
  where
    allocation =
      Allocation.object objectRows objectAllocation
    poker =
      Poker.openingCurlyBracket <> objectPoker <> Poker.closingCurlyBracket


-- * Array
-------------------------

data Array =
  Array {
    {-|
    Amount of elements.
    -}
    arrayElements :: Int,
    arrayAllocation :: Int,
    arrayPoker :: Poker
    }

instance Semigroup Array where
  {-# INLINE (<>) #-}
  Array lElements lAllocation lPoker <> Array rElements rAllocation rPoker =
    Array (lElements + rElements) (lAllocation + rAllocation) poker
    where
      poker =
        if lElements > 0
          then if rElements > 0
            then
              lPoker <> Poker.comma <> rPoker
            else lPoker
          else rPoker
  sconcat list =
    Array
      (getSum (foldMap (Sum . arrayElements) list))
      (getSum (foldMap (Sum . arrayAllocation) list))
      (foldMap arrayPoker list)

instance Monoid Array where
  mempty =
    Array 0 0 mempty
  mconcat list =
    Array
      (getSum (foldMap (Sum . arrayElements) list))
      (getSum (foldMap (Sum . arrayAllocation) list))
      (foldMap arrayPoker list)

{-# INLINE element #-}
element :: Value -> Array
element (Value {..}) =
  Array 1 valueAllocation valuePoker

{-# INLINE elements #-}
elements :: Foldable f => f Value -> Array
elements list =
  foldr step finalize list True 0 0 mempty
  where
    step (Value{..}) next first !size !allocation !poker =
      if first
        then
          next False (succ size) valueAllocation valuePoker
        else
          next False (succ size) (allocation + valueAllocation)
            (poker <> Poker.comma <> valuePoker)
    finalize _ size allocation poker =
      Array size allocation poker


-- * Object
-------------------------

data Object =
  Object {
    {-|
    Amount of rows.
    -}
    objectRows :: Int,
    {-|
    Allocation size for all keys and values.
    Does not account for commas, colons and quotes.
    -}
    objectAllocation :: Int,
    objectPoker :: Poker
    }

instance Semigroup Object where
  {-# INLINE (<>) #-}
  Object lRows lAlloc lPoker <> Object rRows rAlloc rPoker =
    Object (lRows + rRows) (lAlloc + rAlloc) poker
    where
      poker =
        if lRows > 0
          then if rRows > 0
            then
              lPoker <> Poker.comma <> rPoker
            else lPoker
          else rPoker

instance Monoid Object where
  mempty =
    Object 0 0 mempty

{-# INLINE row #-}
row :: Text -> Value -> Object
row keyText (Value {..}) =
  Object amount allocation poker
  where
    amount = 
      1
    allocation =
      Allocation.stringBody keyText +
      valueAllocation
    poker =
      Poker.objectRow keyText valuePoker

{-# INLINE rows #-}
rows :: [(Text, Value)] -> Object
rows list =
  Object amount allocation poker
  where
    amount = 
      length list
    allocation =
      list
        & fmap (\ (key, Value {..}) -> Allocation.stringBody key + valueAllocation)
        & sum
    poker =
      list
        & fmap (\ (key, Value {..}) -> Poker.objectRow key valuePoker)
        & Poker.objectBody
