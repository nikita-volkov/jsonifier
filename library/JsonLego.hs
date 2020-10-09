module JsonLego
(
  -- * ByteString
  value,
  -- * Value
  Value,
  null,
  bool,
  intNumber,
  doubleNumber,
  scientificNumber,
  string,
  array,
  object,
  -- * Object
  Object,
  row,
  rows,
  -- * Array
  Array,
  element,
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

{-# INLINE doubleNumber #-}
doubleNumber :: Double -> Value
doubleNumber =
  scientificNumber . realToFrac

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
    bodyByteString =
      ByteString.jsonStringBody text
    allocation =
      2 + ByteString.length bodyByteString
    poker =
      Poker.string bodyByteString
    in Value allocation poker

{-# INLINE array #-}
array :: Array -> Value
array (Array {..}) =
  Value allocation poker
  where
    allocation =
      Allocation.array arrayElements arrayAllocation
    poker =
      Poker.array arrayElementPokers

{-# INLINE object #-}
object :: Object -> Value
object (Object {..}) =
  Value allocation poker
  where
    allocation =
      Allocation.object objectRows objectAllocation
    poker =
      Poker.object objectRowPokers


-- * Array
-------------------------

data Array =
  Array {
    {-|
    Amount of elements.
    -}
    arrayElements :: Int,
    arrayAllocation :: Int,
    arrayElementPokers :: Acc Poker
    }

instance Semigroup Array where
  Array lElements lAllocation lPokers <> Array rElements rAllocation rPokers =
    Array (lElements + rElements) (lAllocation + rAllocation) (lPokers <> rPokers)

instance Monoid Array where
  mempty =
    Array 0 0 mempty

{-# INLINE element #-}
element :: Value -> Array
element (Value {..}) =
  Array 1 valueAllocation (pure valuePoker)


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
    objectRowPokers :: Acc Poker
    }

instance Semigroup Object where
  Object lRows lAlloc lPokers <> Object rRows rAlloc rPokers =
    Object (lRows + rRows) (lAlloc + rAlloc) (lPokers <> rPokers)

instance Monoid Object where
  mempty =
    Object 0 0 mempty

{-# INLINE row #-}
row :: Text -> Value -> Object
row keyText (Value {..}) =
  Object amount allocation rowPokers
  where
    amount = 
      1
    keyByteString =
      ByteString.jsonStringBody keyText
    allocation =
      ByteString.length keyByteString +
      valueAllocation
    rowPokers =
      pure (Poker.objectRow keyByteString valuePoker)

{-# INLINE rows #-}
rows :: [(Text, Value)] -> Object
rows list =
  Object amount allocation rowPokers
  where
    amount = 
      length list
    listWithPreparedKeys =
      list &
        fmap (first ByteString.jsonStringBody)
    allocation =
      foldl' (\ a (key, Value {..}) -> a + ByteString.length key + valueAllocation)
        0 listWithPreparedKeys
    rowPokers =
      listWithPreparedKeys
        & fmap (\ (key, Value {..}) -> Poker.objectRow key valuePoker)
        & fromList
