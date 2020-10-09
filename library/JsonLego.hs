module JsonLego
(
  -- * Value
  Value,
  object,
  -- * Object
  Object,
  row,
)
where

import JsonLego.Prelude
import JsonLego.Poker (Poker)
import qualified JsonLego.Allocation as Allocation
import qualified JsonLego.Poker as Poker


-- * Value
-------------------------

data Value =
  Value {
    valueAllocation :: Int,
    valuePoker :: Poker
    }

object :: Object -> Value
object (Object {..}) =
  Value allocation poker
  where
    allocation =
      Allocation.object objectRows objectAllocation
    poker =
      Poker.object objectRowPokers

array :: Array -> Value
array (Array {..}) =
  Value allocation poker
  where
    allocation =
      Allocation.array arrayElements arrayAllocation
    poker =
      Poker.array arrayElementPokers


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

row :: Text -> Value -> Object
row keyText (Value {..}) =
  Object amount allocation rowPokers
  where
    amount = 
      1
    allocation =
      Allocation.stringBody keyText +
      valueAllocation
    rowPokers =
      pure (Poker.objectRow keyText valuePoker)
