module JsonLego
(
  run,
  -- * Value
  Value,
  object,
  -- * Object
  Object,
  row,
)
where

import JsonLego.Prelude
import PtrPoker (Poker)
import qualified JsonLego.Allocation as Allocation
import qualified JsonLego.Poker as Poker
import qualified PtrPoker as Poker
import qualified Data.NumberLength as NumberLength


run :: Value -> ByteString
run =
  error "TODO"


-- * Value
-------------------------

data Value =
  Value {
    valueAllocation :: Int,
    valuePoker :: Poker
    }

boolean :: Bool -> Value
boolean =
  \ case
    True ->
      Value 4 Poker.true
    False ->
      Value 5 Poker.false

intNumber :: Int -> Value
intNumber a =
  Value
    (NumberLength.signedNumberLength a)
    (Poker.asciiDecInt a)

array :: Array -> Value
array (Array {..}) =
  Value allocation poker
  where
    allocation =
      Allocation.array arrayElements arrayAllocation
    poker =
      Poker.array arrayElementPokers

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

rows :: [(Text, Value)] -> Object
rows list =
  Object amount allocation rowPokers
  where
    amount = 
      length list
    allocation =
      foldl' (\ a (keyText, Value {..}) -> a + Allocation.stringBody keyText + valueAllocation)
        0 list
    rowPokers =
      list
        & fmap (\ (keyText, Value {..}) -> Poker.objectRow keyText valuePoker)
        & fromList
