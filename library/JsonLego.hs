module JsonLego
(
  -- * ByteString
  json,
  -- * Json
  Json,
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
import PtrPoker.Poke (Poke)
import PtrPoker.Write (Write)
import qualified JsonLego.Allocation as Allocation
import qualified JsonLego.Poke as Poke
import qualified PtrPoker.Poke as Poke
import qualified PtrPoker.Write as Write
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString


{-|
Render a JSON builder into strict bytestring.
-}
{-# INLINE json #-}
json :: Json -> ByteString
json =
  Write.writeByteString . coerce


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

{-# INLINE int64Number #-}
int64Number :: Int64 -> Json
int64Number =
  Json . Write.int64AsciiDec

{-# INLINE doubleNumber #-}
doubleNumber :: Double -> Json
doubleNumber =
  Json . Write.doubleAsciiDec

{-# INLINE scientificNumber #-}
scientificNumber :: Scientific -> Json
scientificNumber =
  Json . Write.scientificAsciiDec

{-# INLINE string #-}
string :: Text -> Json
string text =
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
    finalize _ size contentsAllocation bodyPoker =
      write allocation poke
      where
        allocation =
          Allocation.array size contentsAllocation
        poke =
          Poke.openingSquareBracket <> bodyPoker <> Poke.closingSquareBracket

{-# INLINE object #-}
object :: Foldable f => f (Text, Json) -> Json
object f =
  foldr step finalize f True 0 0 mempty
  where
    step (key, Json (Write.Write{..})) next first !size !allocation !poke =
      if first
        then
          next False 1 rowAllocation rowPoker
        else
          next False (succ size) (allocation + rowAllocation)
            (poke <> Poke.comma <> rowPoker)
      where
        rowAllocation =
          Allocation.stringBody key +
          writeSize
        rowPoker =
          Poke.objectRow key writePoke
    finalize _ size contentsAllocation bodyPoker =
      write allocation poke
      where
        allocation =
          Allocation.object size contentsAllocation
        poke =
          Poke.openingCurlyBracket <> bodyPoker <> Poke.closingCurlyBracket
