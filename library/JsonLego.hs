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
import PtrPoker (Poker)
import qualified JsonLego.Allocation as Allocation
import qualified JsonLego.Poker as Poker
import qualified PtrPoker as Poker
import qualified Data.NumberLength as NumberLength
import qualified JsonLego.ByteString as ByteString
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString


{-|
Render a JSON builder into strict bytestring.
-}
{-# INLINE json #-}
json :: Json -> ByteString
json (Json {..}) =
  ByteString.unsafeCreate valueAllocation (void . Poker.run valuePoker)


-- * Json
-------------------------

data Json =
  Json {
    valueAllocation :: Int,
    valuePoker :: Poker
    }

{-# INLINE null #-}
null :: Json
null =
  Json 4 Poker.null

{-# INLINE bool #-}
bool :: Bool -> Json
bool =
  \ case
    True ->
      Json 4 Poker.true
    False ->
      Json 5 Poker.false

{-# INLINE intNumber #-}
intNumber :: Int -> Json
intNumber a =
  Json
    (NumberLength.signedNumberLength a)
    (Poker.asciiDecInt a)

{-# INLINE int64Number #-}
int64Number :: Int64 -> Json
int64Number a =
  byteString (ByteString.unsafeCreateDownToN 24 populate)
  where
    populate p =
      if a < 0
        then
          error "TODO"
        else
          let
            loop a !length p =
              case divMod a 10 of
                (next, digit) ->
                  let
                    byte = 
                      fromIntegral digit + 48
                    newLength =
                      succ length
                    in if next == 0
                      then
                        poke p byte $> newLength
                      else
                        poke p byte >> loop next newLength (plusPtr p (-1))
            in
              loop a 0 p

{-# INLINE doubleNumber #-}
doubleNumber :: Double -> Json
doubleNumber =
  byteString . ByteString.double

{-# INLINE scientificNumber #-}
scientificNumber :: Scientific -> Json
scientificNumber =
  byteString . ByteString.scientific

{-# INLINE string #-}
string :: Text -> Json
string text =
  let
    allocation =
      2 + Allocation.stringBody text
    poker =
      Poker.string text
    in Json allocation poker

{-# INLINE array #-}
array :: Foldable f => f Json -> Json
array list =
  foldr step finalize list True 0 0 mempty
  where
    step (Json{..}) next first !size !allocation !poker =
      if first
        then
          next False 1 valueAllocation valuePoker
        else
          next False (succ size) (allocation + valueAllocation)
            (poker <> Poker.comma <> valuePoker)
    finalize _ size contentsAllocation bodyPoker =
      Json allocation poker
      where
        allocation =
          Allocation.array size contentsAllocation
        poker =
          Poker.openingSquareBracket <> bodyPoker <> Poker.closingSquareBracket

{-# INLINE object #-}
object :: Foldable f => f (Text, Json) -> Json
object f =
  foldr step finalize f True 0 0 mempty
  where
    step (key, Json{..}) next first !size !allocation !poker =
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
      Json allocation poker
      where
        allocation =
          Allocation.object size contentsAllocation
        poker =
          Poker.openingCurlyBracket <> bodyPoker <> Poker.closingCurlyBracket

{-# INLINE byteString #-}
byteString :: ByteString -> Json
byteString a =
  Json (ByteString.length a) (Poker.byteString a)
