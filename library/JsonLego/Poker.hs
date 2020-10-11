module JsonLego.Poker
where

import JsonLego.Prelude
import PtrPoker
import qualified Foreign.Marshal.Utils as Foreign
import qualified CharQq as Q
import qualified Data.Text.Internal as Text
import qualified Data.Text.Array as TextArray
import qualified JsonLego.Poker.Ffi.JsonEncoding as JsonEncodingFfi


{-# NOINLINE null #-}
null :: Poker
null =
  byteString "null"

{-# INLINE boolean #-}
boolean :: Bool -> Poker
boolean =
  bool false true

{-# NOINLINE true #-}
true :: Poker
true =
  byteString "true"

{-# NOINLINE false #-}
false :: Poker
false =
  byteString "false"

{-# INLINE intNumber #-}
intNumber :: Int -> Poker
intNumber =
  asciiDecInt

{-# INLINE string #-}
string :: Text -> Poker
string (Text.Text arr off len) =
  Poker $ \ ptr ->
    JsonEncodingFfi.string ptr (TextArray.aBA arr) (fromIntegral off) (fromIntegral len)

{-|
> "key":value
-}
{-# INLINE objectRow #-}
objectRow :: Text -> Poker -> Poker
objectRow keyBody valuePoker =
  string keyBody <> colon <> valuePoker

{-# INLINE array #-}
array :: Foldable f => f Poker -> Poker
array f =
  snd (foldl' (\ (first, acc) p -> (False, acc <> if first then p else comma <> p))
      (True, openingSquareBracket) f) <>
  closingSquareBracket

{-# INLINE object #-}
object :: Poker -> Poker
object body =
  openingCurlyBracket <> body <> closingCurlyBracket

{-# INLINE objectBody #-}
objectBody :: Foldable f => f Poker -> Poker
objectBody =
  foldl'
    (\ (first, acc) p -> (False, acc <> if first then p else comma <> p))
    (True, mempty)
    >>> snd

{-# NOINLINE emptyArray #-}
emptyArray :: Poker
emptyArray =
  byteString "[]"

{-# NOINLINE emptyObject #-}
emptyObject :: Poker
emptyObject =
  byteString "{}"

openingSquareBracket :: Poker
openingSquareBracket =
  word8 [Q.ord|[|]

closingSquareBracket :: Poker
closingSquareBracket =
  word8 [Q.ord|]|]

openingCurlyBracket :: Poker
openingCurlyBracket =
  word8 [Q.ord|{|]

closingCurlyBracket :: Poker
closingCurlyBracket =
  word8 [Q.ord|}|]

colon :: Poker
colon =
  word8 [Q.ord|:|]

comma :: Poker
comma =
  word8 [Q.ord|,|]

doubleQuote :: Poker
doubleQuote =
  word8 [Q.ord|"|]
