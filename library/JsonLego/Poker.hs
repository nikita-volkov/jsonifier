module JsonLego.Poker
where

import JsonLego.Prelude
import PtrPoker
import qualified Acc
import qualified CharQq as Q


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
string :: ByteString -> Poker
string body =
  doubleQuote <> byteString body <> doubleQuote

{-|
> "key":value
-}
{-# INLINE objectRow #-}
objectRow :: ByteString -> Poker -> Poker
objectRow keyBody valuePoker =
  string keyBody <> colon <> valuePoker

{-# INLINE array #-}
array :: Acc Poker -> Poker
array acc =
  case Acc.uncons acc of
    Just (h, t) ->
      openingSquareBracket <> h <> foldMap (comma <>) t <> closingSquareBracket
    Nothing ->
      emptyArray

{-# INLINE object #-}
object :: Acc Poker -> Poker
object acc =
  case Acc.uncons acc of
    Just (h, t) ->
      openingCurlyBracket <> h <> foldMap (comma <>) t <> closingCurlyBracket
    Nothing ->
      emptyObject

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
