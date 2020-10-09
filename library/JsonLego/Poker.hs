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

intNumber :: Int -> Poker
intNumber =
  asciiDecInt

string :: ByteString -> Poker
string body =
  doubleQuote <> byteString body <> doubleQuote

{-|
> "key":value
-}
objectRow :: ByteString -> Poker -> Poker
objectRow keyBody valuePoker =
  string keyBody <> colon <> valuePoker

array :: Acc Poker -> Poker
array acc =
  case Acc.uncons acc of
    Just (h, t) ->
      openingSquareBracket <> h <> foldMap (comma <>) t <> closingSquareBracket
    Nothing ->
      emptyArray

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

{-# NOINLINE openingSquareBracket #-}
openingSquareBracket :: Poker
openingSquareBracket =
  word8 [Q.ord|[|]

{-# NOINLINE closingSquareBracket #-}
closingSquareBracket :: Poker
closingSquareBracket =
  word8 [Q.ord|]|]

{-# NOINLINE openingCurlyBracket #-}
openingCurlyBracket :: Poker
openingCurlyBracket =
  word8 [Q.ord|{|]

{-# NOINLINE closingCurlyBracket #-}
closingCurlyBracket :: Poker
closingCurlyBracket =
  word8 [Q.ord|}|]

{-# NOINLINE colon #-}
colon :: Poker
colon =
  word8 [Q.ord|:|]

{-# NOINLINE comma #-}
comma :: Poker
comma =
  word8 [Q.ord|,|]

{-# NOINLINE doubleQuote #-}
doubleQuote :: Poker
doubleQuote =
  word8 [Q.ord|"|]
