module JsonLego.Poker
where

import JsonLego.Prelude
import PtrPoker
import qualified Acc


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

{-|
> "key":value
-}
objectRow :: Text -> Poker -> Poker
objectRow keyText valuePoker =
  stringLiteral keyText <> colon <> valuePoker

stringLiteral :: Text -> Poker
stringLiteral =
  error "TODO"

array :: Acc Poker -> Poker
array =
  error "TODO"

{-# NOINLINE emptyObject #-}
emptyObject :: Poker
emptyObject =
  byteString "{}"

object :: Acc Poker -> Poker
object acc =
  case Acc.uncons acc of
    Just (h, t) ->
      openingCurlyBracket <> h <> foldMap (colon <>) t <> closingCurlyBracket
    Nothing ->
      emptyObject

openingCurlyBracket :: Poker
openingCurlyBracket =
  error "TODO"

closingCurlyBracket :: Poker
closingCurlyBracket =
  error "TODO"

colon :: Poker
colon =
  error "TODO"

comma :: Poker
comma =
  error "TODO"
