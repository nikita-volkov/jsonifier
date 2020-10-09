module JsonLego.Poker
where

import JsonLego.Prelude
import qualified Acc


type Poker =
  Ptr Word8 -> IO (Ptr Word8)

{-|
> "key":value
-}
objectRow :: Text -> Poker -> Poker
objectRow keyText valuePoker =
  stringLiteral keyText >=> colon >=> valuePoker

stringLiteral :: Text -> Poker
stringLiteral =
  error "TODO"

array :: Acc Poker -> Poker
array =
  error "TODO"

emptyObject :: Poker
emptyObject p =
  error "TODO"

object :: Acc Poker -> Poker
object acc =
  case Acc.uncons acc of
    Just (h, t) ->
      openingCurlyBracket >=> h >=> concatPrependingColon t >=> closingCurlyBracket
    Nothing ->
      emptyObject

concatPrependingColon :: Acc Poker -> Poker
concatPrependingColon pokers p =
  foldM (\ p poker -> colon p >>= poker) p pokers

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
