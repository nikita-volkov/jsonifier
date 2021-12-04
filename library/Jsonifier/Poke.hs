{-# LANGUAGE CPP #-}
module Jsonifier.Poke
where

import Jsonifier.Prelude
import PtrPoker.Poke
import qualified Data.Text.Internal as Text
import qualified Data.Text.Array as TextArray
import qualified Jsonifier.Ffi as Ffi


null :: Poke
null =
  byteString "null"

{-# INLINE boolean #-}
boolean :: Bool -> Poke
boolean =
  bool false true

true :: Poke
true =
  byteString "true"

false :: Poke
false =
  byteString "false"

{-# INLINE string #-}
string :: Text -> Poke
#if MIN_VERSION_text(2,0,0)
string (Text.Text (TextArray.ByteArray arr) off len) =
#else
string (Text.Text (TextArray.aBA -> arr) off len) =
#endif
  Poke $ \ ptr ->
    Ffi.encodeString ptr arr (fromIntegral off) (fromIntegral len)

{-|
> "key":value
-}
{-# INLINE objectRow #-}
objectRow :: Text -> Poke -> Poke
objectRow keyBody valuePoke =
  string keyBody <> colon <> valuePoke

{-# INLINE array #-}
array :: Foldable f => f Poke -> Poke
array f =
  snd (foldl' (\ (first, acc) p -> (False, acc <> if first then p else comma <> p))
      (True, openingSquareBracket) f) <>
  closingSquareBracket

{-# INLINE object #-}
object :: Poke -> Poke
object body =
  openingCurlyBracket <> body <> closingCurlyBracket

{-# INLINE objectBody #-}
objectBody :: Foldable f => f Poke -> Poke
objectBody =
  foldl'
    (\ (first, acc) p -> (False, acc <> if first then p else comma <> p))
    (True, mempty)
    >>> snd

emptyArray :: Poke
emptyArray =
  byteString "[]"

emptyObject :: Poke
emptyObject =
  byteString "{}"

openingSquareBracket :: Poke
openingSquareBracket =
  word8 91

closingSquareBracket :: Poke
closingSquareBracket =
  word8 93

openingCurlyBracket :: Poke
openingCurlyBracket =
  word8 123

closingCurlyBracket :: Poke
closingCurlyBracket =
  word8 125

colon :: Poke
colon =
  word8 58

comma :: Poke
comma =
  word8 44

doubleQuote :: Poke
doubleQuote =
  word8 34
