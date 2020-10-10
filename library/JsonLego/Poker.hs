module JsonLego.Poker
where

import JsonLego.Prelude
import PtrPoker
import qualified CharQq as Q
import qualified Data.Text.Internal as Text
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Internal.Encoding.Utf16 as TextUtf16
import qualified JsonLego.Base16EncodingTable as Base16EncodingTable
import qualified JsonLego.UncheckedShifting as UncheckedShifting


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
string body =
  doubleQuote <> stringBody body <> doubleQuote

stringBody :: Text -> Poker
stringBody (Text.Text arr off len) =
  Poker $ go off
  where
    maxOffset =
      off + len
    go !offset !p =
      if offset >= maxOffset
        then
          return p
        else
          if w <= 0x7F
            then
              case w of
                92 {- \\ -} ->
                  poke2 1 92 92
                34 {- \" -} ->
                  poke2 1 92 34
                _ -> 
                  if w < 32
                    then case w of
                      10 {- \n -} ->
                        poke2 1 92 110
                      13 {- \r -} ->
                        poke2 1 92 114
                      9 {- \t -} ->
                        poke2 1 92 116
                      _ {- control -} ->
                        poke p (fromIntegral 92 :: Word8) >>
                        pokeByteOff p 1 (fromIntegral 117 :: Word8) >>
                        Base16EncodingTable.encode8As16h Base16EncodingTable.lowerTable
                          (fromIntegral (UncheckedShifting.shiftr_w16 w 8)) >>=
                        pokeByteOff (castPtr p) 2 >>
                        Base16EncodingTable.encode8As16h Base16EncodingTable.lowerTable
                          (fromIntegral w) >>=
                        pokeByteOff (castPtr p) 4 >>
                        step 1 6                        
                    else
                      poke1 1 w
            else if w <= 0x7FF
              then
                poke2 1 (shiftR w 6 + 0xC0) ((w .&. 0x3f) + 0x80)
              else if w <= 0xDBFF && w >= 0xD800
                then
                  let
                    codepoint =
                      ord (TextUtf16.chr2 w (TextArray.unsafeIndex arr (succ offset)))
                    in
                      poke4 2
                        (shiftR codepoint 18 + 0xF0)
                        (((shiftR codepoint 12) .&. 0x3F) + 0x80)
                        (((shiftR codepoint 6) .&. 0x3F) + 0x80)
                        ((codepoint .&. 0x3F) + 0x80)
                else
                  poke3 1
                    ((shiftR w 12) + 0xE0)
                    (((shiftR w 6) .&. 0x3F) + 0x80)
                    ((w .&. 0x3F) + 0x80)
          where
        w =
          TextArray.unsafeIndex arr offset
        poke1 bytesConsumed a =
          poke p (fromIntegral a :: Word8) >>
          step bytesConsumed 1
        poke2 bytesConsumed a b =
          poke p (fromIntegral a :: Word8) >>
          pokeByteOff p 1 (fromIntegral b :: Word8) >>
          step bytesConsumed 2
        poke3 bytesConsumed a b c =
          poke p (fromIntegral a :: Word8) >>
          pokeByteOff p 1 (fromIntegral b :: Word8) >>
          pokeByteOff p 2 (fromIntegral c :: Word8) >>
          step bytesConsumed 3
        poke4 bytesConsumed a b c d =
          poke p (fromIntegral a :: Word8) >>
          pokeByteOff p 1 (fromIntegral b :: Word8) >>
          pokeByteOff p 2 (fromIntegral c :: Word8) >>
          pokeByteOff p 3 (fromIntegral d :: Word8) >>
          step bytesConsumed 4
        poke6 bytesConsumed a b c d e f =
          poke p (fromIntegral a :: Word8) >>
          pokeByteOff p 1 (fromIntegral b :: Word8) >>
          pokeByteOff p 2 (fromIntegral c :: Word8) >>
          pokeByteOff p 3 (fromIntegral d :: Word8) >>
          pokeByteOff p 4 (fromIntegral e :: Word8) >>
          pokeByteOff p 5 (fromIntegral f :: Word8) >>
          step bytesConsumed 6
        step bytesConsumed bytesEmitted =
          go (offset + bytesConsumed) (plusPtr p bytesEmitted)

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
