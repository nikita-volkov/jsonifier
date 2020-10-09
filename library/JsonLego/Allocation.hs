module JsonLego.Allocation
where

import JsonLego.Prelude
import qualified Data.Text.Internal as Text
import qualified Data.Text.Array as TextArray


object :: Int -> Int -> Int
object rowsAmount contentsAllocation =
  curlies + commas rowsAmount + colonsAndQuotes + contentsAllocation
  where
    curlies =
      2
    colonsAndQuotes =
      rowsAmount * 3

array :: Int -> Int -> Int
array elementsAmount contentsAllocation =
  brackets + commas elementsAmount + contentsAllocation
  where
    brackets =
      2

commas rowsAmount =
  if rowsAmount <= 1
    then 0
    else pred rowsAmount

{-|
Amount of bytes required for an escaped JSON string value without quotes.

https://hackage.haskell.org/package/text-1.2.4.0/docs/src/Data.Text.Encoding.html#encodeUtf8BuilderEscaped
-}
stringBody :: Text -> Int
stringBody (Text.Text arr off len) =
  go off 0
  where
    go !i !size =
      if i >= len
        then
          size
        else
          if w <= 0x7F
            then
              case w of
                92 {- \\ -} -> step 1 2
                34 {- \" -} -> step 1 2
                _ -> 
                  if w < 32
                    then case w of
                      10 {- \n -} -> step 1 2
                      13 {- \r -} -> step 1 2
                      9 {- \t -} -> step 1 2
                      _ {- control -} -> step 1 6
                    else
                      step 1 1
            else if w <= 0x7FF
              then
                step 1 2
              else if w <= 0xDBFF && w >= 0xD800
                then
                  step 2 4
                else
                  step 1 3
          where
        w =
          TextArray.unsafeIndex arr i
        step a b =
          go (i + a) (size + b)
