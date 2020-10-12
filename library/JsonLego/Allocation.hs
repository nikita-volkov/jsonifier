{-# LANGUAGE UnliftedFFITypes #-}
module JsonLego.Allocation
where

import JsonLego.Prelude
import qualified Data.Text.Internal as Text
import qualified Data.Text.Array as TextArray
import qualified JsonLego.Ffi.JsonAllocation as JsonAllocationFfi


{-# INLINE object #-}
object :: Int -> Int -> Int
object rowsAmount contentsAllocation =
  curlies + commas rowsAmount + colonsAndQuotes + contentsAllocation
  where
    curlies =
      2
    colonsAndQuotes =
      rowsAmount * 3

{-# INLINE array #-}
array :: Int -> Int -> Int
array elementsAmount contentsAllocation =
  brackets + commas elementsAmount + contentsAllocation
  where
    brackets =
      2

{-# INLINE commas #-}
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
  JsonAllocationFfi.string
    (TextArray.aBA arr) (fromIntegral off) (fromIntegral len)
    & unsafeDupablePerformIO
    & fromIntegral
