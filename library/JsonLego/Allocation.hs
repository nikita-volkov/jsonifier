module JsonLego.Allocation
where

import JsonLego.Prelude


{-|
Amount of bytes required for an escaped JSON string value without quotes.
-}
stringBody :: Text -> Int
stringBody =
  error "TODO"

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
