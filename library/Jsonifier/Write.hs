module Jsonifier.Write
where

import Jsonifier.Prelude
import PtrPoker.Write
import qualified Jsonifier.Poke as Poke


{-# INLINE scientificString #-}
scientificString :: Scientific -> Write
scientificString a =
  case scientificAsciiDec a of
    Write size poke ->
      Write (size + 2) (Poke.doubleQuote <> poke <> Poke.doubleQuote)
