module Jsonifier.Write
where

import Jsonifier.Prelude
import PtrPoker.Write
import qualified Jsonifier.Poke as Poke


{-# INLINE scientificString #-}
scientificString :: Scientific -> Write
scientificString =
  stringBody . scientificAsciiDec

{-# INLINE stringBody #-}
stringBody :: Write -> Write
stringBody (Write size poke) =
  Write (size + 2) (Poke.doubleQuote <> poke <> Poke.doubleQuote)
