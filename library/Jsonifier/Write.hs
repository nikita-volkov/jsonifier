module Jsonifier.Write where

import qualified Jsonifier.Poke as Poke
import Jsonifier.Prelude
import PtrPoker.Write

{-# INLINE scientificString #-}
scientificString :: Scientific -> Write
scientificString =
  stringBody . scientificAsciiDec

{-# INLINE stringBody #-}
stringBody :: Write -> Write
stringBody (Write size poke) =
  Write (size + 2) (Poke.doubleQuote <> poke <> Poke.doubleQuote)
