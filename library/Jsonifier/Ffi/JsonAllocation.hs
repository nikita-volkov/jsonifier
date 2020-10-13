{-# LANGUAGE UnliftedFFITypes #-}
module Jsonifier.Ffi.JsonAllocation
where

import Jsonifier.Prelude
import Foreign.C
import GHC.Base (ByteArray#, MutableByteArray#)


foreign import ccall unsafe
  "static count_string_allocation"
  string
    :: ByteArray# -> CSize -> CSize -> IO CInt
