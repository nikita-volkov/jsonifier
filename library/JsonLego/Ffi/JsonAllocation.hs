{-# LANGUAGE UnliftedFFITypes #-}
module JsonLego.Ffi.JsonAllocation
where

import JsonLego.Prelude
import Foreign.C
import GHC.Base (ByteArray#, MutableByteArray#)


foreign import ccall unsafe
  "static count_string_allocation"
  string
    :: ByteArray# -> CSize -> CSize -> IO CInt
