{-# LANGUAGE UnliftedFFITypes #-}
module Jsonifier.Ffi.JsonEncoding
where

import Jsonifier.Prelude
import Foreign.C
import GHC.Base (ByteArray#, MutableByteArray#)


foreign import ccall unsafe
  "static encode_utf16_as_string"
  string
    :: Ptr Word8 -> ByteArray# -> CSize -> CSize -> IO (Ptr Word8)
