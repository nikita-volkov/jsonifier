{-# LANGUAGE UnliftedFFITypes #-}
module Jsonifier.Ffi
where

import Jsonifier.Prelude
import Foreign.C
import GHC.Base (ByteArray#, MutableByteArray#)


foreign import ccall unsafe "static count_string_allocation"
  countStringAllocationSize :: ByteArray# -> CSize -> CSize -> IO CInt

foreign import ccall unsafe "static encode_utf16_as_string"
  encodeString :: Ptr Word8 -> ByteArray# -> CSize -> CSize -> IO (Ptr Word8)
