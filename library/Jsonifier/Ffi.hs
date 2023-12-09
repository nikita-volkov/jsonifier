{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Jsonifier.Ffi where

import Foreign.C
import GHC.Base (ByteArray#, MutableByteArray#)
import Jsonifier.Prelude

#if MIN_VERSION_text (2, 0, 0)

foreign import ccall unsafe "static measure_utf8_text_off_len"
  countTextEncoding :: ByteArray# -> CSize -> CSize -> IO CInt

foreign import ccall unsafe "static encode_utf8_text"
  encodeText :: Ptr Word8 -> ByteArray# -> CSize -> CSize -> IO (Ptr Word8)

#else

foreign import ccall unsafe "static measure_utf16_text_off_len"
  countTextEncoding :: ByteArray# -> CSize -> CSize -> IO CInt

foreign import ccall unsafe "static encode_utf16_text"
  encodeText :: Ptr Word8 -> ByteArray# -> CSize -> CSize -> IO (Ptr Word8)

#endif
