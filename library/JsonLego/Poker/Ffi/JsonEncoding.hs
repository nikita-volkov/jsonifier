{-# LANGUAGE UnliftedFFITypes #-}
module JsonLego.Poker.Ffi.JsonEncoding
where

import JsonLego.Prelude
import Foreign.C
import GHC.Base (ByteArray#, MutableByteArray#)


foreign import ccall unsafe
  "static _hs_json_lego_encode_string"
  string
    :: Ptr Word8 -> ByteArray# -> CSize -> CSize -> IO (Ptr Word8)
