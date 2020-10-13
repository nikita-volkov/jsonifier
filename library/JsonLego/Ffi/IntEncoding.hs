module JsonLego.Ffi.IntEncoding
where

import JsonLego.Prelude
import Foreign.C

foreign import ccall unsafe "static write_int64_in_reverse"
  writeInt64InReverse :: Ptr Word8 -> CLLong -> IO CInt

foreign import ccall unsafe "static poke_int64_in_reverse"
  pokeInt64InReverse :: Ptr Word8 -> CLLong -> IO (Ptr Word8)
