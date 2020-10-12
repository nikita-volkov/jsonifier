module JsonLego.Ffi.Dtoa
where

import JsonLego.Prelude
import Foreign.C

foreign import ccall unsafe "static dtoa_grisu3"
  pokeDouble :: Double -> Ptr Word8 -> IO CInt
