{-# LANGUAGE CPP #-}
module Jsonifier.Text
where

import Jsonifier.Prelude
import qualified Data.Text.Internal as Text
import qualified Data.Text.Array as TextArray


{-# INLINE destruct #-}
destruct :: (ByteArray# -> Int -> Int -> x) -> Text -> x
#if MIN_VERSION_text(2,0,0)
destruct k (Text.Text (TextArray.ByteArray arr) off len) = k arr off len
#else
destruct k (Text.Text (TextArray.aBA -> arr) off len) = k arr off len
#endif
