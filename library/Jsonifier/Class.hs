module Jsonifier.Class where

import Jsonifier.Basic

import Data.ByteString ( ByteString )
import Jsonifier.Prelude hiding (null, bool)


(&=) :: (ToJSON a) => Text -> a -> (Text, Json)
name &= value = (name, encode value)
{-# INLINE (&=) #-}


class ToJSON a where
    encode :: a -> Json

instance ToJSON Int where
    encode = intNumber
    {-# INLINE encode #-}

instance ToJSON Bool where
    encode = bool
    {-# INLINE encode #-}

instance ToJSON Word where
    encode = wordNumber
    {-# INLINE encode #-}

instance ToJSON Double where
    encode = doubleNumber
    {-# INLINE encode #-}

instance ToJSON Scientific where
    encode = scientificNumber
    {-# INLINE encode #-}

instance ToJSON Text where
    encode = textString
    {-# INLINE encode #-}

instance (Foldable f) => ToJSON (f Json) where
    encode = array
    {-# INLINE encode #-}

instance (Foldable f) => ToJSON (f (Text, Json)) where
    encode = object
    {-# INLINE encode #-}

instance (ToJSON a) => ToJSON (Maybe a) where
    encode (Just a) = encode a
    encode _        = null
    {-# INLINE encode #-}
