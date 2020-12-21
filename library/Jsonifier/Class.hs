module Jsonifier.Class (
    ToJSON(..)
  , (&=)
)where

import Jsonifier.Basic
    ( Json,
      null,
      bool,
      intNumber,
      wordNumber,
      doubleNumber,
      scientificNumber,
      textString,
      array,
      object )

import Data.ByteString ( ByteString )
import Jsonifier.Prelude hiding (null, bool)


(&=) :: (ToJSON a) => Text -> a -> (Text, Json)
name &= value = (name, toJson value)
{-# INLINE (&=) #-}


class ToJSON a where
    toJson :: a -> Json

instance ToJSON Int where
    toJson = intNumber
    {-# INLINE toJson #-}

instance ToJSON Bool where
    toJson = bool
    {-# INLINE toJson #-}

instance ToJSON Word where
    toJson = wordNumber
    {-# INLINE toJson #-}

instance ToJSON Double where
    toJson = doubleNumber
    {-# INLINE toJson #-}

instance ToJSON Scientific where
    toJson = scientificNumber
    {-# INLINE toJson #-}

instance ToJSON Text where
    toJson = textString
    {-# INLINE toJson #-}

instance (Foldable f) => ToJSON (f Json) where
    toJson = array
    {-# INLINE toJson #-}

instance (Foldable f) => ToJSON (f (Text, Json)) where
    toJson = object
    {-# INLINE toJson #-}

instance (ToJSON a) => ToJSON (Maybe a) where
    toJson (Just a) = toJson a
    toJson _        = null
    {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b) => ToJSON (a, b) where
    toJson (a, b) = array [toJson a, toJson b]
    {-# INLINE toJson #-}

instance (ToJSON a) => ToJSON [a] where
    toJson xs = array $ fmap toJson xs
    {-# INLINE toJson #-}
