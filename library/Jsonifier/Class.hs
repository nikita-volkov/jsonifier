{-# LANGUAGE  IncoherentInstances #-}

module Jsonifier.Class (
    ToJSON(..)
  , (&=)
)where

import Jsonifier.Basic
    (utf8ByteString,  Json,
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

import Data.These ( These(..) )

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Foreign.C.Types (CTime (..))
import Data.Time ( Day, UTCTime, LocalTime, TimeOfDay, ZonedTime )
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB (toStrict)
import Data.Time.Clock.System (SystemTime (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.IntSet as IntSet
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.UUID as UUID
import Data.Strict.Tuple
import qualified Data.Strict.Maybe as Strict
import qualified Data.Strict.Either as Strict

import Data.Tagged

import Jsonifier.Time (timeOfDay, zonedTime, localTime, utcTime,  day )

(&=) :: (ToJSON a) => Text -> a -> (Text, Json)
name &= value = (name, toJson value)
{-# INLINE (&=) #-}

class (Show a) => ToJSONKey a where
    toJKey :: a -> T.Text
    toJKey  = T.pack . show
    {-# INLINE toJKey #-}

instance ToJSONKey Int
instance ToJSONKey Int8
instance ToJSONKey Int16
instance ToJSONKey Int32
instance ToJSONKey Int64
instance ToJSONKey Word
instance ToJSONKey Word8
instance ToJSONKey Word16
instance ToJSONKey Word32
instance ToJSONKey Word64
instance ToJSONKey Integer
instance ToJSONKey Double
instance ToJSONKey Float
instance ToJSONKey Bool
instance ToJSONKey Day
instance ToJSONKey TimeOfDay
instance ToJSONKey LocalTime
instance ToJSONKey ZonedTime
instance ToJSONKey UTCTime

instance ToJSONKey Char where
    toJKey = T.singleton
    {-# INLINE toJKey #-}

instance ToJSONKey String where
    toJKey = T.pack
    {-# INLINE toJKey #-}

instance ToJSONKey T.Text where
    toJKey = id
    {-# INLINE toJKey #-}

instance ToJSONKey LT.Text where
    toJKey = LT.toStrict
    {-# INLINE toJKey #-}

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

instance ToJSON Word8 where
    toJson = wordNumber . fromIntegral
    {-# INLINE toJson #-}

instance ToJSON Word16 where
    toJson = wordNumber . fromIntegral
    {-# INLINE toJson #-}

instance ToJSON Word32 where
    toJson = wordNumber . fromIntegral
    {-# INLINE toJson #-}

instance ToJSON Word64 where
    toJson = wordNumber . fromIntegral
    {-# INLINE toJson #-}

instance ToJSON Int8 where
    toJson = intNumber . fromIntegral
    {-# INLINE toJson #-}

instance ToJSON Int16 where
    toJson = intNumber . fromIntegral
    {-# INLINE toJson #-}

instance ToJSON Int32 where
    toJson = intNumber . fromIntegral
    {-# INLINE toJson #-}

instance ToJSON Int64 where
    toJson = intNumber . fromIntegral
    {-# INLINE toJson #-}

instance ToJSON Integer where
    toJson = intNumber . fromInteger
    {-# INLINE toJson #-}

instance ToJSON Float where
    toJson = doubleNumber . realToFrac
    {-# INLINE toJson #-}

instance ToJSON Double where
    toJson = doubleNumber
    {-# INLINE toJson #-}

instance ToJSON Scientific where
    toJson = scientificNumber
    {-# INLINE toJson #-}

instance ToJSON Ordering where
    toJson o = case o of
                LT -> utf8ByteString "LT"
                EQ -> utf8ByteString "EQ"
                GT -> utf8ByteString "GT"

    {-# INLINE toJson #-}

instance ToJSON Char where
    toJson c = textString (T.singleton c)
    {-# INLINE toJson #-}

instance {-# OVERLAPPING #-} ToJSON String where
    {-# SPECIALIZE instance ToJSON String #-}
    toJson c = textString (T.pack c)
    {-# INLINE toJson #-}

instance ToJSON Text where
    toJson = textString
    {-# INLINE toJson #-}

instance ToJSON LT.Text where
    toJson = textString . LT.toStrict
    {-# INLINE toJson #-}

instance {-# OVERLAPPING #-} (ToJSON a) => ToJSON (Maybe a) where
    toJson (Just a) = toJson a
    toJson _        = null
    {-# INLINE toJson #-}

instance {-# OVERLAPPING #-} (ToJSON a) => ToJSON (Strict.Maybe a) where
    toJson (Strict.Just a) = toJson a
    toJson _               = null
    {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
    toJson (Left x)  = object [ "Left" &=  x]
    toJson (Right x) = object [ "Right" &=  x]
    {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b) => ToJSON (Strict.Either a b) where
    toJson (Strict.Left x)  = object [ "Left" &=  x]
    toJson (Strict.Right x) = object [ "Right" &=  x]
    {-# INLINE toJson #-}

instance ToJSON Version where
    toJson = toJson . showVersion
    {-# INLINE toJson #-}

instance ToJSON Void where
    toJson = absurd
    {-# INLINE toJson #-}

instance (ToJSON a) => ToJSON [a] where
    toJson xs = array $ fmap toJson xs
    {-# INLINE toJson #-}

instance (ToJSON a) => ToJSON (NonEmpty a) where
    toJson xs = array $ fmap toJson xs
    {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b) => ToJSON (These a b) where
    toJson (This a)    = object [ "This" &= a ]
    toJson (That b)    = object [ "That" &= b ]
    toJson (These a b) = object [ "This" &= a, "That" &= b ]
    {-# INLINE toJson #-}

instance (ToJSON a, Integral a) => ToJSON (Ratio a) where
    toJson r = object [ "numerator"   &= numerator   r
                      , "denominator" &= denominator r
                      ]
    {-# INLINE toJson #-}

instance (ToJSON a) => ToJSON (Tagged t a) where
    toJson = toJson . untag
    {-# INLINE toJson #-}

instance ToJSON UUID.UUID where
    toJson = toJson . UUID.toText
    {-# INLINE toJson #-}

instance (ToJSON a) => ToJSON (Seq.Seq a) where
    toJson xs = array $ fmap toJson xs
    {-# INLINE toJson #-}

instance (ToJSON a) => ToJSON (Set.Set a) where
    toJson xs = array $ fmap toJson (Set.toList xs)
    {-# INLINE toJson #-}

instance (ToJSON a) => ToJSON (HashSet.HashSet a) where
    toJson xs = array $ fmap toJson (HashSet.toList xs)
    {-# INLINE toJson #-}

instance (ToJSONKey k, ToJSON v) => ToJSON (M.Map k v) where
    toJson xs = object $ map (\(k, v) -> (toJKey k, toJson v)) (M.toList xs)
    {-# INLINE toJson #-}

instance (ToJSONKey k, ToJSON v) => ToJSON (HM.HashMap k v) where
    toJson xs = object $ map (\(k, v) -> (toJKey k, toJson v)) (HM.toList xs)
    {-# INLINE toJson #-}

instance ToJSON IntSet.IntSet where
    toJson xs = array $ fmap toJson (IntSet.toList xs)
    {-# INLINE toJson #-}

instance (ToJSON a) => ToJSON (V.Vector a) where
    toJson xs = array $ V.map toJson xs
    {-# INLINE toJson #-}

instance (ToJSON a, U.Unbox a) => ToJSON (U.Vector a) where
    toJson xs = array $ V.map toJson (U.convert xs)
    {-# INLINE toJson #-}

instance HasResolution a => ToJSON (Fixed a) where
    toJson = doubleNumber . realToFrac
    {-# INLINE toJson #-}

instance {-# OVERLAPPABLE #-} (Foldable f) => ToJSON (f Json) where
    toJson = array
    {-# INLINE toJson #-}

instance {-# OVERLAPPABLE #-} (Foldable f) => ToJSON (f (Text, Json)) where
    toJson = object
    {-# INLINE toJson #-}

instance ToJSON Day where
    toJson = utf8ByteString . LB.toStrict . B.toLazyByteString . day
    {-# INLINE toJson #-}

instance ToJSON LocalTime where
    toJson = utf8ByteString . LB.toStrict . B.toLazyByteString . localTime
    {-# INLINE toJson #-}

instance ToJSON ZonedTime where
    toJson = utf8ByteString . LB.toStrict . B.toLazyByteString . zonedTime
    {-# INLINE toJson #-}

instance ToJSON UTCTime where
    toJson = utf8ByteString . LB.toStrict . B.toLazyByteString . utcTime
    {-# INLINE toJson #-}

instance ToJSON TimeOfDay where
    toJson = utf8ByteString . LB.toStrict . B.toLazyByteString . timeOfDay
    {-# INLINE toJson #-}

-- instance ToJSON SystemTime where
--     toJson (MkSystemTime secs nsecs) = toJson (fromIntegral secs + fromIntegral nsecs / 1000000000 :: Nano)
--     {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b) => ToJSON (Pair a b) where
    toJson (a :!: b) = array [toJson a, toJson b]
    {-# INLINE toJson #-}

instance ToJSON () where
    toJson _ = array []
    {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b) => ToJSON (a, b) where
    toJson (a, b) = array [toJson a, toJson b]
    {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a, b, c) where
    toJson (a, b, c) = array [toJson a, toJson b, toJson c]
    {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (a, b, c, d) where
    toJson (a, b, c, d) = array [toJson a, toJson b, toJson c, toJson d]
    {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) => ToJSON (a, b, c, d, e) where
    toJson (a, b, c, d, e) = array [toJson a, toJson b, toJson c, toJson d, toJson e]
    {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f) => ToJSON (a, b, c, d, e, f) where
    toJson (a, b, c, d, e, f) = array [toJson a, toJson b, toJson c, toJson d, toJson e, toJson f]
    {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g) => ToJSON (a, b, c, d, e, f, g) where
    toJson (a, b, c, d, e, f, g) = array [toJson a, toJson b, toJson c, toJson d, toJson e, toJson f, toJson g]
    {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h) => ToJSON (a, b, c, d, e, f, g, h) where
    toJson (a, b, c, d, e, f, g, h) = array [toJson a, toJson b, toJson c, toJson d, toJson e, toJson f, toJson g, toJson h]
    {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i) => ToJSON (a, b, c, d, e, f, g, h, i) where
    toJson (a, b, c, d, e, f, g, h, i) = array [toJson a, toJson b, toJson c, toJson d, toJson e, toJson f, toJson g, toJson h, toJson i]
    {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j) => ToJSON (a, b, c, d, e, f, g, h, i, j) where
    toJson (a, b, c, d, e, f, g, h, i, j) = array [toJson a, toJson b, toJson c, toJson d, toJson e, toJson f, toJson g, toJson h, toJson i, toJson j]
    {-# INLINE toJson #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k) => ToJSON (a, b, c, d, e, f, g, h, i, j, k) where
    toJson (a, b, c, d, e, f, g, h, i, j, k) = array [toJson a, toJson b, toJson c, toJson d, toJson e, toJson f, toJson g, toJson h, toJson i, toJson j, toJson k]
    {-# INLINE toJson #-}
