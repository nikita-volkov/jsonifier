{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}


module Main where

import Prelude hiding (null, bool)
import qualified Data.Aeson as A
import qualified Jsonifier as J
import Test.Tasty
import Test.Tasty.HUnit
import Data.ByteString.Lazy as LB (toStrict)
import Data.Typeable
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Debug.Trace
import Data.These
import Data.Ratio
import Data.Fixed
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Array as Arr
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import qualified Data.List.NonEmpty as NE
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Clock
-- import Data.Time.Clock.System


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "jsonifier" [ regression ]

encodeStrict = LB.toStrict . A.encode

aesonTest :: (Show a, A.ToJSON a, J.ToJSON a, Typeable a) => a -> [TestTree]
aesonTest x =
  [
      testCase (show (typeOf x) <> " -> (" <> show x <> ")") $ ((J.toByteString . J.toJson) x) @?= encodeStrict x
  ]

regression :: TestTree
regression = testGroup "aeson" $
  aesonTest (minBound :: Word)    <>
  aesonTest (maxBound :: Word)    <>
  aesonTest (minBound :: Word8)   <>
  aesonTest (maxBound :: Word8)   <>
  aesonTest (minBound :: Word16)  <>
  aesonTest (maxBound :: Word16)  <>
  aesonTest (minBound :: Word32)  <>
  aesonTest (maxBound :: Word32)  <>
  aesonTest (minBound :: Word64)  <>
  aesonTest (maxBound :: Word64)  <>
  aesonTest (minBound :: Int)     <>
  aesonTest (maxBound :: Int)     <>
  aesonTest (minBound :: Int8)    <>
  aesonTest (maxBound :: Int8)    <>
  aesonTest (minBound :: Int16)   <>
  aesonTest (maxBound :: Int16)   <>
  aesonTest (minBound :: Int32)   <>
  aesonTest (maxBound :: Int32)   <>
  aesonTest (minBound :: Int64)   <>
  aesonTest (maxBound :: Int64)   <>
  aesonTest (0    :: Integer)     <>
  aesonTest (42   :: Integer)     <>
  -- aesonTest (0.1  :: Float)      <>
  -- aesonTest (42.1 :: Float)      <>
  -- aesonTest (0.1  :: Double)     <>
  -- aesonTest (42.1 :: Double)     <>

  aesonTest ('l' :: Char)                 <>

  aesonTest ("HelloWorld" :: String)      <>
  aesonTest ("HelloWorld" :: T.Text)      <>
  aesonTest ("HelloWorld" :: LT.Text)     <>

  aesonTest (Nothing @ Int)               <>
  aesonTest (Just @ Int 1)                <>

  aesonTest ([] :: [Int])                 <>
  aesonTest ([1,2,3] :: [Int])            <>
  aesonTest ( NE.fromList @ Int [1,2,3])  <>

  aesonTest (Left 'c' :: Either Char Int) <>
  aesonTest (Right 1  :: Either Char Int) <>

  aesonTest (makeVersion [1,2,3])         <>

  -- aesonTest (This 'c'    :: These Char Int) <>
  -- aesonTest (That 1      :: These Char Int) <>
  -- aesonTest (These 'c' 1 :: These Char Int) <>

  aesonTest (1 % 3 :: Ratio Int)                <>

  aesonTest (Seq.fromList @ Int [1,2,3] )       <>
  aesonTest (Set.fromList @ Int [1,2,3] )       <>
  aesonTest (V.fromList @ Int [1,2,3] )         <>
  aesonTest (U.fromList @ Int [1,2,3] )         <>
  aesonTest (1 :: Micro)                        <>

  aesonTest ()                                  <>
  aesonTest ((1,2) :: (Int,Int))                <>
  aesonTest ((1,2,3) :: (Int,Int,Int))          <>
  aesonTest ((1,2,3,'a') :: (Int,Int,Int,Char)) <>
  aesonTest ((1,2,3,'a') :: (Int,Int,Int,Char)) <>

  aesonTest (ModifiedJulianDay 10425)                                                                <>
  aesonTest (LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 0 0 ))                                     <>
  aesonTest (ZonedTime (LocalTime (ModifiedJulianDay 42) (TimeOfDay 0 0 0 )) (TimeZone 0 True "us")) <>
  aesonTest (UTCTime (ModifiedJulianDay 42) (secondsToDiffTime 123))                                 <>
  aesonTest (TimeOfDay 19 03 45)        <>
  aesonTest True                        <>
  aesonTest False
