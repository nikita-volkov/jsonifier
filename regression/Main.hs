{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (null, bool)
import qualified Data.Aeson as A
import qualified Jsonifier as J
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Data.ByteString.Lazy as LB (toStrict)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Array as Arr
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.List.NonEmpty as NE


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "jsonifier" [ regression ]

encodeStrict :: A.ToJSON a => a -> ByteString
encodeStrict = LB.toStrict . A.encode

checkAesonCompliance :: (Show a, A.ToJSON a, J.ToJSON a, Typeable a) => a -> [TestTree]
checkAesonCompliance x =
  [
      testCase (show (typeOf x) <> " -> (" <> show x <> ")") $ (J.toByteString . J.toJson) x @?= encodeStrict x
  ]

regression :: TestTree
regression = testGroup "checkAsonCompliance" $
  checkAesonCompliance (minBound :: Word)    <>
  checkAesonCompliance (maxBound :: Word)    <>
  checkAesonCompliance (minBound :: Word8)   <>
  checkAesonCompliance (maxBound :: Word8)   <>
  checkAesonCompliance (minBound :: Word16)  <>
  checkAesonCompliance (maxBound :: Word16)  <>
  checkAesonCompliance (minBound :: Word32)  <>
  checkAesonCompliance (maxBound :: Word32)  <>
  checkAesonCompliance (minBound :: Word64)  <>
  checkAesonCompliance (maxBound :: Word64)  <>
  checkAesonCompliance (minBound :: Int)     <>
  checkAesonCompliance (maxBound :: Int)     <>
  checkAesonCompliance (minBound :: Int8)    <>
  checkAesonCompliance (maxBound :: Int8)    <>
  checkAesonCompliance (minBound :: Int16)   <>
  checkAesonCompliance (maxBound :: Int16)   <>
  checkAesonCompliance (minBound :: Int32)   <>
  checkAesonCompliance (maxBound :: Int32)   <>
  checkAesonCompliance (minBound :: Int64)   <>
  checkAesonCompliance (maxBound :: Int64)   <>
  checkAesonCompliance (0    :: Integer)     <>
  checkAesonCompliance (42   :: Integer)     <>
  -- checkAesonCompliance (0.1  :: Float)      <>
  -- checkAesonCompliance (42.1 :: Float)      <>
  -- checkAesonCompliance (0.1  :: Double)     <>
  -- checkAesonCompliance (42.1 :: Double)     <>

  checkAesonCompliance ('l' :: Char)                 <>

  checkAesonCompliance ("HelloWorld" :: String)      <>
  checkAesonCompliance ("HelloWorld" :: T.Text)      <>
  checkAesonCompliance ("HelloWorld" :: LT.Text)     <>

  checkAesonCompliance (Nothing @ Int)               <>
  checkAesonCompliance (Just @ Int 1)                <>

  checkAesonCompliance ([] :: [Int])                 <>
  checkAesonCompliance ([1,2,3] :: [Int])            <>
  checkAesonCompliance ( NE.fromList @ Int [1,2,3])  <>

  checkAesonCompliance (Left 'c' :: Either Char Int) <>
  checkAesonCompliance (Right 1  :: Either Char Int) <>

  checkAesonCompliance (makeVersion [1,2,3])         <>

  -- checkAesonCompliance (This 'c'    :: These Char Int) <>
  -- checkAesonCompliance (That 1      :: These Char Int) <>
  -- checkAesonCompliance (These 'c' 1 :: These Char Int) <>

  checkAesonCompliance (1 % 3 :: Ratio Int)                <>

  checkAesonCompliance (Seq.fromList @ Int [1,2,3] )       <>
  checkAesonCompliance (Set.fromList @ Int [1,2,3] )       <>
  checkAesonCompliance (V.fromList @ Int [1,2,3] )         <>
  checkAesonCompliance (U.fromList @ Int [1,2,3] )         <>
  checkAesonCompliance (1 :: Micro)                        <>

  checkAesonCompliance ()                                  <>
  checkAesonCompliance ((1,2) :: (Int,Int))                <>
  checkAesonCompliance ((1,2,3) :: (Int,Int,Int))          <>
  checkAesonCompliance ((1,2,3,'a') :: (Int,Int,Int,Char)) <>
  checkAesonCompliance ((1,2,3,'a') :: (Int,Int,Int,Char)) <>

  checkAesonCompliance (ModifiedJulianDay 10425)                                                                <>
  checkAesonCompliance (LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 0 0 ))                                     <>
  checkAesonCompliance (ZonedTime (LocalTime (ModifiedJulianDay 42) (TimeOfDay 0 0 0 )) (TimeZone 0 True "us")) <>
  checkAesonCompliance (UTCTime (ModifiedJulianDay 42) (secondsToDiffTime 123))                                 <>
  checkAesonCompliance (TimeOfDay 19 03 45)        <>
  checkAesonCompliance True                        <>
  checkAesonCompliance False
