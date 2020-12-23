{-# LANGUAGE TypeApplications #-}


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

import qualified Data.List.NonEmpty as NE


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "jsonifier" [ regression ]

encodeStrict = LB.toStrict . A.encode

boundedTest :: (Bounded a, A.ToJSON a, J.ToJSON a, Typeable a) => a -> [TestTree]
boundedTest x =
  [
      testCase (show (typeOf x)) $  ((J.toByteString . J.toJson)  x) @?= encodeStrict x
  ]

integralTest :: (Integral a, A.ToJSON a, J.ToJSON a, Typeable a) => a -> [TestTree]
integralTest x =
  [
      testCase (show (typeOf x)) $  ((J.toByteString . J.toJson)  x) @?= encodeStrict x
  ]

floatingTest :: (Floating a, A.ToJSON a, J.ToJSON a, Typeable a) => a -> [TestTree]
floatingTest x =
  [
      testCase (show (typeOf x)) $ ((J.toByteString . J.toJson) x) @?= encodeStrict x
  ]

stringTest :: (IsString a, A.ToJSON a, J.ToJSON a, Typeable a) => a -> [TestTree]
stringTest x =
  [
      testCase (show (typeOf x)) $ ((J.toByteString . J.toJson) x) @?= encodeStrict x
  ]

jsonTest :: (A.ToJSON a, J.ToJSON a, Typeable a) => a -> [TestTree]
jsonTest x =
  [
      testCase (show (typeOf x)) $ ((J.toByteString . J.toJson) x) @?= encodeStrict x
  ]

regression :: TestTree
regression = testGroup "aeson" $
  boundedTest (minBound :: Word)    <>
  boundedTest (maxBound :: Word)    <>
  boundedTest (minBound :: Word8)   <>
  boundedTest (maxBound :: Word8)   <>
  boundedTest (minBound :: Word16)  <>
  boundedTest (maxBound :: Word16)  <>
  boundedTest (minBound :: Word32)  <>
  boundedTest (maxBound :: Word32)  <>
  boundedTest (minBound :: Word64)  <>
  boundedTest (maxBound :: Word64)  <>
  boundedTest (minBound :: Int)     <>
  boundedTest (maxBound :: Int)     <>
  boundedTest (minBound :: Int8)    <>
  boundedTest (maxBound :: Int8)    <>
  boundedTest (minBound :: Int16)   <>
  boundedTest (maxBound :: Int16)   <>
  boundedTest (minBound :: Int32)   <>
  boundedTest (maxBound :: Int32)   <>
  boundedTest (minBound :: Int64)   <>
  boundedTest (maxBound :: Int64)   <>
  integralTest (0    :: Integer)    <>
  integralTest (42   :: Integer)    <>
  -- floatingTest (0.1  :: Float)      <>
  -- floatingTest (42.1 :: Float)      <>
  -- floatingTest (0.1  :: Double)     <>
  --- floatingTest (42.1 :: Double)    <>
  [ testCase "Char" $  ((J.toByteString . J.toJson) 'l') @?= encodeStrict 'l' ] <>
  stringTest ("HelloWorld" :: String)  <>
  stringTest ("HelloWorld" :: T.Text)  <>
  stringTest ("HelloWorld" :: LT.Text) <>

  jsonTest (Nothing @ Int)              <>
  jsonTest (Just @ Int 1)               <>

  jsonTest ([] :: [Int])                <>
  jsonTest ([1,2,3] :: [Int])           <>
  jsonTest ( NE.fromList @ Int [1,2,3]) <>

  jsonTest (Left 'c' :: Either Char Int) <>
  jsonTest (Right 1  :: Either Char Int) <>

  boundedTest (True)                    <>
  boundedTest (False)
