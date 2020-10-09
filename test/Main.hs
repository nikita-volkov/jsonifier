module Main where

import Prelude hiding (assert)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import qualified Test.QuickCheck as QuickCheck
import qualified Data.Aeson as Aeson
import qualified JsonLego as JL
import qualified Data.HashMap.Strict as HashMap


main =
  defaultMain $ 
  testGroup "All tests" [
    testProperty "Rendering random JSON tree and parsing it produces the same tree" $
      \ aeson ->
        let
          byteString =
            JL.value (aesonJL aeson)
          decoding =
            Aeson.eitherDecodeStrict' byteString
          in Right aeson === decoding
    ]

instance Arbitrary Aeson.Value where
  arbitrary =
    genericArbitrary
  shrink =
    genericShrink

aesonJL :: Aeson.Value -> JL.Value
aesonJL =
  \ case
    Aeson.Null ->
      JL.null
    Aeson.Bool a ->
      JL.bool a
    Aeson.Number a ->
      JL.scientificNumber a
    Aeson.String a ->
      JL.string a
    Aeson.Array a ->
      JL.array (foldMap (JL.element . aesonJL) a)
    Aeson.Object a ->
      JL.object (HashMap.foldMapWithKey (\ k -> JL.row k . aesonJL) a)
