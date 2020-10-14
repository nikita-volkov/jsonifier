module Main where

import Prelude hiding (null, bool, Ap)
import Hedgehog
import Hedgehog.Main
import qualified Data.Aeson as A
import qualified Jsonifier as J
import qualified Data.HashMap.Strict as HashMap
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


main =
  defaultMain $ pure $ checkParallel $ $$(discover)

prop_sample =
  withTests 1 $
  property $ do
    sample <- liftIO $ load "samples/twitter100.json"
    A.eitherDecodeStrict' (J.toByteString (aesonJson sample)) === Right sample
  where
    load :: FilePath -> IO A.Value
    load fileName =
      A.eitherDecodeFileStrict' fileName
        >>= either fail return
    aesonJson :: A.Value -> J.Json
    aesonJson =
      \ case
        A.Null ->
          J.null
        A.Bool a ->
          J.bool a
        A.Number a ->
          J.scientificNumber a
        A.String a ->
          J.textString a
        A.Array a ->
          J.array (fmap aesonJson a)
        A.Object a ->
          J.object (HashMap.foldMapWithKey (\ k -> (: []) . (,) k . aesonJson) a)

prop_aesonRoundtrip =
  withTests 99999 $
  property $ do
    (encoding, aeson) <- forAll gen
    A.eitherDecodeStrict' encoding === Right aeson
  where
    gen :: Gen (ByteString, A.Value)
    gen =
      json <&> first J.toByteString
      where
        json =
          Gen.recursive Gen.choice [
            null, bool,
            intNumber, doubleNumber, scientificNumber,
            textString
            ] [
            array, object
            ]
        null =
          pure (J.null, A.Null)
        bool =
          Gen.bool <&> \ a -> (J.bool a, A.Bool a)
        intNumber =
          Gen.int Range.linearBounded <&> \ a -> (J.intNumber a, A.Number (fromIntegral a))
        doubleNumber =
          realFloatNumber J.doubleNumber
        realFloatNumber jsonifier =
          realFloat (Range.exponentialFloat (-9999) 9999) <&> \ a ->
            (jsonifier a,
              A.Number (if isNaN a || isInfinite a then 0 else realToFrac a))
          where
            realFloat range =
              Gen.frequency [
                (99, Gen.realFloat range),
                (1, nonReal)
                ]
              where
                nonReal =
                  Gen.element [0 / 0, 1 / 0, (-1) / 0, -0]
        scientificNumber =
          Gen.realFrac_
            (Range.linearFrac (-99999999999999) 99999999999999)
            <&> \ a -> (J.scientificNumber a, A.Number a)
        textString =
          Gen.text (Range.exponential 0 9999) Gen.unicode
            <&> \ a -> (J.textString a, A.String a)
        array =
          Gen.list (Range.exponential 0 999) json
            <&> \ a -> (
              J.array (fmap fst a),
              A.Array (fromList (fmap snd a))
              )
        object =
          Gen.list (Range.exponential 0 999) row <&> \ a -> (
            J.object (fmap (fmap fst) a),
            A.Object (fromList (fmap (fmap snd) a))
            )
          where
            row =
              Gen.text (Range.exponential 0 99) Gen.unicode
                & fmap (,)
                & flip ap json
