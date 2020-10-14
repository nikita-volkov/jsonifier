module Main where

import Prelude hiding (null, bool)
import Hedgehog
import Hedgehog.Main
import qualified Data.Aeson as A
import qualified Jsonifier as J
import qualified Data.HashMap.Strict as HashMap
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.ByteString.Char8 as Char8ByteString


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
  withTests 999 $
  property $ do
    sample <- forAll sampleGen
    let encoding = sampleJsonifier sample
    annotate (Char8ByteString.unpack encoding)
    A.eitherDecodeStrict' encoding === Right (sampleAeson sample)

data Sample =
  NullSample |
  BoolSample Bool |
  IntNumberSample Int |
  DoubleNumberSample Double |
  ScientificNumberSample Scientific |
  TextStringSample Text |
  ArraySample [Sample] |
  ObjectSample [(Text, Sample)]
  deriving (Eq, Show)

sampleGen :: Gen Sample
sampleGen =
  sample
  where
    sample =
      Gen.recursive Gen.choice [
        null, bool,
        intNumber,
        doubleNumber,
        scientificNumber,
        textString
        ] [
        array, object
        ]
    null =
      pure NullSample
    bool =
      Gen.bool <&> BoolSample
    intNumber =
      Gen.int Range.linearBounded <&> IntNumberSample
    doubleNumber =
      realFloatNumber DoubleNumberSample
    realFloatNumber pack =
      realFloat (Range.exponentialFloat (-9999) 9999) <&> pack
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
      scientific (Range.linearFrac (-99999999999999) 99999999999999)
        <&> ScientificNumberSample
      where
        scientific range =
          Gen.realFrac_ range <&> fromRational
    textString =
      Gen.text (Range.exponential 0 9999) Gen.unicode <&> TextStringSample
    array =
      Gen.list (Range.exponential 0 999) sample <&> ArraySample
    object =
      rowList (Range.exponential 0 999) <&> ObjectSample
      where
        rowList range =
          Gen.list range row <&>
          nubBy (on (==) fst)
          where
            row =
              Gen.text (Range.exponential 0 99) Gen.unicode
                & fmap (,)
                & flip ap sample

sampleJsonifier :: Sample -> ByteString
sampleJsonifier =
  J.toByteString . sample
  where
    sample =
      \ case
        NullSample -> J.null
        BoolSample a -> J.bool a
        IntNumberSample a -> J.intNumber a
        DoubleNumberSample a -> J.doubleNumber a
        ScientificNumberSample a -> J.scientificNumber a
        TextStringSample a -> J.textString a
        ArraySample a -> J.array (fmap sample a)
        ObjectSample a -> J.object (fmap (fmap sample) a)

sampleAeson :: Sample -> A.Value
sampleAeson =
  sample
  where
    sample =
      \ case
        NullSample -> A.Null
        BoolSample a -> A.Bool a
        IntNumberSample a -> A.Number (fromIntegral a)
        DoubleNumberSample a -> realNumber a
        ScientificNumberSample a -> A.Number a
        TextStringSample a -> A.String a
        ArraySample a -> A.Array (fromList (fmap sample a))
        ObjectSample a -> A.Object (fromList (fmap (fmap sample) a))
      where
        realNumber a =
          A.Number $
          if isNaN a || isInfinite a then 0 else (read . show) a
