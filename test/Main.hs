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
import qualified Data.Vector as Vector
import qualified Main.Util.HedgehogGens as GenExtras


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
  withTests 9999 $
  property $ do
    sample <- forAll sampleGen
    let encoding = sampleJsonifier sample
    annotate (Char8ByteString.unpack encoding)
    aeson <- evalEither (A.eitherDecodeStrict' encoding)
    evalEither (maybe (Right ()) Left (detectMismatchInSampleAndAeson sample aeson))

data Sample =
  NullSample |
  BoolSample Bool |
  IntNumberSample Int |
  WordNumberSample Word |
  DoubleNumberSample Double |
  ScientificNumberSample Scientific |
  TextStringSample Text |
  ScientificStringSample Scientific |
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
        wordNumber,
        doubleNumber,
        scientificNumber,
        textString,
        scientificString
        ] [
        array, object
        ]
    null =
      pure NullSample
    bool =
      Gen.bool <&> BoolSample
    intNumber =
      Gen.int Range.exponentialBounded <&> IntNumberSample
    wordNumber =
      Gen.word Range.exponentialBounded <&> WordNumberSample
    doubleNumber =
      GenExtras.realFloat <&> DoubleNumberSample
    scientificNumber =
      GenExtras.scientific <&> ScientificNumberSample
    textString =
      Gen.text (Range.exponential 0 9999) Gen.unicode <&> TextStringSample
    scientificString =
      GenExtras.scientific <&> ScientificStringSample
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
        WordNumberSample a -> J.wordNumber a
        DoubleNumberSample a -> J.doubleNumber a
        ScientificNumberSample a -> J.scientificNumber a
        TextStringSample a -> J.textString a
        ScientificStringSample a -> J.scientificString a
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
        WordNumberSample a -> A.Number (fromIntegral a)
        DoubleNumberSample a -> realNumber a
        ScientificNumberSample a -> A.Number a
        ScientificStringSample a -> A.String (fromString (show a))
        TextStringSample a -> A.String a
        ArraySample a -> A.Array (Vector.fromList (fmap sample a))
        ObjectSample a -> A.Object (HashMap.fromList (fmap (fmap sample) a))
      where
        realNumber a =
          A.Number $
          if isNaN a || isInfinite a then 0 else (read . show) a

{-|
We have to come down to this trickery due to small differences in
the way scientific renders floating point values and how ptr-poker does.
There is no difference when floating point comparison is used instead of scientific,
so this is what we achieve here.

It must be mentioned that this only applies to floating point numbers.
-}
detectMismatchInSampleAndAeson :: Sample -> A.Value -> Maybe (Sample, A.Value)
detectMismatchInSampleAndAeson =
  \ case
    NullSample ->
      \ case
        A.Null -> Nothing
        a -> Just (NullSample, a)
    BoolSample a ->
      \ case
        A.Bool b | b == a -> Nothing
        b -> Just (BoolSample a, b)
    IntNumberSample a ->
      \ case
        A.Number b | round b == a -> Nothing
        b -> Just (IntNumberSample a, b)
    WordNumberSample a ->
      \ case
        A.Number b | round b == a -> Nothing
        b -> Just (WordNumberSample a, b)
    DoubleNumberSample a ->
      \ case
        -- This is what it's all for.
        A.Number b | realToFrac b == if isNaN a || isInfinite a then 0 else a -> Nothing
        b -> Just (DoubleNumberSample a, b)
    ScientificNumberSample a ->
      \ case
        A.Number b | a == b -> Nothing
        b -> Just (ScientificNumberSample a, b)
    TextStringSample a ->
      \ case
        A.String b | a == b -> Nothing
        b -> Just (TextStringSample a, b)
    ScientificStringSample a ->
      \ case
        A.String b | (fromString . show) a == b -> Nothing
        b -> Just (ScientificStringSample a, b)
    ArraySample a ->
      \ case
        A.Array b | Vector.length b == length a ->
          toList b
            & zip a
            & foldMap (\ (aa, bb) -> fmap First (detectMismatchInSampleAndAeson aa bb))
            & fmap getFirst
        b -> Just (ArraySample a, b)
    ObjectSample a ->
      \ case
        A.Object b ->
          a & foldMap (\ (ak, av) ->
                case HashMap.lookup ak b of
                  Just bv -> fmap First (detectMismatchInSampleAndAeson av bv)
                  Nothing -> Just (First (ObjectSample a, A.Object b))
                )
            & fmap getFirst
        b -> Just (ObjectSample a, b)
