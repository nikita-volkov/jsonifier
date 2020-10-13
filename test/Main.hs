module Main where

import Prelude
import Hedgehog
import Hedgehog.Main
import qualified Data.Aeson as Aeson
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
    Aeson.eitherDecodeStrict' (J.toByteString (aesonJson sample)) === Right sample

prop_aesonRoundtrip =
  withTests 99999 $
  property $ do
    aeson <- forAll aesonGen
    encoding <- forAll (J.toByteString <$> jlGen aeson)
    Aeson.eitherDecodeStrict' encoding === Right aeson

aesonGen :: Gen Aeson.Value
aesonGen =
  value
  where
    value =
      Gen.recursive Gen.choice [null, bool, number, string] [array, object]
    null =
      pure Aeson.Null
    bool =
      Aeson.Bool <$> Gen.bool
    number =
      Aeson.Number . realToFrac <$> Gen.double (Range.linearFrac (-9999) 9999)
    string =
      Aeson.String <$> Gen.text (Range.exponential 0 999) Gen.unicode
    array =
      Aeson.Array . fromList <$> Gen.list (Range.exponential 0 99) value
    object =
      Aeson.Object . fromList <$> Gen.list (Range.exponential 0 99) row
      where
        row =
          (,) <$>
            Gen.text (Range.exponential 0 99) Gen.unicode <*>
            value

jlGen :: Aeson.Value -> Gen J.Json
jlGen =
  \ case
    Aeson.Null ->
      return $ J.null
    Aeson.Bool a ->
      return $ J.bool a
    Aeson.Number a ->
      return $ J.scientificNumber a
    Aeson.String a ->
      return $ J.textString a
    Aeson.Array a ->
      a & traverse jlGen
        & fmap J.array
    Aeson.Object a ->
      HashMap.toList a
        & traverse (traverse jlGen)
        & fmap J.object

aesonJson :: Aeson.Value -> J.Json
aesonJson =
  \ case
    Aeson.Null ->
      J.null
    Aeson.Bool a ->
      J.bool a
    Aeson.Number a ->
      J.scientificNumber a
    Aeson.String a ->
      J.textString a
    Aeson.Array a ->
      J.array (fmap aesonJson a)
    Aeson.Object a ->
      J.object (HashMap.foldMapWithKey (\ k -> (: []) . (,) k . aesonJson) a)

load :: FilePath -> IO Aeson.Value
load fileName =
  Aeson.eitherDecodeFileStrict' fileName
    >>= either fail return
