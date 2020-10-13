module Main where

import Prelude
import Hedgehog
import Hedgehog.Main
import qualified Data.Aeson as Aeson
import qualified JsonLego as JL
import qualified Data.HashMap.Strict as HashMap
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


main =
  defaultMain $ pure $ checkParallel $ $$(discover)

prop_sample =
  withTests 1 $
  property $ do
    sample <- liftIO $ load "samples/twitter100.json"
    Aeson.eitherDecodeStrict' (JL.toByteString (aesonJL sample)) === Right sample

prop_aesonRoundtrip =
  withTests 99999 $
  property $ do
    aeson <- forAll aesonGen
    encoding <- forAll (JL.toByteString <$> jlGen aeson)
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

jlGen :: Aeson.Value -> Gen JL.Json
jlGen =
  \ case
    Aeson.Null ->
      return $ JL.null
    Aeson.Bool a ->
      return $ JL.bool a
    Aeson.Number a ->
      return $ JL.scientificNumber a
    Aeson.String a ->
      return $ JL.string a
    Aeson.Array a ->
      a & traverse jlGen
        & fmap JL.array
    Aeson.Object a ->
      HashMap.toList a
        & traverse (traverse jlGen)
        & fmap JL.object

aesonJL :: Aeson.Value -> JL.Json
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
      JL.array (fmap aesonJL a)
    Aeson.Object a ->
      JL.object (HashMap.foldMapWithKey (\ k -> (: []) . (,) k . aesonJL) a)

load :: FilePath -> IO Aeson.Value
load fileName =
  Aeson.eitherDecodeFileStrict' fileName
    >>= either fail return
