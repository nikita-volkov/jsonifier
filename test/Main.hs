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

prop_aesonRoundtrip =
  withTests 99999 $
  property $ do
    aeson <- forAll aesonGen
    tripping aeson (JL.value . aesonJL) Aeson.eitherDecodeStrict'

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
