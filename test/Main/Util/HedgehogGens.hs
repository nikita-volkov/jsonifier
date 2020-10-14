module Main.Util.HedgehogGens where

import Prelude
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Numeric.Limits as NumericLimits


scientific :: Gen Scientific
scientific =
  Gen.realFrac_ (Range.linearFrac (-99999999999999) 99999999999999)
    <&> fromRational

realFloat =
  Gen.frequency [
    (99, realRealFloat)
    ,
    (1, nonRealRealFloat)
    ]

realRealFloat =
  Gen.realFloat (Range.exponentialFloat NumericLimits.minValue NumericLimits.maxValue)

nonRealRealFloat =
  Gen.element [0 / 0, 1 / 0, (-1) / 0, -0]
