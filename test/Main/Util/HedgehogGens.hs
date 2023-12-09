module Main.Util.HedgehogGens where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Numeric.Limits as NumericLimits
import Prelude

scientific :: Gen Scientific
scientific =
  Gen.realFrac_ (Range.linearFrac (-99999999999999) 99999999999999)
    <&> fromRational

realFloat :: (MonadGen m, RealFloat a) => m a
realFloat =
  Gen.frequency
    [ (99, realRealFloat),
      (1, nonRealRealFloat)
    ]

realRealFloat :: (MonadGen m, RealFloat a) => m a
realRealFloat =
  Gen.realFloat (Range.exponentialFloat NumericLimits.minValue NumericLimits.maxValue)

nonRealRealFloat :: (MonadGen m, Fractional a) => m a
nonRealRealFloat =
  Gen.element [0 / 0, 1 / 0, (-1) / 0, -0]
