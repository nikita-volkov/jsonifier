module Jsonifier.Time where

-- from Data.Attoparsec.Time.Internal...
import GHC.Num
import Data.Fixed ( Fixed(MkFixed), Pico )
import Data.Int ( Int64 )
import Data.Time
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder.Prim as BP

import Data.Ord
import Prelude
import Data.Char

data TimeOfDay64 = TOD {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int64

data T = T {-# UNPACK #-} !Char {-# UNPACK #-} !Char

toPico :: Integer -> Pico
toPico = MkFixed
{-# INLINE toPico #-}

fromPico :: Pico -> Integer
fromPico (MkFixed i) = i
{-# INLINE fromPico #-}
posixDayLength :: DiffTime
posixDayLength = 86400

diffTimeOfDay64 :: DiffTime -> TimeOfDay64
diffTimeOfDay64 t
  | t >= posixDayLength = TOD 23 59 (60000000000000 + pico (t - posixDayLength))
  | otherwise = TOD (fromIntegral h) (fromIntegral m) s
    where (h,mp) = pico t `quotRem` 3600000000000000
          (m,s)  = mp `quotRem` 60000000000000
          pico   = fromIntegral . diffTimeToPicoseconds


toTimeOfDay64 :: TimeOfDay -> TimeOfDay64
toTimeOfDay64 (TimeOfDay h m s) = TOD h m (fromIntegral (fromPico s))

day :: Day -> B.Builder
day dd = encodeYear yr <>
         BP.primBounded (ascii6 ('-',(mh,(ml,('-',(dh,dl)))))) ()
  where (yr,m,d)    = toGregorian dd
        !(T mh ml)  = twoDigits m
        !(T dh dl)  = twoDigits d
        encodeYear y
            | y >= 1000 = B.integerDec y
            | y >= 0    = BP.primBounded (ascii4 (padYear y)) ()
            | y >= -999 = BP.primBounded (ascii5 ('-',padYear (- y))) ()
            | otherwise = B.integerDec y
        padYear y =
            let (ab,c) = fromIntegral y `quotRem` 10
                (a,b)  = ab `quotRem` 10
            in ('0',(digit a,(digit b,digit c)))
{-# INLINE day #-}

dayTime :: Day -> TimeOfDay64 -> B.Builder
dayTime d t = day d <> B.char7 'T' <> timeOfDay64 t
{-# INLINE dayTime #-}

utcTime :: UTCTime -> B.Builder
utcTime (UTCTime d s) = dayTime d (diffTimeOfDay64 s) <> B.char7 'Z'
{-# INLINE utcTime #-}

localTime :: LocalTime -> B.Builder
localTime (LocalTime d t) = dayTime d (toTimeOfDay64 t)
{-# INLINE localTime #-}

zonedTime :: ZonedTime -> B.Builder
zonedTime (ZonedTime t z) = localTime t <> timeZone z
{-# INLINE zonedTime #-}

twoDigits :: Int -> T
twoDigits a     = T (digit hi) (digit lo)
  where (hi,lo) = a `quotRem` 10
{-# INLINE twoDigits #-}

digit :: Int -> Char
digit x = chr (x + 48)
{-# INLINE  digit #-}

ascii2 cs = BP.liftFixedToBounded $ const cs BP.>$< BP.char7 >*< BP.char7
{-# INLINE ascii2 #-}

ascii4 :: (Char, (Char, (Char, Char))) -> BP.BoundedPrim a
ascii4 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii4 #-}

ascii5 :: (Char, (Char, (Char, (Char, Char)))) -> BP.BoundedPrim a
ascii5 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii5 #-}

ascii6 :: (Char, (Char, (Char, (Char, (Char, Char))))) -> BP.BoundedPrim a
ascii6 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii6 #-}

ascii8 :: (Char, (Char, (Char, (Char, (Char, (Char, (Char, Char)))))))
       -> BP.BoundedPrim a
ascii8 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii8 #-}

timeOfDay :: TimeOfDay -> B.Builder
timeOfDay t = timeOfDay64 (toTimeOfDay64 t)
{-# INLINE timeOfDay #-}

timeOfDay64 :: TimeOfDay64 -> B.Builder
timeOfDay64 (TOD h m s)
  | frac == 0 = hhmmss -- omit subseconds if 0
  | otherwise = hhmmss <> BP.primBounded showFrac frac
  where
    hhmmss  = BP.primBounded (ascii8 (hh,(hl,(':',(mh,(ml,(':',(sh,sl)))))))) ()
    !(T hh hl)  = twoDigits h
    !(T mh ml)  = twoDigits m
    !(T sh sl)  = twoDigits (fromIntegral real)
    (real,frac) = s `quotRem` pico
    showFrac = ('.',) >$< (BP.liftFixedToBounded BP.char7 >*< trunc12)
    trunc12 = (`quotRem` micro) >$<
              BP.condB (\(_,y) -> y == 0) (fst >$< trunc6) (digits6 >*< trunc6)
    digits6 = ((`quotRem` milli) . fromIntegral) >$< (digits3 >*< digits3)
    trunc6  = ((`quotRem` milli) . fromIntegral) >$<
              BP.condB (\(_,y) -> y == 0) (fst >$< trunc3) (digits3 >*< trunc3)
    digits3 = (`quotRem` 10) >$< (digits2 >*< digits1)
    digits2 = (`quotRem` 10) >$< (digits1 >*< digits1)
    digits1 = BP.liftFixedToBounded (digit >$< BP.char7)
    trunc3  = BP.condB (== 0) BP.emptyB $
              (`quotRem` 100) >$< (digits1 >*< trunc2)
    trunc2  = BP.condB (== 0) BP.emptyB $
              (`quotRem` 10)  >$< (digits1 >*< trunc1)
    trunc1  = BP.condB (== 0) BP.emptyB digits1

    pico       = 1000000000000 -- number of picoseconds  in 1 second
    micro      =       1000000 -- number of microseconds in 1 second
    milli      =          1000 -- number of milliseconds in 1 second


timeZone :: TimeZone -> B.Builder
timeZone (TimeZone off _ _)
  | off == 0  = B.char7 'Z'
  | otherwise = BP.primBounded (ascii6 (s,(hh,(hl,(':',(mh,ml)))))) ()
  where !s         = if off < 0 then '-' else '+'
        !(T hh hl) = twoDigits h
        !(T mh ml) = twoDigits m
        (h,m)      = abs off `quotRem` 60
{-# INLINE timeZone #-}