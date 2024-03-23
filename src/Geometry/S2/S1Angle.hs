module Geometry.S2.S1Angle
  ( S1Angle(..),
    fromRadians,
    fromDegrees,
    toRadians,
    toDegrees,
    normalized,
  )
where

import Geometry.S2.Util (dblRem, tau)

-- |
-- This type represents a one-dimensional angle (as opposed to a
-- two-dimensional solid angle).  It has methods for converting angles to
-- or from radians, degrees, and the E5/E6/E7 representations (i.e. degrees
-- multiplied by 1e5/1e6/1e7 and rounded to the nearest integer).

-- The internal representation is a double-precision value in radians, so
-- conversion to and from radians is exact.  Conversions between E5, E6, E7,
-- and Degrees are not always exact; for example, Degrees(3.1) is different
-- from E6(3100000) or E7(310000000).  However, the following properties are
-- guaranteed for any integer "n", provided that "n" is in the input range of
-- both functions:
--
--     Degrees(n) == E6(1000000 * n)
--     Degrees(n) == E7(10000000 * n)
--          E6(n) == E7(10 * n)

-- The corresponding properties are *not* true for E5, so if you use E5 then
-- don't test for exact equality when comparing to other formats such as
-- Degrees or E7.

-- The following conversions between degrees and radians are exact:
--
--          Degrees(180) == Radians(M_PI)
--       Degrees(45 * k) == Radians(k * M_PI / 4)  for k == 0..8

-- These identities also hold when the arguments are scaled up or down by any
-- power of 2.  Some similar identities are also true, for example,
-- Degrees(60) == Radians(M_PI / 3), but be aware that this type of identity
-- does not hold in general.  For example, Degrees(3) != Radians(M_PI / 60).

-- Similarly, the conversion to radians means that Angle::Degrees(x).degrees()
-- does not always equal "x".  For example,
--
--         S1Angle::Degrees(45 * k).degrees() == 45 * k      for k == 0..8
--   but       S1Angle::Degrees(60).degrees() != 60.

-- This means that when testing for equality, you should allow for numerical
-- errors or convert to discrete E5/E6/E7 values first.
--
-- CAVEAT: All of the above properties depend on "double" being the usual
-- 64-bit IEEE 754 type (which is true on almost all modern platforms).
newtype S1Angle = S1Angle Double deriving (Eq, Ord, Show)

instance Num S1Angle where
  (S1Angle a) + (S1Angle b) = S1Angle (a + b)
  (S1Angle a) * (S1Angle b) = S1Angle (a * b)
  abs (S1Angle rad) = S1Angle (abs rad)
  signum (S1Angle rad) = S1Angle (signum rad)
  fromInteger n = S1Angle (fromInteger n)
  negate (S1Angle rad) = S1Angle (negate rad)

fromRadians :: Double -> S1Angle
fromRadians rad = S1Angle rad

fromDegrees :: Double -> S1Angle
fromDegrees deg = S1Angle ((pi / 180) * deg)

toRadians :: S1Angle -> Double
toRadians (S1Angle rad) = rad

toDegrees :: S1Angle -> Double
toDegrees (S1Angle rad) = (180 / pi) * rad

-- | Return the angle normalized to the range (-180, 180] degrees.
normalized :: S1Angle -> S1Angle
normalized (S1Angle rad) = S1Angle (if rem <= -pi then pi else rem)
  where
    rem = dblRem rad tau
