module Geometry.S2.S2Predicates
  ( triageSign,
  )
where

import Geometry.S2.S2Point
import Numeric.Limits (epsilon)

-- |
-- Returns +1 if the points are definitely CCW, -1 if they are definitely CW, and 0 if two points
-- are identical or the result is uncertain.
triageSign :: S2Point -> S2Point -> S2Point -> Int
triageSign a b c
  | det > maxDetError = 1
  | det < (- maxDetError) = -1
  | otherwise = 0
  where
    det = (a `cross3` b) <.> c
    -- kMaxDetError is the maximum error in computing (AxB).C where all vectors
    -- are unit length.  Using standard inequalities, it can be shown that
    --
    --  fl(AxB) = AxB + D where |D| <= (|AxB| + (2/sqrt(3))*|A|*|B|) * e
    --
    -- where "fl()" denotes a calculation done in floating-point arithmetic,
    -- |x| denotes either absolute value or the L2-norm as appropriate, and
    -- e = 0.5*DBL_EPSILON.  Similarly,
    --
    --  fl(B.C) = B.C + d where |d| <= (1.5*|B.C| + 1.5*|B|*|C|) * e .
    --
    -- Applying these bounds to the unit-length vectors A,B,C and neglecting
    -- relative error (which does not affect the sign of the result), we get
    --
    --  fl((AxB).C) = (AxB).C + d where |d| <= (2.5 + 2/sqrt(3)) * e
    --
    -- which is about 3.6548 * e, or 1.8274 * DBL_EPSILON.
    --
    -- In order to support vectors of magnitude <= sqrt(2), we double this value.
    maxDetError :: Double
    maxDetError = 3.6548 * (epsilon :: Double)
