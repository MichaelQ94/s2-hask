module Geometry.S2.S2Predicates
  ( orderedCCW,
    triageSign,
  )
where

import Geometry.S2.S2Point
import Numeric.Limits (epsilon)

-- |
-- Given 4 points on the unit sphere, return true if the edges OA, OB, and
-- OC are encountered in that order while sweeping CCW around the point O.
-- You can think of this as testing whether A <= B <= C with respect to the
-- CCW ordering around O that starts at A, or equivalently, whether B is
-- contained in the range of angles (inclusive) that starts at A and extends
-- CCW to C.  Properties:
--
--  (1) If OrderedCCW(a,b,c,o) && OrderedCCW(b,a,c,o), then a == b
--  (2) If OrderedCCW(a,b,c,o) && OrderedCCW(a,c,b,o), then b == c
--  (3) If OrderedCCW(a,b,c,o) && OrderedCCW(c,b,a,o), then a == b == c
--  (4) If a == b or b == c, then OrderedCCW(a,b,c,o) is true
--  (5) Otherwise if a == c, then OrderedCCW(a,b,c,o) is false
--
-- REQUIRES: a /= o && b /= o && c /= o
orderedCCW :: S2Point -> S2Point -> S2Point -> S2Point -> Bool
orderedCCW a b c o =
  -- The last inequality below is ">" rather than ">=" so that we return true
  -- if A == B or B == C, and otherwise false if A == C.  Recall that
  -- Sign(x,y,z) == -Sign(z,y,x) for all x,y,z.
  sum (map fromEnum [triageSign b o a >= 0, triageSign c o b >= 0, triageSign a o c > 0]) >= 2

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
