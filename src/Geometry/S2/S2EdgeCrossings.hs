module Geometry.S2.S2EdgeCrossings where

import Data.List (maximumBy)
import Geometry.S2.S2Point
import Geometry.S2.S2Predicates (orderedCCW, triageSign)
import Numeric.Limits (epsilon)

-- |
-- This function determines whether the edge AB intersects the edge CD.
-- Returns +1 if AB crosses CD at a point that is interior to both edges.
-- Returns  0 if any two vertices from different edges are the same.
-- Returns -1 otherwise.
--
-- Note that if an edge is degenerate (A == B or C == D), the return value
-- is 0 if two vertices from different edges are the same and -1 otherwise.
--
-- Properties of CrossingSign:
--
--  (1) CrossingSign(b,a,c,d) == CrossingSign(a,b,c,d)
--  (2) CrossingSign(c,d,a,b) == CrossingSign(a,b,c,d)
--  (3) CrossingSign(a,b,c,d) == 0 if a==c, a==d, b==c, b==d
--  (3) CrossingSign(a,b,c,d) <= 0 if a==b or c==d (see above)
crossingSign :: S2Point -> S2Point -> S2Point -> S2Point -> Int
crossingSign a b c d
  -- Eliminate cases where two vetrices from different edges are equal
  | a == c || a == d || b == c || b == d = 0
  -- Eliminate cases where an input edge is degenerate
  | a == b || c == d = -1
  -- For there to be an edge crossing, the triangles ACB, CBD, BDA, DAC must
  -- all be oriented the same way (CW or CCW).
  | acb /= 0 && acb == cbd && cbd == bda && bda == dac && dac == acb = 1
  | otherwise = -1
  where
    acb = triageSign a c b
    cbd = triageSign c b d
    bda = triageSign b d a
    dac = triageSign d a c

-- A convenience function that calls CrossingSign() to handle cases
-- where all four vertices are distinct, and VertexCrossing() to handle
-- cases where two or more vertices are the same.  This defines a crossing
-- function such that point-in-polygon containment tests can be implemented
-- by simply counting edge crossings.
edgeOrVertexCrossing :: S2Point -> S2Point -> S2Point -> S2Point -> Bool
edgeOrVertexCrossing a b c d
  | crossing < 0 = False
  | crossing > 0 = True
  | otherwise = vertexCrossing a b c d
  where
    crossing = crossingSign a b c d

-- |
-- Given two edges AB and CD where at least two vertices are identical
-- (i.e. CrossingSign(a,b,c,d) == 0), this function defines whether the
-- two edges "cross" in such a way that point-in-polygon containment tests can
-- be implemented by counting the number of edge crossings.  The basic rule is
-- that a "crossing" occurs if AB is encountered after CD during a CCW sweep
-- around the shared vertex starting from a fixed reference point.
--
-- Note that according to this rule, if AB crosses CD then in general CD
-- does not cross AB.  However, this leads to the correct result when
-- counting polygon edge crossings.  For example, suppose that A,B,C are
-- three consecutive vertices of a CCW polygon.  If we now consider the edge
-- crossings of a segment BP as P sweeps around B, the crossing number
-- changes parity exactly when BP crosses BA or BC.
--
-- Useful properties of VertexCrossing (VC):
--
--  (1) VC(a,a,c,d) == VC(a,b,c,c) == false
--  (2) VC(a,b,a,b) == VC(a,b,b,a) == true
--  (3) VC(a,b,c,d) == VC(a,b,d,c) == VC(b,a,c,d) == VC(b,a,d,c)
--  (3) If exactly one of a,b equals one of c,d, then exactly one of
--      VC(a,b,c,d) and VC(c,d,a,b) is true
--
-- It is an error to call this method with 4 distinct vertices.
vertexCrossing :: S2Point -> S2Point -> S2Point -> S2Point -> Bool
vertexCrossing a b c d
  -- If A == B or C == D there is no intersection.  We need to check this
  -- case first in case 3 or more input points are identical.
  | a == b || c == d = False
  -- If any other pair of vertices is equal, there is a crossing if and only
  -- if OrderedCCW() indicates that the edge AB is further CCW around the
  -- shared vertex O (either A or B) than the edge CD, starting from an
  -- arbitrary fixed reference point.
  --
  -- Optimization: if AB=CD or AB=DC, we can avoid most of the calculations.
  | a == b = b == d || orderedCCW (ortho a) d b a
  | b == d = orderedCCW (ortho b) c a b
  | a == d = b == c || orderedCCW (ortho a) c b a
  | b == c = orderedCCW (ortho b) d a b
  -- Should not be reached.
  | otherwise = False

-- |
-- Returns a unit-length vector that is orthogonal to "a".  Satisfies
-- Ortho(-a) = -Ortho(a) for all a.
ortho :: S2Point -> S2Point
ortho a =
  let k = largestAbsComponent a
      temp =
        ( if k == 1 then 1 else 0.012,
          if k == 2 then 1 else 0.0053,
          if k == 0 then 1 else 0.00457
        )
   in normalized (a `cross3` temp)

largestAbsComponent :: S2Point -> Int
largestAbsComponent (x, y, z) =
  fst (maximumBy (\a b -> compare (abs . snd $ a) (abs . snd $ b)) [(0, x), (1, y), (2, z)])
