module Geometry.S2.S2Point
  ( module Data.AdditiveGroup,
    module Data.Cross,
    module Data.VectorSpace,
    S2Point,
    angleBetween,
    ortho,
  )
where

import Data.AdditiveGroup
import Data.Cross
import Data.VectorSpace
import Geometry.S2.S1Angle (S1Angle, fromRadians)

-- |
-- An S2Point represents a point on the unit sphere as a 3D vector.  Usually
-- points are normalized to be unit length, but some methods do not require
-- this. Among other things, there are overloaded operators that make it convenient
-- to write arithmetic expressions (e.g. ((1 - x) *^ p1) ^+^ (x *^ p2)).
type S2Point = (Double, Double, Double)

-- |
-- Return the angle between two points, which is also equal to the distance
-- between these points on the unit sphere.  The points do not need to be
-- normalized.  This function has a maximum error of 3.25 * DBL_EPSILON (or
-- 2.5 * DBL_EPSILON for angles up to 1 radian). If either point is
-- zero-length (e.g. an uninitialized S2Point), or almost zero-length, the
-- resulting angle will be zero.
angleBetween :: S2Point -> S2Point -> S1Angle
angleBetween a b = fromRadians (atan2 (magnitude (a `cross3` b)) (a <.> b))

-- | Returns a unit vector orthogonal to this one.
ortho :: S2Point -> S2Point
ortho p = normalized (p `cross3` setComponent (largestAbsComponent p - 1) 1 (0, 0, 0))

-- Implementation Details

-- |
-- Returns value of 0-indexed component of the tuple. Indices outside the range [0, 3) are wrapped
-- into it via modular arithmetic.
component :: Int -> S2Point -> Double
component 0 (x, _, _) = x
component 1 (_, y, _) = y
component 2 (_, _, z) = z
component i p = component (i `mod` 3) p

-- |
-- Returns a new S2Point whose i-th component has been set to the given value. Indices outside the
-- range [0, 3) are wrapped into it via modular arithmetic.
setComponent :: Int -> Double -> S2Point -> S2Point
setComponent 0 x (_, y, z) = (x, y, z)
setComponent 1 y (x, _, z) = (x, y, z)
setComponent 2 z (x, y, _) = (x, y, z)
setComponent i w p = setComponent (i `mod` 3) w p

-- | Returns the index of the largest component by absolute value.
largestAbsComponent :: S2Point -> Int
largestAbsComponent (x, y, z) = fst (max' (max' (0, abs x) (1, abs y)) (2, abs z))
  where max' (i, a) (j, b) = if a >= b then (i, a) else (j, b)
