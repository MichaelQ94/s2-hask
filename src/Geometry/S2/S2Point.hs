module Geometry.S2.S2Point
  ( module Data.AdditiveGroup,
    module Data.Cross,
    module Data.VectorSpace,
    S2Point,
    angleBetween,
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
-- to write arithmetic expressions (e.g. (1-x)*p1 + x*p2).
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
