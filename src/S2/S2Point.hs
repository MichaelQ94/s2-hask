module S2.S2Point (
  module Data.AdditiveGroup,
  module Data.Cross,
  module Data.VectorSpace,
  S2Point
) where

import Data.AdditiveGroup
import Data.Cross
import Data.VectorSpace

-- An S2Point represents a point on the unit sphere as a 3D vector.  Usually
-- points are normalized to be unit length, but some methods do not require
-- this. Among other things, there are overloaded operators that make it convenient
-- to write arithmetic expressions (e.g. (1-x)*p1 + x*p2).
type S2Point = (Double, Double, Double)
