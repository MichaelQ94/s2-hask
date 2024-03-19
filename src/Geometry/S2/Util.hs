module Geometry.S2.Util(dblRem) where

-- | Returns the number @r@ such that @r = a - qb@ and
-- | where q is the result of @(b/a)@, rounded to the nearest
-- | integer. In the case of a tie, q is chosen to be even.
dblRem :: Double -> Double -> Double
dblRem a b = a - (b * fromIntegral (round (a / b)))