module Geometry.S2.Util
  ( dblRem,
    halfPi,
    tau,
  )
where

-- | Returns the number @r@ such that @r = a - qb@ and
-- | where q is the result of @(b/a)@, rounded to the nearest
-- | integer. In the case of a tie, q is chosen to be even.
dblRem :: Double -> Double -> Double
dblRem a b = a - (b * fromIntegral (round (a / b)))

halfPi = pi / 2

tau = 2 * pi
