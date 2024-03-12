{-# OPTIONS_GHC -F -pgmF htfpp #-}

module S2PointTest (htf_thisModulesTests) where

import S2.S2Point as S2Point
import Test.Framework

-- Already tested by the vector-space library, but gives a demo of
-- the vector operations imported from it.
prop_vec_addition :: S2Point -> S2Point -> Bool
prop_vec_addition p1@(x1, y1, z1) p2@(x2, y2, z2) =
  p1 ^+^ p2 == (x1 + x2, y1 + y2, z1 + z2)

prop_vec_scale :: Double -> S2Point -> Bool
prop_vec_scale s p@(x, y, z) = s *^ p == (s * x, s * y, s * z)

prop_vec_dot :: S2Point -> S2Point -> Bool
prop_vec_dot p1@(x1, y1, z1) p2@(x2, y2, z2) =
  p1 <.> p2 == (x1 * x2) + (y1 * y2) + (z1 * z2)

prop_vec_cross :: S2Point -> S2Point -> Bool
prop_vec_cross p1@(x1, y1, z1) p2@(x2, y2, z2) =
  p1 `cross3` p2
    == ( (y1 * z2) - (y2 * z1),
         (z1 * x2) - (z2 * x1),
         (x1 * y2) - (x2 * y1)
       )
