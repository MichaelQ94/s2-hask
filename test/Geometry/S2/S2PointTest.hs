{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Geometry.S2.S2PointTest (htf_thisModulesTests) where

import Geometry.S2.S1Angle (toRadians)
import Geometry.S2.S2Generators ( normalizedS2Points )
import Geometry.S2.S2Point as S2Point
    ( AdditiveGroup((^+^)),
      HasCross3(cross3),
      InnerSpace((<.>)),
      VectorSpace((*^)),
      S2Point,
      angleBetween,
      ortho )
import Test.Framework
    ( assertEqual,
      forAll,
      makeLoc,
      qcAssertion,
      makeQuickCheckTest,
      makeTestSuite,
      makeUnitTest,
      Property,
      TestSuite )

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

-- Tests of functions defined in S2Point.hs
test_angleBetween :: IO ()
test_angleBetween = do
  assertEqual (pi / 2) (toRadians $ angleBetween (1, 0, 0) (0, 0, 2))
  assertEqual 0 (toRadians $ angleBetween (1, 0, 0) (1, 0, 0))

prop_ortho_orthogonalWrtInnerProduct :: Property
prop_ortho_orthogonalWrtInnerProduct =
  forAll normalizedS2Points $ \p -> p <.> ortho p < 1E-15
