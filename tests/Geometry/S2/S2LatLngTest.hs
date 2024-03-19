{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Geometry.S2.S2LatLngTest (htf_thisModulesTests) where

import Geometry.S2.S1Angle as S1Angle
import Geometry.S2.S2LatLng as S2LatLng
import Test.Framework

prop_toRadians_fromRadians :: Double -> Double -> Bool
prop_toRadians_fromRadians latRad lngRad =
  (toRadians . lat $ s2LatLng) == latRad
    && (toRadians . lng $ s2LatLng) == lngRad
  where
    s2LatLng = S2LatLng.fromRadians latRad lngRad

test_basic =
  let ll_rad = S2LatLng.fromRadians (pi / 4) (pi / 2)
      ll_deg = S2LatLng.fromDegrees 45 90
      bad1 = S2LatLng.fromDegrees 120 200
      better1 = S2LatLng.normalized bad1
      bad2 = S2LatLng.fromDegrees (-100) (-360)
      better2 = S2LatLng.normalized bad2
   in do
        assertEqual ll_rad ll_deg
        assertValid ll_deg
        assertInvalid (S2LatLng.fromDegrees (-91) 0)
        assertInvalid (S2LatLng.fromDegrees 0 181)

        assertInvalid bad1
        assertValid better1
        assertEqual (S1Angle.fromDegrees 90) (lat better1)
        assertEqual
          (S1Angle.toRadians . S1Angle.fromDegrees $ -160)
          (S1Angle.toRadians . lng $ better1)

        assertInvalid bad2
        assertValid better2
        assertEqual (S1Angle.fromDegrees (-90)) (lat better2)
        assertEqual 0 (S1Angle.toRadians . lng $ better2)


assertValid :: S2LatLng -> IO ()
assertValid latLng = assertEqual True (S2LatLng.isValid latLng)

assertInvalid :: S2LatLng -> IO ()
assertInvalid latLng = assertEqual False (S2LatLng.isValid latLng)
