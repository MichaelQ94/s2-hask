{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Geometry.S2.S2LatLngTest (htf_thisModulesTests) where

import Geometry.S2.S1Angle as S1Angle
import Geometry.S2.S2LatLng as S2LatLng
import Test.Framework

prop_toRadians_fromRadians :: Double -> Double -> Bool
prop_toRadians_fromRadians lat lng =
  (toRadians . latitude $ s2LatLng) == lat
    && (toRadians . longitude $ s2LatLng) == lng
  where
    s2LatLng = S2LatLng.fromRadians lat lng

test_basic =
  let ll_rad = S2LatLng.fromRadians (pi / 4) (pi / 2)
      ll_deg = S2LatLng.fromDegrees 45 90
   in do
        assertEqual ll_rad ll_deg
