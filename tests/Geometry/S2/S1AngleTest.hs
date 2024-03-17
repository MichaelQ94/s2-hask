{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Geometry.S2.S1AngleTest (htf_thisModulesTests) where

import Geometry.S2.S1Angle as S1Angle
import Test.Framework

prop_toRadians_fromRadians :: Double -> Bool
prop_toRadians_fromRadians rad = (toRadians . fromRadians $ rad) == rad

test_degreesVsRadians = expectedDegreesVsRadians (-8) 8

test_normalized_correctlyCanonicalizesAngles = do
  assertEqual (fromDegrees 0) (normalized . fromDegrees $ 360)
  assertEqual (fromDegrees (-90)) (normalized . fromDegrees $ (-90))
  assertEqual (fromDegrees 180) (normalized . fromDegrees $ (-180))
  assertEqual (fromDegrees 180) (normalized . fromDegrees $ 180)
  assertEqual (fromDegrees 180) (normalized . fromDegrees $ 540)
  assertEqual (fromDegrees 90) (normalized . fromDegrees $ (-270))

expectedDegreesVsRadians :: Int -> Int -> IO ()
expectedDegreesVsRadians k n
  | k > n = return ()
  | otherwise = do
      assertEqual (fromDegrees deg) (fromRadians ((fromIntegral k) * pi / 4))
      assertEqual deg (toDegrees . fromDegrees $ deg)
      expectedDegreesVsRadians (k + 1) n
  where
    deg = fromIntegral (45 * k)
