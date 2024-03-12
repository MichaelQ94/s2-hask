{-# OPTIONS_GHC -F -pgmF htfpp #-}

module S1AngleTest (htf_thisModulesTests) where

import S2.S1Angle as S1Angle
import Test.Framework

prop_toRadians_fromRadians :: Double -> Bool
prop_toRadians_fromRadians rad = (toRadians . fromRadians $ rad) == rad

test_degreesVsRadians = expectedDegreesVsRadians (-8) 8

expectedDegreesVsRadians :: Int -> Int -> IO ()
expectedDegreesVsRadians k n
  | k > n = return ()
  | otherwise = do
      assertEqual (fromDegrees deg) (fromRadians ((fromIntegral k) * pi / 4))
      assertEqual deg (toDegrees . fromDegrees $ deg)
      expectedDegreesVsRadians (k + 1) n
  where
    deg = fromIntegral (45 * k)