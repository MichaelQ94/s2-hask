{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Geometry.S2.S2LatLngTest (htf_thisModulesTests) where

import Geometry.S2.S1Angle as S1Angle (fromDegrees, toDegrees, toRadians)
import Geometry.S2.S2LatLng as S2LatLng
  ( S2LatLng,
    fromDegrees,
    fromPoint,
    fromRadians,
    isValid,
    lat,
    lng,
    normalized,
    pointLat,
    pointLng,
    toPoint,
  )
import Geometry.S2.S2Point as S2Point (S2Point, angleBetween, magnitude)
import Test.Framework
  ( Gen,
    Property,
    TestSuite,
    assertEqual,
    forAll,
    makeLoc,
    makeQuickCheckTest,
    makeTestSuite,
    makeUnitTest,
    qcAssertion,
    suchThat,
  )
import Test.QuickCheck.Gen (choose, elements)

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

test_convertToAndFromS2Point = do
  assertEqual
    90
    (toDegrees . lat . S2LatLng.fromPoint . S2LatLng.toPoint $ S2LatLng.fromDegrees 90 65)
  assertEqual
    (- pi / 2)
    (toRadians . lat . S2LatLng.fromPoint . S2LatLng.toPoint $ S2LatLng.fromRadians (- pi / 2) 1)
  assertEqual
    180
    (toDegrees . lng . S2LatLng.fromPoint . S2LatLng.toPoint $ S2LatLng.fromDegrees 12.2 180)
  assertEqual
    pi
    (toRadians . lng . S2LatLng.fromPoint . S2LatLng.toPoint $ S2LatLng.fromRadians 0.1 (- pi))

prop_convertFromAndToS2Point :: Property
prop_convertFromAndToS2Point =
  forAll normalizedS2Points $ \p -> approxEqual p (S2LatLng.toPoint . S2LatLng.fromPoint $ p)

test_negativeZeros = do
  assertIdentical 0 (toRadians . pointLat $ (1, 0, -0.0))
  assertIdentical 0 (toRadians . pointLng $ (1, -0.0, 0))
  assertIdentical pi (toRadians . pointLng $ (-1, -0.0, 0))
  assertIdentical 0 (toRadians . pointLng $ (-0.0, 0, 1))
  assertIdentical 0 (toRadians . pointLng $ (-0.0, -0.0, 1))

assertIdentical :: Double -> Double -> IO ()
assertIdentical x y = do
  assertEqual x y
  assertEqual (isNegativeZero x) (isNegativeZero y)

assertValid :: S2LatLng -> IO ()
assertValid latLng = assertEqual True (S2LatLng.isValid latLng)

assertInvalid :: S2LatLng -> IO ()
assertInvalid latLng = assertEqual False (S2LatLng.isValid latLng)

approxEqual :: S2Point -> S2Point -> Bool
approxEqual a b = toRadians (S2Point.angleBetween a b) < 1E-15

normalizedS2Points :: Gen S2Point
normalizedS2Points = do
  z <- choose (-1, 1)
  let maxY = sqrt (1 - (z * z))
  y <- choose (- maxY, maxY)
  let absX = sqrt (1 - (z * z) - (y * y))
  x <- elements [-absX, absX]
  return (x, y, z)
