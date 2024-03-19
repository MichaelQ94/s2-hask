module Geometry.S2.S2LatLng
  ( S2LatLng,
    Geometry.S2.S2LatLng.fromRadians,
    Geometry.S2.S2LatLng.fromDegrees,
    lat,
    lng,
    isValid,
    Geometry.S2.S2LatLng.normalized,
  )
where

import Geometry.S2.S1Angle (S1Angle)
import Geometry.S2.S1Angle as S1Angle
import Geometry.S2.Util (dblRem, halfPi, tau)

-- This class represents a point on the unit sphere as a pair
-- of latitude-longitude coordinates.  Like the rest of the "geometry"
-- package, the intent is to represent spherical geometry as a mathematical
-- abstraction, so functions that are specifically related to the Earth's
-- geometry (e.g. easting/northing conversions) should be put elsewhere.
data S2LatLng = S2LatLng S1Angle S1Angle deriving (Eq, Show)

fromRadians :: Double -> Double -> S2LatLng
fromRadians lat lng = S2LatLng (S1Angle.fromRadians lat) (S1Angle.fromRadians lng)

fromDegrees :: Double -> Double -> S2LatLng
fromDegrees lat lng = S2LatLng (S1Angle.fromDegrees lat) (S1Angle.fromDegrees lng)

lat :: S2LatLng -> S1Angle
lat (S2LatLng latitude _) = latitude

lng :: S2LatLng -> S1Angle
lng (S2LatLng _ longitude) = longitude

-- Return true if the latitude is between -90 and 90 degrees inclusive
-- and the longitude is between -180 and 180 degrees inclusive.
isValid :: S2LatLng -> Bool
isValid (S2LatLng (S1Angle latRad) (S1Angle lngRad)) =
  -halfPi <= latRad && latRad <= halfPi && -pi <= lngRad && lngRad <= halfPi

-- Clamps the latitude to the range [-90, 90] degrees, and adds or subtracts
-- a multiple of 360 degrees to the longitude if necessary to reduce it to
-- the range [-180, 180].
normalized :: S2LatLng -> S2LatLng
normalized (S2LatLng (S1Angle latRad) (S1Angle lngRad)) =
  S2LatLng (S1Angle (max (-halfPi) . min halfPi $ latRad)) (S1Angle (dblRem lngRad tau))
