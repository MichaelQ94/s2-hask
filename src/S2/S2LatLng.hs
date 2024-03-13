module S2.S2LatLng
  ( S2LatLng,
    S2.S2LatLng.fromRadians,
    S2.S2LatLng.fromDegrees,
    latitude,
    longitude,
  )
where

import S2.S1Angle as S1Angle

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

latitude :: S2LatLng -> S1Angle
latitude (S2LatLng lat _) = lat

longitude :: S2LatLng -> S1Angle
longitude (S2LatLng _ lng) = lng
