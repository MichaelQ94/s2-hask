module Geometry.S2.S2Generators where

import Geometry.S2.S2Point as S2Point (S2Point, normalized)
import Test.Framework (Gen)
import Test.QuickCheck.Gen (choose, elements)

normalizedS2Points :: Gen S2Point
normalizedS2Points = do
  x2 <- choose (0, 1)
  y2 <- choose (0, 1 - x2)
  let absX = sqrt x2
  let absY = sqrt y2
  let absZ = sqrt (1 - x2 - y2)
  x <- elements [- absX, absX]
  y <- elements [- absY, absY]
  z <- elements [- absZ, absZ]
  -- Balance the asymmetry of floating-point operations by randomly choosing one of the 6 valid
  -- permutations of the coordinates.
  elements [(x, y, z), (y, z, x), (z, x, y), (z, y, x), (y, x, z), (x, z, y)]
