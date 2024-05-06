{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main (main) where

import Test.Framework
import {-@ HTF_TESTS @-} Geometry.S2.S1AngleTest
import {-@ HTF_TESTS @-} Geometry.S2.S2CellIdTest
import {-@ HTF_TESTS @-} Geometry.S2.S2LatLngTest
import {-@ HTF_TESTS @-} Geometry.S2.S2LoopTest
import {-@ HTF_TESTS @-} Geometry.S2.S2PointTest

main :: IO ()
main = htfMain htf_importedTests
