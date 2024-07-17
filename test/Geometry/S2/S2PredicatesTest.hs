{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Geometry.S2.S2PredicatesTest (htf_thisModulesTests) where

import Geometry.S2.S2Point
import Geometry.S2.S2Predicates
import Test.Framework
    ( assertEqual, makeLoc, makeTestSuite, makeUnitTest, TestSuite )

test_triageSign_simple =
  let x = (1, 0, 0)
      y = (0, 1, 0)
      z = (0, 0, 1)
   in do
        assertEqual 1 (triageSign x y z)
        assertEqual (- 1) (triageSign z y x)
        assertEqual 0 (triageSign x y y)
        assertEqual 0 (triageSign z z z)
        assertEqual 0 (triageSign x y (normalized (x + y)))

test_orderedCCW_simple =
  let x = (1, 0, 0)
      y = (0, 1, 0)
      z = (0, 0, 1)
      d = normalized (x + y + z)
   in do
        assertEqual True (orderedCCW x y z d)
        assertEqual False (orderedCCW z y x d)
        assertEqual True (orderedCCW x x y d)
        assertEqual True (orderedCCW x y y d)
        assertEqual True (orderedCCW z z z d)
        assertEqual True (orderedCCW z z z z)
        assertEqual False (orderedCCW x y x z)
