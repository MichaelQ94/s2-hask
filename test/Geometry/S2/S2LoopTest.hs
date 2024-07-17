{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Geometry.S2.S2LoopTest (htf_thisModulesTests) where

import Geometry.S2.S2Loop (empty, fromVertices, full, numVertices, vertex)
import Test.Framework
    ( assertEqual, makeLoc, makeTestSuite, makeUnitTest, TestSuite )

test_emptyLoopIsSingleton :: IO ()
test_emptyLoopIsSingleton = assertEqual (numVertices empty) 1

test_fullLoopIsSingleton :: IO ()
test_fullLoopIsSingleton = assertEqual (numVertices full) 1

test_vertex_retrievesExpectedVertices :: IO ()
test_vertex_retrievesExpectedVertices =
  let v0 = (1, 0, 0)
      v1 = (0, 1, 0)
      v2 = (0, 0, 1)
      loop = fromVertices [v0, v1, v2]
   in do
        assertEqual (vertex loop 0) v0
        assertEqual (vertex loop 1) v1
        assertEqual (vertex loop 2) v2
        assertEqual (vertex loop (-1)) v2
        assertEqual (vertex loop 3) v0
