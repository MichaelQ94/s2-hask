{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Geometry.S2.S2LoopTest (htf_thisModulesTests) where

import Geometry.S2.S2Loop (empty, full, numVertices)
import Test.Framework

test_emptyLoopIsSingleton = assertEqual (numVertices empty) 1

test_fullLoopIsSingleton = assertEqual (numVertices full) 1
