{-# OPTIONS_GHC -F -pgmF htfpp #-}
module S2CellIdTest (htf_thisModulesTests) where

import S2.S2CellId
import Test.Framework

test_none = do
  assertEqual (S2.S2CellId.id (S2.S2CellId.none ())) 0

test_sentinel = do
  assertEqual (S2.S2CellId.id (S2.S2CellId.sentinel ())) 0xFFFFFFFFFFFFFFFF

main = htfMain htf_thisModulesTests
