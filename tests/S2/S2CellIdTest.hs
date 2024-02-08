{-# OPTIONS_GHC -F -pgmF htfpp #-}
module S2CellIdTest (htf_thisModulesTests) where

import Data.Word (Word64)
import S2.S2CellId
import qualified S2.S2CellId as S2CellId
import Test.Framework
import Data.Bool (Bool)

prop_id :: Word64 -> Bool
prop_id rawId = (S2CellId.id . S2CellId $ rawId) == rawId

test_none = do
  assertRawIdEquals 0 (S2CellId.none ())

test_sentinel = do
  assertRawIdEquals 0xFFFFFFFFFFFFFFFF (S2CellId.sentinel ())

test_isValid_none = do
  assertEqual False (isValid . S2CellId.none $ ())

test_isValid_sentinel = do
  assertEqual False (isValid . S2CellId.sentinel $ ())

test_fromToken_none = do
  assertRawIdEquals 0 (S2CellId.fromToken "0x0000000000000000")

test_fromToken_sentinel_upper = do
  assertRawIdEquals 0xFFFFFFFFFFFFFFFF (S2CellId.fromToken "0xFFFFFFFFFFFFFFFF")

test_fromToken_sentinel_lower = do
  assertRawIdEquals 0xFFFFFFFFFFFFFFFF (S2CellId.fromToken "0xffffffffffffffff")

test_toTokenToken_none = do
  assertEqual "0x0000000000000000" (S2CellId.toToken . S2CellId.none $ ())

test_toToken_sentinel = do
  assertEqual "0xffffffffffffffff" (S2CellId.toToken . S2CellId.sentinel $ ())

prop_fromToken_toToken :: Word64 -> Bool
prop_fromToken_toToken rawId =
  (S2CellId.id . S2CellId.fromToken . S2CellId.toToken . S2CellId $ rawId) == rawId

assertRawIdEquals :: Word64 -> S2CellId.S2CellId -> IO ()
assertRawIdEquals expectedRawId cellId = assertEqual expectedRawId (S2CellId.id cellId)

main = htfMain htf_thisModulesTests
