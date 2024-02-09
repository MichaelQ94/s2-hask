{-# OPTIONS_GHC -F -pgmF htfpp #-}

module S2CellIdTest (htf_thisModulesTests) where

import Data.Word (Word64)
import S2.S2CellId as S2CellId
import Test.Framework

prop_id :: Word64 -> Bool
prop_id rawId = (S2CellId.id . S2CellId $ rawId) == rawId

test_none = do
  assertRawIdEquals 0 (none ())

test_sentinel = do
  assertRawIdEquals 0xFFFFFFFFFFFFFFFF (sentinel ())

test_isValid_none = do
  assertEqual False (isValid . none $ ())

test_isValid_sentinel = do
  assertEqual False (isValid . sentinel $ ())

test_parentChildRelationships = do
  let parentId = S2CellId 0x1555555555555550
  -- TODO: "loop" this
  assertEqual parentId (parent . child 0 $ parentId)
  assertEqual parentId (parent . child 1 $ parentId)
  assertEqual parentId (parent . child 2 $ parentId)
  assertEqual parentId (parent . child 3 $ parentId)
  assertEqual (parent parentId) (parentAtLevel (level parentId - 1) (parent . child 0 $ parentId))
  assertEqual (parent parentId) (parentAtLevel (level parentId - 1) (parent . child 1 $ parentId))
  assertEqual (parent parentId) (parentAtLevel (level parentId - 1) (parent . child 2 $ parentId))
  assertEqual (parent parentId) (parentAtLevel (level parentId - 1) (parent . child 3 $ parentId))

test_fromToken_none = do
  assertRawIdEquals 0 (fromToken "0x0000000000000000")

test_fromToken_sentinel_upper = do
  assertRawIdEquals 0xFFFFFFFFFFFFFFFF (fromToken "0xFFFFFFFFFFFFFFFF")

test_fromToken_sentinel_lower = do
  assertRawIdEquals 0xFFFFFFFFFFFFFFFF (fromToken "0xffffffffffffffff")

test_toToken_none = do
  assertEqual "0x0000000000000000" (toToken . none $ ())

test_toToken_sentinel = do
  assertEqual "0xffffffffffffffff" (toToken . sentinel $ ())

prop_fromToken_toToken :: Word64 -> Bool
prop_fromToken_toToken rawId = (S2CellId.id . fromToken . toToken . S2CellId $ rawId) == rawId

assertRawIdEquals :: Word64 -> S2CellId -> IO ()
assertRawIdEquals expectedRawId cellId = assertEqual expectedRawId (S2CellId.id cellId)

main = htfMain htf_thisModulesTests
