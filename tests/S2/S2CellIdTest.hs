{-# OPTIONS_GHC -F -pgmF htfpp #-}

module S2CellIdTest (htf_thisModulesTests) where

import Data.Word (Word64)
import S2.S2CellId as S2CellId
import Test.Framework

prop_id :: Word64 -> Bool
prop_id rawId = (S2CellId.id . S2CellId $ rawId) == rawId

test_none = assertRawIdEquals 0 none

test_sentinel = assertRawIdEquals 0xFFFFFFFFFFFFFFFF sentinel

test_isValid_none = assertEqual False (isValid none)

test_isValid_sentinel = assertEqual False (isValid sentinel)

test_parentChildRelationships = do
  let parentId = S2CellId 0x1555555555555550
  let parentLevel = level parentId
  forEachChild parentId (assertEqual parentId . parent)
  -- TODO: "loop" over all parent levels, not just the immediate grandparent
  forEachChild parentId (assertEqual (parent parentId) . parentAtLevel (parentLevel - 1))

test_fromToken_none = assertRawIdEquals 0 (fromToken "0x0000000000000000")

test_fromToken_sentinel_upper =
  assertRawIdEquals 0xFFFFFFFFFFFFFFFF (fromToken "0xFFFFFFFFFFFFFFFF")

test_fromToken_sentinel_lower =
  assertRawIdEquals 0xFFFFFFFFFFFFFFFF (fromToken "0xffffffffffffffff")

test_toToken_none = assertEqual "0x0000000000000000" (toToken none)

test_toToken_sentinel = assertEqual "0xffffffffffffffff" (toToken sentinel)

prop_fromToken_toToken :: Word64 -> Bool
prop_fromToken_toToken rawId = (S2CellId.id . fromToken . toToken . S2CellId $ rawId) == rawId

assertRawIdEquals :: Word64 -> S2CellId -> IO ()
assertRawIdEquals expectedRawId cellId = assertEqual expectedRawId (S2CellId.id cellId)

forEachChild :: S2CellId -> (S2CellId -> IO ()) -> IO ()
forEachChild cellId f = do
  f (child 0 cellId)
  f (child 1 cellId)
  f (child 2 cellId)
  f (child 3 cellId)

main = htfMain htf_thisModulesTests
