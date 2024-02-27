module S2.S2CellId
  ( S2CellId (..),
    S2.S2CellId.id,
    none,
    sentinel,
    isValid,
    face,
    level,
    parent,
    parentAtLevel,
    child,
    fromToken,
    toToken,
    lsb,
    lsbForLevel,
  )
where

import Data.Bits
  ( Bits (complement, setBit, shiftL, shiftR, testBit, (.&.), (.|.)),
    FiniteBits (countTrailingZeros),
  )
import Data.Char (digitToInt, intToDigit, isHexDigit)
import Data.Word (Word64)

-- |
-- An S2CellId is a 64-bit unsigned integer that uniquely identifies a
-- cell in the S2 cell decomposition.  It has the following format:
--
--    id = [face][face_pos]
--
--    face:     a 3-bit number (range 0..5) encoding the cube face.
--
--    face_pos: a 61-bit number encoding the position of the center of this
--              cell along the Hilbert curve over this face (see the Wiki
--              pages for details).
--
-- Sequentially increasing cell ids follow a continuous space-filling curve
-- over the entire sphere.  They have the following properties:
--
-- - The id of a cell at level k consists of a 3-bit face number followed
--   by k bit pairs that recursively select one of the four children of
--   each cell.  The next bit is always 1, and all other bits are 0.
--   Therefore, the level of a cell is determined by the position of its
--   lowest-numbered bit that is turned on (for a cell at level k, this
--   position is 2 * (kMaxLevel - k).)
--
-- - The id of a parent cell is at the midpoint of the range of ids spanned
--   by its children (or by its descendants at any level).
--
-- Leaf cells are often used to represent points on the unit sphere, and
-- this class provides methods for converting directly between these two
-- representations.  For cells that represent 2D regions rather than
-- discrete point, it is better to use the S2Cell class.
newtype S2CellId = S2CellId Word64 deriving (Eq, Ord, Show)

numFaces :: () -> Int
numFaces () = 6

maxLevel :: () -> Int
maxLevel () = 30

posBits :: () -> Int
posBits () = 2 * maxLevel () + 1

-- | The 64-bit unique identifier for this cell.
id :: S2CellId -> Word64
id (S2CellId rawId) = rawId

-- | Returns an invalid cell id.
none :: () -> S2CellId
none () = S2CellId 0

-- |
-- Returns an invalid S2CellId guaranteed to be larger than any valid cell id. Useful for creating
-- indexes.
sentinel :: () -> S2CellId
sentinel () = S2CellId 0xFFFFFFFFFFFFFFFF

-- |
-- Returns true if the result of `id` represents a valid cell. All functions taking S2CellIds as input
-- require `isValid` to be True unless otherwise specified.
isValid :: S2CellId -> Bool
isValid cellId = (face cellId < numFaces ()) && (lsb cellId .&. 0x1555555555555555 > 0)

-- | Which cube face this cell belongs to, in the range 0..5.
face :: S2CellId -> Int
face (S2CellId rawId) = fromIntegral (shiftR rawId (posBits ()))

-- | Return the subdivision level of this cell (range 0..maxLevel)
level :: S2CellId -> Int
level (S2CellId rawId) = maxLevel () - (countTrailingZeros rawId `div` 2)

-- | Return the cell at the previous level.
parent :: S2CellId -> S2CellId
parent cellId@(S2CellId rawId) = S2CellId ((rawId .&. (complement newLsb + 1)) .|. newLsb)
  where
    newLsb = shiftL (lsb cellId) 2

-- | Return the cell at the given level (which must be less than or equal to the current level).
parentAtLevel :: Int -> S2CellId -> S2CellId
parentAtLevel level (S2CellId rawId) = S2CellId ((rawId .&. (complement newLsb + 1)) .|. newLsb)
  where
    newLsb = lsbForLevel level

-- |
-- Return the immediate child of this cell at the given traversal order position (in the range 0 to 3).
-- This cell must not be a leaf cell.
child :: Int -> S2CellId -> S2CellId
{-
To change the level, we need to move the least-significant bit two
positions downward.  We do this by subtracting (4 * new_lsb) and adding
new_lsb.  Then to advance to the given child cell, we add
(2 * position * new_lsb).
-}
child position cellId@(S2CellId rawId) =
  S2CellId (rawId + fromIntegral (2 * position + 1 - 4) * shiftR (lsb cellId) 2)

-- |
-- Returns the string @0x{id[7]}...{id[0]}@ where @id[7]@ is the most significant digit in the
-- hexadecmial representation of `S2CellId.id`, @id[6]@ is the next lowest place value, etc.
-- @`fromToken` (`toToken` x) == x@ even if @x@ is invalid.
toToken :: S2CellId -> String
toToken (S2CellId rawId) = "0x" ++ toTokenImpl rawId 0 0 ""

-- |
-- Decodes an S2CellId from a string representation which is expected to be formatted
-- just like the output of `toToken`. Returns @none ()@ for malformed inputs. @`fromToken`
-- (`toToken` x) == x@ even if @x@ is invalid.
fromToken :: String -> S2CellId
fromToken ('0' : 'x' : s) = fromTokenImpl (Just 0) s 16
fromToken _ = none ()

-- |
-- Return the lowest-numbered bit that is on for this cell id, which is
-- equal to @(uint64{1} << (2 * (kMaxLevel - level)))@.  So for example,
-- @a.lsb() <= b.lsb()@ if and only if @a.level() >= b.level()@, but the
-- first test is more efficient.
lsb :: S2CellId -> Word64
lsb (S2CellId rawId) = rawId .&. (complement rawId + 1)

-- | Return the lowest-numbered bit that is on for cells at the given level.
lsbForLevel :: Int -> Word64
lsbForLevel level = shiftL 1 (2 * (maxLevel () - level))

-- Implementation details

toTokenImpl :: Word64 -> Int -> Int -> String -> String
toTokenImpl rawId i c acc
  | i == 64 = acc
  | r == 3 = toTokenImpl rawId (i + 1) 0 (intToDigit c' : acc)
  | otherwise = toTokenImpl rawId (i + 1) c' acc
  where
    r = i `mod` 4
    c' = if testBit rawId i then setBit c r else c

fromTokenImpl :: Maybe Word64 -> String -> Int -> S2CellId
fromTokenImpl (Just rawId) [] 0 = S2CellId rawId
fromTokenImpl Nothing _ _ = none ()
fromTokenImpl _ (x : xs) 0 = none ()
fromTokenImpl _ [] _ = none ()
fromTokenImpl (Just rawId) (c : cs) i = fromTokenImpl (shiftAndAppend rawId c) cs (i - 1)

shiftAndAppend :: Word64 -> Char -> Maybe Word64
shiftAndAppend rawId c = if isHexDigit c then Just (rawId * 16 + toEnum (digitToInt c)) else Nothing
