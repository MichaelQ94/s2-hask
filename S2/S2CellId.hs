module S2.S2CellId
(
  S2CellId(..),
  S2.S2CellId.id,
  none,
  sentinel,
  isValid,
  face,
  fromToken,
  toToken,
  lsb
) where
import Data.Bits (Bits((.&.), complement, setBit, shiftR, testBit))
import Data.Char (digitToInt, intToDigit, isHexDigit)
import Data.Word (Word64)

{-
An S2CellId is a 64-bit unsigned integer that uniquely identifies a
cell in the S2 cell decomposition.  It has the following format:

   id = [face][face_pos]

   face:     a 3-bit number (range 0..5) encoding the cube face.

   face_pos: a 61-bit number encoding the position of the center of this
             cell along the Hilbert curve over this face (see the Wiki
             pages for details).

Sequentially increasing cell ids follow a continuous space-filling curve
over the entire sphere.  They have the following properties:

- The id of a cell at level k consists of a 3-bit face number followed
  by k bit pairs that recursively select one of the four children of
  each cell.  The next bit is always 1, and all other bits are 0.
  Therefore, the level of a cell is determined by the position of its
  lowest-numbered bit that is turned on (for a cell at level k, this
  position is 2 * (kMaxLevel - k).)

- The id of a parent cell is at the midpoint of the range of ids spanned
  by its children (or by its descendants at any level).

Leaf cells are often used to represent points on the unit sphere, and
this class provides methods for converting directly between these two
representations.  For cells that represent 2D regions rather than
discrete point, it is better to use the S2Cell class.
-}
newtype S2CellId = S2CellId Word64 deriving (Eq, Ord)

numFaces :: () -> Int
numFaces () = 6

maxCellLevel :: () -> Int
maxCellLevel () = 30

posBits :: () -> Int
posBits () = 2 * maxCellLevel () + 1

-- | The 64-bit unique identifier for this cell.
id :: S2CellId -> Word64
id (S2CellId rawId) = rawId

-- | Returns an invalid cell id.
none :: () -> S2CellId
none () = S2CellId 0

{-|
Returns an invalid S2CellId guaranteed to be larger than any valid cell id. Useful for creating
indexes.
-}
sentinel :: () -> S2CellId
sentinel () = S2CellId 0xFFFFFFFFFFFFFFFF

{-|
Returns true if the result of `id` represents a valid cell. All functions taking S2CellIds as input
require `isValid` to be True unless otherwise specified.
-}
isValid :: S2CellId -> Bool
isValid cellId = (face cellId < numFaces ()) && (lsb cellId .&. 0x1555555555555555 > 0)

-- | Which cube face this cell belongs to, in the range 0..5.
face :: S2CellId -> Int
face = fromIntegral . flip shiftR (posBits ()) . S2.S2CellId.id

{-|
Returns the string @0x{id[7]}...{id[0]}@ where @id[7]@ is the most significant digit in the
hexadecmial representation of `S2CellId.id`, @id[6]@ is the next lowest place value, etc.
@`fromToken` (`toToken` x) == x@ even if @x@ is invalid.
-}
toToken :: S2CellId -> String
toToken cellId = "0x" ++ toTokenImpl (S2.S2CellId.id cellId) 63 0

{-|
Decodes an S2CellId from a string representation which is expected to be formatted
just like the output of `toToken`. Returns @none ()@ for malformed inputs. @`fromToken`
(`toToken` x) == x@ even if @x@ is invalid.
-}
fromToken :: String -> S2CellId
fromToken ('0':'x':s) = fromTokenImpl (Just 0) s 16
fromToken _ = none ()

{-|
Return the lowest-numbered bit that is on for this cell id, which is
equal to @(uint64{1} << (2 * (kMaxLevel - level)))@.  So for example,
@a.lsb() <= b.lsb()@ if and only if @a.level() >= b.level()@, but the
first test is more efficient.
-}
lsb :: S2CellId -> Word64
lsb cellId = rawId .&. (complement rawId + 1) where rawId = S2.S2CellId.id cellId

-- Implementation details

toTokenImpl :: Word64 -> Int -> Int -> String
toTokenImpl rawId i c
  | i == 0 = [intToDigit c']
  | r == 0 = intToDigit c' : toTokenImpl rawId (i-1) 0
  | otherwise = toTokenImpl rawId (i-1) c'
  where r = i `mod` 4
        c' = if testBit rawId i then setBit c r else c

fromTokenImpl :: Maybe Word64 -> String -> Int -> S2CellId
fromTokenImpl (Just rawId) [] 0 = S2CellId rawId
fromTokenImpl Nothing _ _ = none ()
fromTokenImpl _ (x:xs) 0 = none ()
fromTokenImpl _ [] _ = none ()
fromTokenImpl (Just rawId) (c:cs) i = fromTokenImpl (shiftAndAppend rawId c) cs (i-1)

shiftAndAppend :: Word64 -> Char -> Maybe Word64
shiftAndAppend rawId c = if isHexDigit c then Just (rawId * 16 + toEnum (digitToInt c)) else Nothing
