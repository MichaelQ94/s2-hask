module S2.S2CellId
(
  S2CellId,
  fromHexStr,
  toHexStr
) where
import Data.Bits (Bits(setBit, testBit))
import qualified Data.Word
import Data.Char (digitToInt, intToDigit)
import Data.Maybe

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

This class is intended to be copied by value as desired.  It uses
the default copy constructor and assignment operator.
-}

type S2CellId = Data.Word.Word64

{-
Returns the string "0x{id[7]}...{id[0]}" where {id[7]} is the most significant digit
in the hexadecmial representation of {id}, id[6] is the next lowest place value, etc.
-}
toHexStr :: S2CellId -> String
toHexStr id = "0x" ++ toHexStrImpl 0 63 id

{-
Decodes an S2CellId from a string representation which is expected to be formatted
just like the output of toHexStr.
-}
fromHexStr :: String -> Maybe S2CellId
fromHexStr ('0':'x':s) = fromHexStrImpl 0 15 s
fromHexStr _ = Nothing

-- Implementation details

toHexStrImpl :: Int -> Int -> S2CellId -> [Char]
toHexStrImpl c i id
  | i == 0 = [intToDigit c']
  | r == 0 = intToDigit c' : toHexStrImpl 0 (i-1) id
  | otherwise = toHexStrImpl c' (i-1) id
  where r = i `mod` 4
        c' = if testBit id i then setBit c r else c

fromHexStrImpl :: S2CellId -> Int -> String -> Maybe S2CellId
fromHexStrImpl _ 0 (c:d:ds) = Nothing
fromHexStrImpl id 0 [c] = Just (shiftAndAppend id c)
fromHexStrImpl id i (c:cs) = fromHexStrImpl (shiftAndAppend id c) (i-1) cs
fromHexStrImpl _ _ [] = Nothing

shiftAndAppend :: S2CellId -> Char -> S2CellId
shiftAndAppend id c = id * 16 + toEnum (digitToInt c)
