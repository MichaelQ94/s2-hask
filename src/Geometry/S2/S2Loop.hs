{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Geometry.S2.S2Loop
  ( S2Loop,
    empty,
    full,
    fromVertices,
    numVertices,
    vertex,
  )
where

import Data.Sequence (Seq, fromList, index, length)
import Geometry.S2.S2Point ( S2Point )

-- |
-- An S2Loop represents a simple spherical polygon.  It consists of a single
-- chain of vertices where the first vertex is implicitly connected to the
-- last. All loops are defined to have a CCW orientation, i.e. the interior of
-- the loop is on the left side of the edges.  This implies that a clockwise
-- loop enclosing a small area is interpreted to be a CCW loop enclosing a
-- very large area.
--
-- Loops are not allowed to have any duplicate vertices (whether adjacent or
-- not).  Non-adjacent edges are not allowed to intersect, and furthermore edges
-- of length 180 degrees are not allowed (i.e., adjacent vertices cannot be
-- antipodal).  Loops must have at least 3 vertices (except for the empty and
-- full loops discussed below).  Although these restrictions are not enforced
-- in optimized code, you may get unexpected results if they are violated.
--
-- There are two special loops: the "empty loop" contains no points, while the
-- "full loop" contains all points.  These loops do not have any edges, but to
-- preserve the invariant that every loop can be represented as a vertex
-- chain, they are defined as having exactly one vertex each (see empty and
-- eull).
--
-- Point containment of loops is defined such that if the sphere is subdivided
-- into faces (loops), every point is contained by exactly one face.  This
-- implies that loops do not necessarily contain their vertices.
--
-- Note: The reason that duplicate vertices and intersecting edges are not
-- allowed is that they make it harder to define and implement loop
-- relationships, e.g. whether one loop contains another.  If your data does
-- not satisfy these restrictions, you can use S2Builder to normalize it.
newtype S2Loop = S2Loop (Seq S2Point) deriving (Eq, Show)

-- |
-- A special loop of length 1 that creates an empty loop (i.e., a
-- loop with no edges that contains no points).  Example usage:
--
-- The loop may be safely encoded lossily (e.g. by snapping it to an S2Cell
-- center) as long as its position does not move by 90 degrees or more.
empty :: S2Loop
empty = fromVertices [(0, 0, 1)]

-- |
-- A special vertex chain of length 1 that creates a full loop (i.e., a loop
-- with no edges that contains all points).  See `empty` for details.
full :: S2Loop
full = fromVertices [(0, 0, -1)]

-- | Creates an S2Loop with the given vertex chain.
fromVertices :: [S2Point] -> S2Loop
fromVertices = S2Loop . fromList

numVertices :: S2Loop -> Int
numVertices (S2Loop vertices) = Data.Sequence.length vertices

-- |
-- Retrieve the vertex at the given index. If the index lies outside the range @[0, numVertices)@,
-- the index will wrap around to give a valid vertex.
vertex :: S2Loop -> Int -> S2Point
vertex loop@(S2Loop vertices) i = index vertices (i `mod` numVertices loop)
