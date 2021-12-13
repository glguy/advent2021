{-# Language ImportQualifiedPost #-}
{-|
Module      : Advent.Coord
Description : Row-major coordinates
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

2-dimensional coordinates commonly found in AoC problems
where y grows down, x grows right.

@
   -y
    ↑
-x ←0→ +x
    ↓
   +y
@

-}
{-# Language BangPatterns, TypeFamilies, TypeOperators, DeriveGeneric #-}
module Advent.Coord where

import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Arr (Ix(unsafeIndex, range, index, inRange, unsafeRangeSize) )
import GHC.Generics (Generic)
import GHC.Ix (indexError)

-- | Two-dimensional coordinate
data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq, Generic)

-- | Row (y) of coordinate
coordRow :: Coord -> Int
coordRow (C row _) = row

-- | Column (x) of coordinate
coordCol :: Coord -> Int
coordCol (C _ col) = col

-- | Row-major coordinate indexing
--
-- >>> range (C 1 1, C 2 2)
-- [C 1 1,C 1 2,C 2 1,C 2 2]
--
-- >>> index (C 1 1, C 2 2) <$> range (C 1 1, C 2 2)
-- [0,1,2,3]
instance Ix Coord where
  unsafeIndex (C lorow locol, C hirow hicol) (C row col) =
    unsafeIndex (lorow,hirow) row * unsafeRangeSize (locol,hicol) + unsafeIndex (locol,hicol) col
  {-# INLINE unsafeIndex #-}

  index b i
    | inRange b i = unsafeIndex b i
    | otherwise   = indexError b i "Coord" 
  {-# INLINE index #-}

  inRange (C lorow locol, C hirow hicol) (C row col) =
    inRange (lorow,hirow) row && inRange (locol,hicol) col
  {-# INLINE inRange #-}

  range (C lorow locol, C hirow hicol) =
    [C row col | row <- [lorow..hirow], col <- [locol..hicol]]
  {-# INLINE range #-}

  unsafeRangeSize (C lorow locol, C hirow hicol) =
    (hirow - lorow + 1) * (hicol - locol + 1)
  {-# INLINE unsafeRangeSize #-}

-- | Decrement y coordinate
above :: Coord -> Coord
above (C y x) = C (y-1) x

-- | Increment y coordinate
below :: Coord -> Coord
below (C y x) = C (y+1) x

-- | Decrement x coordinate
left :: Coord -> Coord
left  (C y x) = C y (x-1)

-- | Increment x coordinate
right :: Coord -> Coord
right (C y x) = C y (x+1)

-- | Swap x and y coordinates
invert :: Coord -> Coord
invert (C y x) = C x y

-- | Rotate coordinate 90-degrees CCW about the origin
turnLeft :: Coord -> Coord
turnLeft  (C y x) = C (-x) y

-- | Rotate coordinate 90-degrees CW about the origin
turnRight :: Coord -> Coord
turnRight (C y x) = C x (-y)

-- | Rotate the coordinate 180-degrees about the origin
turnAround :: Coord -> Coord
turnAround (C y x) = C (-y) (-x)

-- | Compute the Manhattan distance between two coordinates
manhattan :: Coord -> Coord -> Int
manhattan (C x y) (C u v) = abs (x-u) + abs (y-v)

-- | Compute the 4 cardinal neighbors of a coordinate: north, south, east, west
cardinal :: Coord -> [Coord]
cardinal c = c `seq` [above c, left c, right c, below c]

-- | Compute the 8 cardinal neighbors and diagonal neighbors
neighbors :: Coord -> [Coord]
neighbors c = c `seq` [above c, left c, right c, below c,
                       above (left c), above (right c),
                       below (left c), below (right c)]

-- | Find the upper-left and lower-right coordinates that
-- inclusively contain all the coordinates in a list of
-- coordinates.
boundingBox :: [Coord] -> Maybe (Coord, Coord)
boundingBox t =
  case t of
    []         -> Nothing
    C y x : cs -> go y x y x cs
  where
    go loy lox hiy hix [] = Just (C loy lox, C hiy hix)
    go loy lox hiy hix (C y x : cs) = go (min loy y) (min lox x) (max hiy y) (max hix x) cs

-- | Coordinate at the origin
origin :: Coord
origin = C 0 0

-- | Unit vector pointing up
north :: Coord
north = C (-1) 0

-- | Unit vector pointing right
east :: Coord
east = C 0 1

-- | Unit vector pointing down
south :: Coord
south = C 1 0

-- | Unit vector pointing left
west :: Coord
west = C 0 (-1)

-- | Add two coordinates as vectors from the origin
addCoord :: Coord -> Coord -> Coord
addCoord (C y x) (C v u) = C (y+v) (x+u)

-- | Scale a coordinate as a vector from the origin
scaleCoord :: Int -> Coord -> Coord
scaleCoord n (C x y) = C (x*n) (y*n)

-- | Render a minimal bounding box containing all the characters
-- at the given coordinates. Empty space filled with space characters.
drawCoords :: Map Coord Char -> String
drawCoords pixels =
  case boundingBox (Map.keys pixels) of
    Nothing -> ""
    Just (C miny minx, C maxy maxx) ->
      unlines [[Map.findWithDefault ' ' (C y x) pixels | x <- [minx .. maxx]] | y <- [miny .. maxy]]

-- | Given a list of lines pair up each character with
-- its position.
coordLines :: [String] -> [(Coord, Char)]
coordLines rows = [(C y x, z) | (y,row) <- zip [0..] rows, (x,z) <- zip [0..] row]
