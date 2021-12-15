{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/15>

Finding the shortest route through a cave, and then
finding the shortest route through a slightly larger cave.

-}
module Main (main) where

import Advent.Coord
import Advent.Input (getInputMap)
import Advent.Search (astar)
import Data.Char
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)

-- | >>> :main
-- 698
-- 3022
main :: IO ()
main =
 do inp <- fmap digitToInt <$> getInputMap 15
    print (solve inp)
    print (solve (extendCave inp))

-- | Compute the risk score traveling through a cave.
solve :: Map Coord Int -> Int
solve m = fromJust (lookup end costs)
  where
    end = maximum (Map.keys m)
    costs = astar step origin
    step here = [ (next, cost, 0)
                | next <- cardinal here
                , Just cost <- [Map.lookup next m]]

-- | Build a larger cave by tiling the input cave in a 5x5
-- grid. Added caves have their risk values updated according
-- to their new locations.
extendCave :: Map Coord Int -> Map Coord Int
extendCave m = mconcat
  [ (fixRisk . (tx + ty +)) <$>
    Map.mapKeysMonotonic (addCoord dy . addCoord dx) m
    | tx <- [0..4], let dx = scaleCoord (wx*tx) east
    , ty <- [0..4], let dy = scaleCoord (wy*ty) south
    ]
  where
    C hiy hix = maximum (Map.keys m)
    wy = hiy+1
    wx = hix+1 

-- | Risks are defined to roll over from 9 back to 1
--
-- >>> fixRisk <$> [1,5,9,12]
-- [1,5,9,3]
fixRisk :: Int -> Int
fixRisk x = (x - 1) `mod` 9 + 1