{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/11>

-}
module Main (main) where

import Advent (getInputLines)
import Advent.Coord (Coord(..), neighbors)
import Data.Char (digitToInt)
import Data.List (elemIndex, unfoldr)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main =
  do inp <- getInputLines 11
     let m = Map.fromList [(C y x, digitToInt z) | (y,zs) <- zip [0..] inp, (x,z) <- zip [0..] zs ]
     let flashes = simulate m
     print (sum (take 100 flashes))
     print (1 + fromJust (elemIndex (Map.size m) flashes))

simulate :: Map Coord Int -> [Int]
simulate = unfoldr (Just . step)

step :: Map Coord Int -> (Int, Map Coord Int)
step m =
  case foldl flash (Set.empty, fmap (1+) m) [k | (k,9) <- Map.toList m] of
    (flashed, m') -> (Set.size flashed, dim m')

dim :: Map Coord Int -> Map Coord Int
dim = fmap (\e -> if e > 9 then 0 else e)

flash :: (Set Coord, Map Coord Int) -> Coord -> (Set Coord, Map Coord Int)
flash (flashed, m) x
  | Set.notMember x flashed
  , Just e <- Map.lookup x m = 
      let m' = Map.insert x (e+1) m in
      if e >= 9
        then foldl flash (Set.insert x flashed, m') (neighbors x)
        else (flashed, m')
  | otherwise = (flashed, m)
