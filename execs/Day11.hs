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

import Advent (count, getInputLines)
import Advent.Coord (Coord(..), neighbors)
import Data.Char (digitToInt)
import Data.List (elemIndex, unfoldr)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)

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
step m = (count (0==) m', m')
  where m' = foldl flash (fmap (1+) m) [k | (k,9) <- Map.toList m]

flash :: (Map Coord Int) -> Coord -> Map Coord Int
flash m x =
  case Map.lookup x m of
    Just e
      | e > 8 -> foldl flash (Map.insert x 0 m) (neighbors x)
      | e > 0 -> Map.insert x (e+1) m
    _         -> m
