{-# Language QuasiQuotes, ParallelListComp #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/5>

The input is a bunch of segments; count intersections.

-}
module Main (main) where

import Advent (counts, countBy)
import Advent.Format (format)

-- | >>> :main
-- 8060
-- 21577
main :: IO ()
main =
  do inp <- [format|5 (%u,%u -> %u,%u%n)*|]
     print (solve (filter isStraight inp))
     print (solve inp)

-- | Compute the number of points covered by more than one segment
solve :: [(Int, Int, Int, Int)] -> Int
solve = countBy (> 1) . counts . concatMap points

-- | Predicate for straight segments
isStraight :: (Int, Int, Int, Int) -> Bool
isStraight (x1, y1, x2, y2) = x1 == x2 || y1 == y2

-- | Enumerate the points contained in a segment
points :: (Int, Int, Int, Int) -> [(Int, Int)]
points (x1, y1, x2, y2)
  | x1 == x2  = [(x1,y) | y <- range y1 y2]
  | y1 == y2  = [(x,y1) | x <- range x1 x2]
  | otherwise = [(x,y)  | x <- range x1 x2 | y <- range y1 y2]

-- | Inclusive enumeration of the integers between two bounds
range :: Int -> Int -> [Int]
range x y
  | x <= y    = [x .. y]
  | otherwise = [x, x-1 .. y]
