{-# Language QuasiQuotes, ParallelListComp #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/5>

The input is a bunch of lines; count intersections.

-}
module Main (main) where

import Advent (count)
import Advent.Format (format)
import Data.List (group, sort)

main :: IO ()
main =
  do inp <- [format|5 (%u,%u -> %u,%u%n)*|]
     print (solve (filter isStraight inp))
     print (solve inp)

solve :: [(Int, Int, Int, Int)] -> Int
solve = count (not . null . tail) . group . sort . concatMap points

isStraight :: (Int, Int, Int, Int) -> Bool
isStraight (x1, y1, x2, y2) = x1 == x2 || y1 == y2

points :: (Int, Int, Int, Int) -> [(Int, Int)]
points (x1, y1, x2, y2)
  | x1 == x2  = [(x1,y) | y <- range y1 y2]
  | y1 == y2  = [(x,y1) | x <- range x1 x2]
  | otherwise = [(x,y) | x <- range x1 x2 | y <- range y1 y2]

range :: Int -> Int -> [Int]
range x y
  | x <= y    = [x .. y]
  | otherwise = [x, x-1 .. y]
        




