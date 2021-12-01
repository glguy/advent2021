{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 1 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/1>

Count the number of increasing pairs of measurements.

-}
module Main where

import Advent (count)
import Advent.Format (format)

-- | >>> :main
-- 1681
-- 1704

main :: IO ()
main =
  do input <- [format|1 (%u%n)*|]
     print (solve 1 input)
     print (solve 3 input)

solve :: Int -> [Int] -> Int
solve n input = count (uncurry (<)) (zip input (drop n input))
