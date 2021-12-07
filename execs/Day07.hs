{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/7>

Find the minimum fuel cost to move the submarines to a common point.

-}
module Main (main) where

import Advent.Format (format)
import Data.List (sort)

main :: IO ()
main =
  do inp <- [format|7 %u&,%n|]

     let median = sort inp !! (length inp `div` 2)
     print (sum [abs (x - median) | x <- inp])

     let (lo,hi) = (minimum inp, maximum inp)
     print (minimum [sum [triangle (abs (x-y)) | x <- inp] | y <- [lo..hi]])

triangle :: Int -> Int
triangle i = i * (i+1) `div` 2
