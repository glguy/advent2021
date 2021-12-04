{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/4>

Today we played Bingo and picked the first and last winning cards

-}
module Main (main) where

import Advent
import Advent.Format (format)
import Data.List (partition, transpose)

type Board = [[Int]]

main :: IO ()
main =
  do (calls, boxes) <- [format|4 %u&,%n%n((( *%u)+%n)+)&%n|]
     let outcomes = play calls boxes
     print (head outcomes)
     print (last outcomes)

play :: [Int] -> [Board] -> [Int]
play [] _ = []
play (c:calls) boards =
  case partition winner (map (mark c) boards) of
    (winners, losers) -> [c * score w | w <- winners] ++ play calls losers

mark :: Int -> Board -> Board
mark c = map (map (\x -> if x == c then -1 else x))

score :: Board -> Int
score b = sum (filter (/= -1) (concat b))

winner :: Board -> Bool
winner b = f b || f (transpose b)
  where f = any (all (== -1))
