{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/8>

Figure out how the miswired segment display works.

-}
module Main (main) where

import Advent ( count )
import Advent.Format (format)

import Data.List ( foldl', permutations, sort )
import Data.Map (Map)
import Data.Map qualified as Map

main :: IO ()
main = do
  inp <- [format|8 (%s&  %| %s& %n)*|]
  let outs = map solve inp
  print (count (`elem` [1,4,7,8]) (concat outs))
  print (sum (map toInt outs))

toInt :: [Int] -> Int
toInt = foldl' (\acc x -> 10 * acc + x) 0

wires :: String
wires = ['a'..'g']

segments :: Map String Int
segments = Map.fromList (zip ["abcefg","cf","acdeg","acdfg","bcdf","abdfg","abdefg","acf","abcdefg","abcdfg"] [0..9])

decode :: String -> Int
decode x = segments Map.! x

solve :: ([String], [String]) -> [Int]
solve (xs, ys) = head
  [ [decode (rewire y) | y <- ys]
  | assignment <- Map.fromList . zip wires <$> permutations wires
  , let rewire x = sort (map (assignment Map.!) x)
  , all (\z -> rewire z `Map.member` segments) xs
  ]