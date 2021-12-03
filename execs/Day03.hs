{-# Language ImportQualifiedPost, QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/3>

Select binary numbers using the most and least common bit
in each position.

-}
module Main (main) where

import Advent (cardinality)
import Advent.Format (format)
import Data.List (foldl', transpose)
import Data.Map qualified as Map

-- | A bit
data B = B0 | B1 deriving (Read, Show, Eq, Ord)

-- | Complement of the bit
cmpl :: B -> B
cmpl B0 = B1
cmpl B1 = B0

pure[] -- make B available for reify in format

main :: IO ()
main =
  do inp <- [format|3 (@B*%n)*|]
     print (harness pick1 inp)
     print (harness pick2 inp)

-- | Interpret list of booleans as a big-endian binary number
toNum :: [B] -> Integer
toNum = foldl' (\acc b -> 2*acc + case b of B0->0; B1->1) 0

-- | Use selection function to pick output bit by column
pick1 :: ([B] -> B) -> [[B]] -> [B]
pick1 sel xs = map sel (transpose xs)

-- | Use selection function to pick bit and keep those entries
pick2 :: ([B] -> B) -> [[B]] -> [B]
pick2 _ [x] = x
pick2 sel xs = b : pick2 sel [ ys | y:ys <- xs, b == y]
  where
    b = sel (map head xs)

-- | Given a function that requires a selection function run
-- it on the selection function picking the most and least frequent
-- values and then multiple those results together
harness :: (([B] -> B) -> [[B]] -> [B]) -> [[B]] -> Integer
harness f xs = toNum (f k xs) * toNum (f (cmpl . k) xs)
  where
    look = Map.findWithDefault 0
    h m | look B0 m <= look B1 m = B1
        | otherwise              = B0
    k = h . cardinality