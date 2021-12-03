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

import Advent (count)
import Advent.Format (format)
import Data.List (foldl', transpose)

-- | A bit
data B = B0 | B1 deriving (Read, Show, Eq, Ord)

-- | Bit complement
cmpl :: B -> B
cmpl B0 = B1
cmpl B1 = B0

mempty -- make B available for reify in format

main :: IO ()
main =
  do inp <- [format|3 (@B*%n)*|]
     print (harness pick1 inp)
     print (harness pick2 inp)

-- | Interpret list of bits as a big-endian binary number
fromBits :: [B] -> Integer
fromBits = foldl' (\acc b -> 2*acc + case b of B0->0; B1->1) 0

-- | Use selection function to pick output bit by column
pick1 :: ([B] -> B) -> [[B]] -> [B]
pick1 sel xs = map sel (transpose xs)

-- | Use selection function to filter entries by each bit column
pick2 :: ([B] -> B) -> [[B]] -> [B]
pick2 _ [x] = x
pick2 sel xs = b : pick2 sel [ ys | y:ys <- xs, b == y]
  where
    b = sel (map head xs)

-- | Given a function that requires a selection function run
-- it on the selection function picking the most and least frequent
-- values and then multiple those results together
harness :: (([B] -> B) -> [[B]] -> [B]) -> [[B]] -> Integer
harness f xs = fromBits (f h xs) * fromBits (f (cmpl . h) xs)
  where
    h m | count (B0==) m <= count (B1==) m = B1
        | otherwise                        = B0