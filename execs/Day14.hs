{-# Language BlockArguments, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/14>

Build a huge polymer chain and compute how many of
each element it contains.

This problem requires memoization as the size of the
resulting polymer would be humungous!

-}
module Main (main) where

import Advent (format, power)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map.Strict qualified as Map

-- | Associates pairs of elements followed by an element
-- with the resulting pairs of elements when applying the rule.
type Rule a = Map (a,a) (Map (a,a) Int)

-- | >>> :main
-- 2068
-- 2158894777814
main :: IO ()
main =
  do (seed, table) <- [format|14 %s%n%n(%c%c -> %c%n)*|]
     let rule = tableToRule table
     print (solve rule 10 seed)
     print (solve rule 40 seed)

solve :: Ord a => Rule a -> Int -> [a] -> Int
solve rule n seed = maximum occ - minimum occ
  where
    ruleN = power thenRule rule n

    occ = Map.insertWith (+) (last seed) 1
        $ Map.unionsWith (+)
        [ Map.mapKeysWith (+) fst (ruleN Map.! pair)
        | pair <- zip seed (tail seed)]

tableToRule :: Ord a => [(a,a,a)] -> Rule a
tableToRule xs = Map.fromList [((l,r), Map.fromList [((l,m), 1),((m,r), 1)]) | (l,r,m) <- xs]

thenRule :: Ord a => Rule a -> Rule a -> Rule a
thenRule x y = x <&> \m ->
  Map.unionsWith (+) [(v *) <$> (y Map.! k) | (k,v) <- Map.toList m]
