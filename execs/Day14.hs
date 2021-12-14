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

import Advent ( format )
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.MemoTrie (memo3)

-- | >>> :main
-- 2068
-- 2158894777814
main :: IO ()
main =
  do (seed, rules) <- [format|14 %s%n%n(%c%c -> %c%n)*|]
     let ruleMap = Map.fromList [((l, r), m) | (l,r,m) <- rules]
     print (solve ruleMap 10 seed)
     print (solve ruleMap 40 seed)

-- | Given the polymer insertion rules and a left and right
-- element, compute the resulting counts including the left
-- side, all generated elements, but not the right-most side.
-- Given the computed counts return the difference between the
-- most and least frequent element.
solve ::
  Map (Char, Char) Char {- ^ pair insertion rules -} ->
  Int                   {- ^ iteration count      -} ->
  String                {- ^ polymer template     -} ->
  Int
solve ruleMap n seed = maximum occ - minimum occ
  where
    occ = Map.insertWith (+) (last seed) 1
        $ Map.unionsWith (+) (zipWith (go n) seed (tail seed))

    go = memo3 \i l r ->
      if i == 0
        then Map.singleton l 1
        else let m = ruleMap Map.! (l,r) in
             Map.unionWith (+) (go (i - 1) l m) (go (i - 1) m r)