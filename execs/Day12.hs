{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/12>

Search around a cave visiting some caves more than others.

-}
module Main (main) where

import Advent.Format (format)
import Data.Char (isUpper)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | >>> :main
-- 3761
-- 99138
main :: IO ()
main =
 do inp <- toAdj <$> [format|12 (%a+-%a+%n)*|]
    print (start inp False)
    print (start inp True)

-- | Compute directed edge map from a list of undirected edges.
toAdj :: [(String,String)] -> Map String [String]
toAdj inp = Map.fromListWith (++)
  [(x,[y]) | (a,b) <- inp, (x,y) <- [(a,b),(b,a)], y /= "start"]

-- | Search the cave exploration given the directed edges and a
-- flag if we're allowed to visit a small cave an extra time.
start :: Map String [String] -> Bool -> Int
start paths extra = go paths extra "start" Set.empty 

go :: Map String [String] -> Bool -> String -> Set String -> Int
go paths extra here seen = sum (map f (paths Map.! here))
  where
    f next
      | next == "end"           = 1
      | isUpper (head next)     = go paths extra next seen
      | Set.notMember next seen = go paths extra next (Set.insert next seen)
      | extra                   = go paths False next seen
      | otherwise               = 0
