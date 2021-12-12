{-# Language ImportQualifiedPost, QuasiQuotes, BlockArguments #-}
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
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Map (Map)
import Data.Map qualified as Map
import Advent.SmallSet (SmallSet)
import Advent.SmallSet as Set
import Data.List (mapAccumL)
import Data.MemoTrie

-- | >>> :main
-- 3761
-- 99138
main :: IO ()
main =
 do inp <- toAdj . label <$> [format|12 (%a+-%a+%n)*|]
    print (start inp False)
    print (start inp True)

-- | Compute directed edge map from a list of undirected edges.
toAdj :: [(Int, Int)] -> IntMap [Int]
toAdj inp = IntMap.fromListWith (++)
  [(x,[y]) | (a,b) <- inp, (x,y) <- [(a,b),(b,a)], y /= 0]

-- | Search the cave exploration given the directed edges and a
-- flag if we're allowed to visit a small cave an extra time.
start :: IntMap [Int] -> Bool -> Int
start paths extra = go extra 0 Set.empty 
  where
    go = memo3 \extra here seen ->
      let
        f next
          | next == 1               = 1
          | next < 0                = go extra next seen
          | not (Set.member next seen) = go extra next (Set.insert next seen)
          | extra                   = go False next seen
          | otherwise               = 0
        in sum (map f (paths IntMap.! here))

-- | Map all the cave names to integers. Use negative integers for big caves.
label :: [(String, String)] -> [(Int,Int)]
label = snd . mapAccumL f (Map.fromList [("start",0),("end",1)])
  where
    g m x = case Map.lookup x m of
              Just i -> (m, i)
              Nothing -> (Map.insert x i m, i)
                where i = if isUpper (head x) then -Map.size m else Map.size m
    f m (x,y) = (m2, (x',y'))
      where
        (m1,x') = g m x
        (m2,y') = g m1 y