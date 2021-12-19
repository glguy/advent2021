{-# Language LambdaCase, BlockArguments, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/19>

To correlate all the scanner readings this program
selects the first scanner to be "correct". All other
scanners will be oriented relative to the first scanner.
As each scanner's location is fixed it will be queued
to be compared to all the uncorrelated scanner outputs.
Scanning in this order ensures no pair of scanners is
compared more than once.

-}
module Main (main) where

import Advent.Format (format)
import Advent.Coord3 (Coord3(..), origin, manhattan, diff, add)
import Control.Monad ((>=>))
import Data.List (transpose)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

-- | >>> :main
-- 457
-- 13243
main :: IO ()
main =
 do inp <- [format|19 (--- scanner %u ---%n(%d,%d,%d%n)*)&%n|]
    let coord (x,y,z) = C3 x y z
    let scanners = map coord <$> Map.fromList inp

    let (offsets, locations) = unzip (Map.elems (start scanners))
    print (Set.size (Set.unions locations))
    print (maximum (manhattan <$> offsets <*> offsets))

-- | Starts the scanner reading correlation algorithm.
start ::
  Ord a =>
  Map a [Coord3] {- ^ uncorrelated scanner readings -} ->
  Map a (Coord3, Set Coord3) {- ^ correlated scanner locations and readings -}
start scanners =
  case Map.minViewWithKey scanners of
    Nothing -> Map.empty
    Just ((k,v),scanners') ->
      assemble scanners' (Map.singleton k (origin, Set.fromList v)) [k]

-- | Worker for 'start'.
assemble ::
  Ord a =>
  Map a [Coord3]             {- ^ uncorrelated scanner readings -} ->
  Map a (Coord3, Set Coord3) {- ^ correlated scanner locations and readings -} ->
  [a]                        {- ^ recently correlated scanners -} ->
  Map a (Coord3, Set Coord3) {- ^ completed correlated locations and readings -}
assemble remain known _ | Map.null remain = known
assemble _ _ [] = error "bad input"
assemble remain known (i:cs) =
  assemble (Map.difference remain new) (Map.union known new) (Map.keys new ++ cs)
  where
  reference = snd (known Map.! i)
  new = Map.mapMaybe (match reference) remain

match :: Set Coord3 -> [Coord3] -> Maybe (Coord3, Set Coord3)
match xset ys = listToMaybe
 [(offset, yset')
   | yset <- Set.fromList <$> reorient ys
   , offset <- diff <$> Set.toList xset <*> Set.toList yset
   , let yset' = Set.mapMonotonic (add offset) yset
   , 12 <= Set.size (Set.intersection xset yset')
 ]

reorient :: [Coord3] -> [[Coord3]]
reorient = transpose . map (rotations >=> faces)

faces :: Coord3 -> [Coord3]
faces (C3 x y z) =
  [
    C3 x y z,
    C3 y (-x) z,
    C3 (-x) (-y) z,
    C3 (-y) x z,
    C3 y z x,
    C3 y (-z) (-x)
  ]

-- | Return the 4 rotations of a point around the x-axis
rotations :: Coord3 -> [Coord3]
rotations (C3 x y z) =
  [
    C3 x y z,
    C3 x (-z) y,
    C3 x (-y) (-z),
    C3 x z (-y)
  ]
