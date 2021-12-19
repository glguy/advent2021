{-# Language LambdaCase, BlockArguments, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/19>

-}
module Main (main) where

import Advent.Format (format)
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
 do (i0,ps0):inp <- [format|19 (--- scanner %u ---%n(%d,%d,%d%n)*)&%n|]
    let toP (x,y,z) = P x y z
    let scanners = Map.fromList [(i, map toP xs) | (i, xs) <- inp]
    
    let (offsets, locations) =
          unzip $ Map.elems $
           assemble scanners
                  (Map.singleton i0 (P 0 0 0, Set.fromList (map toP ps0)))
                  [i0]

    print (Set.size (Set.unions locations))
    print (maximum [manhattan p q | p <- offsets, q <- offsets])

assemble ::
  Ord a =>
  Map a [P]        {- ^ uncorrelated scanner readings -} ->
  Map a (P, Set P) {- ^ correlated scanner locations and readings -} ->
  [a]              {- ^ recently correlated scanners -} ->
  Map a (P, Set P)
assemble remain known _ | Map.null remain = known
assemble _ _ [] = error "bad input"
assemble remain known (i:cs) =
  assemble (Map.difference remain new) (Map.union known new) (Map.keys new ++ cs)
  where
  reference = snd (known Map.! i)
  new = Map.mapMaybe (match reference) remain

match :: Set P -> [P] -> Maybe (P, Set P)
match xset ys = listToMaybe
 [(offset, yset')
   | yset <- Set.fromList <$> reorient ys
   , offset <- [diff x y | x <- Set.toList xset, y <- Set.toList yset]    
   , let yset' = Set.mapMonotonic (add offset) yset
   , 12 <= Set.size (Set.intersection xset yset')
 ]

-- * 3D points

data P = P !Int !Int !Int deriving (Eq, Ord, Show)

manhattan :: P -> P -> Int
manhattan (P x1 y1 z1) (P x2 y2 z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

diff :: P -> P -> P
diff (P x y z) (P x' y' z') = P (x-x') (y-y') (z-z')

add :: P -> P -> P
add  (P x y z) (P x' y' z') = P (x+x') (y+y') (z+z')

reorient :: [P] -> [[P]]
reorient = transpose . map (rotations >=> faces)

faces :: P -> [P]
faces (P x y z) =
  [
    P x y z,
    P y (-x) z,
    P (-x) (-y) z,
    P (-y) x z,
    P y z x,
    P y (-z) (-x)
  ]

rotations :: P -> [P]
rotations (P x y z) =
  [
    P x y z,
    P x (-z) y,
    P x (-y) (-z),
    P x z (-y)
  ]
