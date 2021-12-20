{-# Language LambdaCase, BlockArguments, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/20>

-}
module Main (main) where

import Advent
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Advent.Coord

main :: IO ()
main =
  do (alg, img) <- [format|20 %s%n%n(%s%n)*|]
     let alg1 = Set.fromList [i | (i,'#') <- zip [0..] alg]
     let img1 = Map.keysSet (Map.filter ('#'==) (Map.fromList $ coordLines img))

     let Picture True out1 = times 2 (step alg1) (Picture True img1)
     print $ length out1
     let Picture True out2 = times 50 (step alg1) (Picture True img1)
     print $ length out2


step :: Set Int -> Picture -> Picture
step alg (Picture def img) = Picture def' img'
  where
    img' = Set.fromList
      [  x
        | x <- ordNub (concatMap cell (Set.toList img))
        , let n = fromDigits 2
                   [if (if Set.member i img then def else not def) then 1 else 0  | i <- cell x]
                   , Set.member n alg == def'
      ]
    def'
      | def = Set.member 511 alg
      | otherwise = Set.member 0 alg

cell :: Coord -> [Coord]
cell (C y x) = C <$> [y-1 .. y+1] <*> [x-1 .. x+1]


data Picture = Picture !Bool !(Set Coord)
