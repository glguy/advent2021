{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost, TemplateHaskell, OverloadedLists #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/13>

Given a paper with some dots and a series of fold instructions
we fold and fold and fold and find our secret code.

-}
module Main (main) where

import Advent.Coord (Coord(C), drawCoords)
import Advent.Format (format)
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Map qualified as Map

data A = Ax | Ay

mempty -- template haskell staging

-- | >>> :main
-- 716
-- ███  ███   ██  █  █ ████ ███  █    ███ 
-- █  █ █  █ █  █ █ █  █    █  █ █    █  █
-- █  █ █  █ █    ██   ███  ███  █    █  █
-- ███  ███  █    █ █  █    █  █ █    ███ 
-- █ █  █    █  █ █ █  █    █  █ █    █ █ 
-- █  █ █     ██  █  █ █    ███  ████ █  █
main :: IO ()
main =
  do (points, folds) <- [format|13 (%u,%u%n)*%n(fold along @A=%u%n)*|]
     let states = scanl foldPoints (Set.fromList points) folds
         p1 = states !! 1 -- points after first instruction
         p2 = last states -- points after last instruction
     print (length p1)
     putStr (drawCoords (Map.fromList [(C y x, '█') | (x,y) <- Set.toList p2]))

foldPoints :: Set (Int, Int) -> (A, Int) -> Set (Int, Int)
foldPoints inp (Ax, lx) = Set.map (\(x,y) -> (foldAxis lx x, y)) inp
foldPoints inp (Ay, ly) = Set.map (\(x,y) -> (x, foldAxis ly y)) inp

foldAxis :: Int -> Int -> Int
foldAxis a i
  | i > a     = 2 * a - i
  | otherwise = i