{-# Language LambdaCase, BlockArguments, ImportQualifiedPost, NumericUnderscores, BinaryLiterals, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/20>

-}
module Main (main) where

import Advent (ordNub, times, fromDigits, format)
import Advent.Coord(Coord(..), coordLines)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Set (Set)
import Data.Set qualified as Set

-- | Pictures have the boolean value at all of the coordinates in
-- the set, and have the complement value at any other coordinate.
data Picture = Picture !Bool !(Set Coord)

main :: IO ()
main =
  do (algStr, imgStrs) <- [format|20 %s%n%n(%s%n)*|]
     let alg = IntSet.fromList [i | (i,'#') <- zip [0..] algStr]
     let img = Picture True (Set.fromList [c | (c, '#') <- coordLines imgStrs])

     let Picture True out1 = times 2 (step alg) img
     print (length out1)

     let Picture True out2 = times 50 (step alg) img
     print (length out2)

-- | Apply the given image enhancement algorithm to a picture
step :: IntSet -> Picture -> Picture
step alg (Picture val img) = Picture val' img'
  where
    val'
      | val       = IntSet.member 0b111_111_111 alg
      | otherwise = IntSet.member 0b000_000_000 alg

    img' = Set.fromList
      [  x
        | x <- ordNub (concatMap cell img)
        , let n = fromDigits 2 [if Set.member i img == val then 1 else 0  | i <- cell x]
        , IntSet.member n alg == val'
      ]

-- | 3x3 neighborhood around a coordinate
cell :: Coord -> [Coord]
cell (C y x) = C <$> [y-1 .. y+1] <*> [x-1 .. x+1]
