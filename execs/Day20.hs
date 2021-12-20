{-# Language LambdaCase, BlockArguments, ImportQualifiedPost, NumericUnderscores, BinaryLiterals, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/20>

This problem has us implement a cellular automaton on an
infinite grid. The problem requires special treatment
so that we can represent updates to infinite space.

-}
module Main (main) where

import Advent (format, fromDigits, times)
import Advent.Coord(Coord(..), coordLines)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Set (Set)
import Data.Set qualified as Set

-- | Pictures have the boolean value at all coordinates with the
-- exception of those listed in the set.
data Picture = Picture { _background :: !Bool, exceptions :: !(Set Coord) }

-- | >>> :main
-- 5179
-- 16112
main :: IO ()
main =
 do (algStr, imgStrs) <- [format|20 %s%n%n(%s%n)*|]
    let alg = IntSet.fromList [i | (i,'#') <- zip [0..] algStr]
    let pic = Picture False (Set.fromList [c | (c, '#') <- coordLines imgStrs])

    print (length (exceptions (times  2 (step alg) pic)))
    print (length (exceptions (times 50 (step alg) pic)))

-- | Apply the given image enhancement algorithm to a picture
step :: IntSet -> Picture -> Picture
step alg (Picture bg img) = Picture bg' img'
  where
    bg' = IntSet.member (if bg then 0b111_111_111 else 0b000_000_000) alg

    img' = Set.filter except (Set.fromList (concatMap cell img))

    except x = IntSet.member n alg /= bg'
      where
          n = fromDigits 2 [if Set.member i img /= bg then 1 else 0 | i <- cell x]

-- | 3x3 neighborhood around a coordinate
cell :: Coord -> [Coord]
cell (C y x) = C <$> [y-1 .. y+1] <*> [x-1 .. x+1]
