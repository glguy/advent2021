{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/9>

Find the basins on the height map.

-}
module Main (main) where
import Advent (cardinality)
import Advent.Coord (Coord(..), cardinal)
import Advent.Format (format)
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map

-- >>> :main
-- 588
-- 964712
main :: IO ()
main =
  do  inp <- [format|9 (%s%n)*|]
      let m = Map.fromList
                [ (C y x, digitToInt z)
                | (y,xs) <- zip [0..] inp
                , (x,z ) <- zip [0..] xs
                , z /= '9']

      print (sum [1+h | (c,h) <- Map.toList m, null (downs m c h)])

      let basinId = Map.mapWithKey part2 m
          part2 c h =
            do xs <- traverse (basinId Map.!) (downs m c h)
               case xs of
                 [] -> Just c
                 y:ys | all (y==) ys -> Just y
                      | otherwise    -> Nothing

      let top n = take n . reverse . sort
      print $ product $ top 3
            $ Map.elems $ cardinality $ Map.elems $ Map.mapMaybe id basinId

downs :: Map Coord Int -> Coord -> Int -> [Coord]
downs m c h = [x | x <- cardinal c, Map.findWithDefault 9 x m < h]
