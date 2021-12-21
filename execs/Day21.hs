{-# Language BlockArguments, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/21>

-}
module Main (main) where

import Advent (format, counts)
import Advent.Coord
import Control.Monad ( replicateM )
import Data.List (foldl')
import Data.Map qualified as Map
import Data.MemoTrie (memo3, mup, HasTrie)

-- | >>> :main
-- 428736
-- 57328067654557
main :: IO ()
main =
 do (p1,p2) <- [format|21 Player 1 starting position: %u%nPlayer 2 starting position: %u%n|]
    print (part1 0 1 p1 p2 0 0)
    print (let C y x = part2 p1 p2 0 0 in max y x)

wrap :: Int -> Int -> Int
wrap x n = (x - 1) `mod` n + 1

part1 :: Int -> Int -> Int -> Int -> Int -> Int -> Int
part1 rolls die1 p1 p2 p1s p2s
  | p1s' >= 1000 = rolls' * p2s
  | otherwise = part1 rolls' die' p2 p1' p2s p1s'
  where
        rolls' = rolls + 3
        die2 = wrap (die1 + 1) 100
        die3 = wrap (die2 + 1) 100
        die' = wrap (die3 + 1) 100
        move = die1 + die2 + die3
        p1'  = wrap (p1 + move) 10
        p1s' = p1s + p1'

part2 :: Int -> Int -> Int -> Int -> Coord
part2 = memo4 \p1 p2 p1s p2s ->
    foldl' addCoord origin
    [ scaleCoord n
    $ if p1s' >= 21
        then south
        else invert (part2 p2 p1' p2s p1s')
      | (move, n) <- somerolls
      , let p1' = wrap (p1 + move) 10
      , let p1s' = p1s + p1'
    ]
  where
    somerolls = Map.toList (counts (sum <$> replicateM 3 [1,2,3]))

memo4 ::
  (HasTrie a, HasTrie b, HasTrie c, HasTrie d) =>
  (a -> b -> c -> d -> e) ->
  (a -> b -> c -> d -> e)
memo4 = mup memo3
