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
import Advent.Coord (Coord(..), invert, scaleCoord)
import Control.Monad (replicateM)
import Data.Map qualified as Map
import Data.MemoTrie (HasTrie, memo3, mup)

-- | >>> :main
-- 428736
-- 57328067654557
main :: IO ()
main =
 do (p1,p2) <- [format|21 Player 1 starting position: %u%nPlayer 2 starting position: %u%n|]
    print (part1 0 p1 p2 0 0)
    print (let C y x = part2 p1 p2 0 0 in max y x)

-- | Compute the @die rolls * losing score@ once one player
-- wins with 1000 points.
part1 ::
  Int {- ^ turn counter -} ->
  Int {- ^ player 1 location -} ->
  Int {- ^ player 2 location -} ->
  Int {- ^ player 1 score    -} ->
  Int {- ^ player 2 score    -} ->
  Int {- ^ player 2 score * 3 * turns -}
part1 turns p1 p2 p1s p2s
  | p1s' >= 1000 = 3 * turns' * p2s
  | otherwise    = part1 turns' p2 p1' p2s p1s'
  where
        turns' = turns + 1
        p1'    = wrap (p1 + 6 - turns) 10
        p1s'   = p1s + p1'

-- | Compute the possible ways a the players can win while playing with
-- a 3-sided dice given some starting conditions.
part2 ::
  Int   {- ^ player 1 location -} ->
  Int   {- ^ player 2 location -} ->
  Int   {- ^ player 1 score    -} ->
  Int   {- ^ player 2 score    -} ->
  Coord {- ^ vector of p1 wins and p2 wins -}
part2 = memo4 \p1 p2 p1s p2s ->
    sum
    [ if p1s' >= 21
        then C n 0
        else scaleCoord n (invert (part2 p2 p1' p2s p1s'))
      | (move, n) <- somerolls
      , let p1' = wrap (p1 + move) 10
      , let p1s' = p1s + p1'
    ]
  where
    somerolls = Map.toList (counts (sum <$> replicateM 3 [1,2,3]))

-- * Helpers

-- | Wrap number between @1@ and an inclusive upper bound
wrap :: Int {- ^ value -} -> Int {- ^ bound -} -> Int
wrap x n = (x - 1) `mod` n + 1

-- | Memoize a quaternary function on successive arguments.
-- Take care to exploit any partial evaluation.
memo4 ::
  (HasTrie a, HasTrie b, HasTrie c, HasTrie d) =>
  (a -> b -> c -> d -> e) ->
  (a -> b -> c -> d -> e)
memo4 = mup memo3
