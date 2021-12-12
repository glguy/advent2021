{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/6>

Multiplying fish!

-}
module Main (main) where

import Advent (counts)
import Advent.Format (format)
import Data.Map (Map)
import Data.Map qualified as Map

-- | >>> :main
-- 376194
-- 1693022481538
main :: IO ()
main =
  do inp <- counts <$> [format|6 %u&,%n|]
     let generations = sum <$> iterate step inp
     print (generations !! 80)
     print (generations !! 256)

-- | Advance the simulation one day.
--
-- >>> step (Map.fromList [(0,1),(1,2),(2,1),(6,1),(8,1)])
-- fromList [(0,2),(1,1),(5,1),(6,1),(7,1),(8,1)]
step :: Map Int Int -> Map Int Int
step xs = Map.fromListWith (+) (tick =<< Map.toList xs)
  where
    tick (0,n) = [(6,n), (8,n)]
    tick (d,n) = [(d-1,n)]
