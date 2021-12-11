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

import Advent (frequencies)
import Advent.Format (format)
import Data.Map (Map)
import Data.Map qualified as Map

-- | >>> :main
-- 376194
-- 1693022481538
main :: IO ()
main =
  do inp <- frequencies <$> [format|6 %u&,%n|]
     print (sum (iterate step inp !! 80))
     print (sum (iterate step inp !! 256))

step :: Map Int Int -> Map Int Int 
step xs = Map.fromListWith (+) (tick =<< Map.toList xs)
  where
    tick (0,n) = [(6,n), (8,n)]
    tick (d,n) = [(d-1,n)]
