{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/12>

Search around a cave visiting some caves more than others.

-}
module Main (main) where

import Advent.Format (format)
import Data.Char (isUpper)
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | >>> :main
-- 3761
-- 99138
main :: IO ()
main =
 do inp <- toAdj <$> [format|12 (%a+-%a+%n)*|]
    print (start False inp)
    print (start True inp)

toAdj :: [(String,String)] -> Map String [String]
toAdj inp =
  delete "start" <$> -- don't bother going back
  Map.fromListWith (++) [entry | (a,b) <- inp, entry <- [(a,[b]),(b,[a])]]

start :: Bool -> Map String [String] -> Int
start extra paths = step paths extra Set.empty "start"

step :: Map String [String] -> Bool -> Set String -> String -> Int
step paths extra seen here = sum (map f (paths Map.! here))
  where
    f next
      | next == "end"           = 1
      | isUpper (head next)     = step paths extra seen                   next
      | Set.notMember next seen = step paths extra (Set.insert next seen) next
      | extra                   = step paths False seen                   next
      | otherwise               = 0
