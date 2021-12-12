{-# Language ImportQualifiedPost, QuasiQuotes, MultiWayIf #-}
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
    print (solve False inp)
    print (solve True inp)

toAdj :: [(String,String)] -> Map String [String]
toAdj inp =
  delete "start" <$> -- don't bother going back
  Map.fromListWith (++) [entry | (a,b) <- inp, entry <- [(a,[b]),(b,[a])]]

solve :: Bool -> Map String [String] -> Int
solve extra paths = length (step paths extra Set.empty "start")

step :: Map String [String] -> Bool -> Set String -> String -> [()]
step _ _ _ "end" = [()]
step paths extra seen here =
  do next <- paths Map.! here
     (extra', seen') <-
        if | isUpper (head next)     -> [(extra, seen)]
           | Set.notMember next seen -> [(extra, Set.insert next seen)]
           | extra                   -> [(False, seen)]
           | otherwise               -> []
     step paths extra' seen' next                    
