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
import Advent.Search (dfsOn)
import Data.Char (isLower, isUpper)
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.List.NonEmpty (NonEmpty((:|)))

-- | >>> :main
-- 3761
-- 99138
main :: IO ()
main =
 do inp <- [format|12 (%a+-%a+%n)*|]
    let adj = fmap (delete "start") -- don't bother going back
            $ Map.fromListWith (++)
            $ concat [ [(a,[b]),(b,[a])] | (a,b) <- inp]
    let go e = print (length [() | ("end" :| _, _) <- solve e adj])
    go False
    go True

solve :: Bool -> Map String [String] -> [(NonEmpty String, Bool)]
solve extra paths = dfsOn fst (step paths) ("start" :| [], extra)

step :: Map String [String] -> (NonEmpty String, Bool) -> [(NonEmpty String, Bool)]
step paths (here  :| xs, extra) =
  [ (z :| here : xs, extra')
  | "end" /= here
  , z <- paths Map.! here
  , isUpper (head z) || extra || notElem z xs
  , let extra' = if extra && isLower (head z) && z `elem` xs then False else extra
  ]
