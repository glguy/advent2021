{-# Language QuasiQuotes, BlockArguments #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/13>

Given a paper with some dots and a series of fold instructions
we fold and fold and fold and find our secret code.

-}
module Main (main) where

import Advent (ordNub)
import Advent.Format (format)
import Data.Foldable (for_)

main :: IO ()
main =
  do (dots, folds) <- [format|13 (%u,%u%n)*%n(fold along %c=%u%n)*|]
     let dots' = scanl foldup dots folds
         p1 = dots' !! 1 -- points after first instruction
         p2 = last dots' -- points after last instruction
         ymax = maximum (snd <$> p2)
         xmax = maximum (fst <$> p2)
     print (length p1)
     for_ [0..ymax] \y ->   
        putStrLn [if (x,y) `elem` p2 then '█' else '▒' | x <- [0..xmax]]

foldup :: [(Int, Int)] -> (Char, Int) -> [(Int, Int)]
foldup inp ('x',lx) = ordNub [(foldAxis lx x, y) | (x,y) <- inp]
foldup inp ('y',ly) = ordNub [(x, foldAxis ly y) | (x,y) <- inp]
foldup _   _        = error "bad fold axis"

foldAxis :: Int -> Int -> Int
foldAxis a i
  | i > a     = 2 * a - i
  | otherwise = i