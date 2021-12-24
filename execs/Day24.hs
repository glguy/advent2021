{-# Language BangPatterns, ViewPatterns #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/24>

-}
module Main (main) where

import Advent (getInputLines, chunks, fromDigits)
import Data.Char (intToDigit)
import Text.Read (readMaybe)

main :: IO ()
main =
 do inp <- map extract . chunks 18 . map words <$> getInputLines 24
    print $ fromDigits 10 $ head $ solve [9,8..1] 0 inp
    print $ fromDigits 10 $ head $ solve [1,2..9] 0 inp

solve :: [Int] -> Int -> [(Int, Int, Int)] -> [[Int]]
solve guesses !z ((a,b,c):bs) =
  [i:is
    | i <- if b < 0 then [w | let w = z`mod`26 + b, 1 <= w, w <= 9] else guesses
    , is <- solve guesses (impl a b c i z) bs
  ]
solve _ z [] = [[] | z == 0]
     
extract :: [[String]] -> (Int, Int, Int)
extract [
  ["inp", "w"      ],
  ["mul", "x", "0" ],
  ["add", "x", "z" ],
  ["mod", "x", "26"],
  ["div", "z", readMaybe -> Just a],
  ["add", "x", readMaybe -> Just b],
  ["eql", "x", "w" ],
  ["eql", "x", "0" ],
  ["mul", "y", "0" ],
  ["add", "y", "25"],
  ["mul", "y", "x" ],
  ["add", "y", "1" ],
  ["mul", "z", "y" ],
  ["mul", "y", "0" ],
  ["add", "y", "w" ],
  ["add", "y", readMaybe -> Just c],
  ["mul", "y", "x" ],
  ["add", "z", "y" ]] =
  (a, b, c)
extract x = error (show x)

impl :: Int -> Int -> Int -> Int -> Int -> Int
impl a b c w z
  | z`mod`26 + b == w = z `div` a
  | otherwise         = z `div` a * 26 + w + c
