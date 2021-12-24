{-# Language BangPatterns #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/24>

-}
module Main (main) where

import Advent (getInputLines, chunks)
import Data.Char (intToDigit)

main :: IO ()
main =
 do inp <- map extract . chunks 18 . map words <$> getInputLines 24
    let blocks = [(impl a b c, b) | (a,b,c) <- inp]
    putStrLn $ map intToDigit $ head $ solve [9,8..1] 0 blocks
    putStrLn $ map intToDigit $ head $ solve [1,2..9] 0 blocks

solve :: [Int] -> Int -> [(Int -> Int -> Int, Int)] -> [[Int]]
solve guesses !z ((f,b):bs) =
  [i:is
    | i <- if b < 0 then [w | let w = z`mod`26 + b, 1 <= w, w <= 9] else guesses
    , is <- solve guesses (f i z) bs
  ]
solve _ z [] = [[] | z == 0]
     
extract :: [[String]] -> (Int, Int, Int)
extract [
    ["inp","w"],
    ["mul","x","0"],
    ["add","x","z"],
    ["mod","x","26"],
    ["div","z",a],
    ["add","x",b],
    ["eql","x","w"],
    ["eql","x","0"],
    ["mul","y","0"],
    ["add","y","25"],
    ["mul","y","x"],
    ["add","y","1"],
    ["mul","z","y"],
    ["mul","y","0"],
    ["add","y","w"],
    ["add","y",c],
    ["mul","y","x"],
    ["add","z","y"]] =
    (read a, read b, read c)
extract x = error (show x)

impl :: Int -> Int -> Int -> Int -> Int -> Int
impl a b c w z
  | z`mod`26 + b == w = z `div` a
  | otherwise         = z `div` a * 26 + w + c
