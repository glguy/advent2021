{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/10>

-}
module Main (main) where

import Advent (getInputLines)
import Data.Either (partitionEithers)
import Data.List (sort)

-- | >>> :main
-- 392043
-- 1605968119
main :: IO ()
main =
  do inp <- getInputLines 10
     let (p1, p2) = partitionEithers (validate [] <$> inp)
     print (sum (map cost1 p1))
     print (middle (map cost2 p2))

-- | Return the median of an odd-length list
middle :: Ord a => [a] -> a
middle xs = sort xs !! (length xs `div` 2)

validate :: String -> String -> Either Char String
validate (x:xs) (y:ys) | x == y                      = validate xs ys
validate xs     (y:ys) | Just x <- lookup y brackets = validate (x:xs) ys
validate _      (y:_ )                               = Left y
validate xs     []                                   = Right xs

brackets :: [(Char,Char)]
brackets = [('(',')'),('[',']'),('{','}'),('<','>')]

cost1 :: Char -> Int
cost1 ')' = 3
cost1 ']' = 57
cost1 '}' = 1197
cost1 '>' = 25137
cost1 x   = error ("cost1: bad input " ++ show x)

cost2 :: String -> Int
cost2 = foldl (\acc x -> acc * 5 + f x) 0
  where
    f ')' =  1
    f ']' =  2
    f '}' =  3
    f '>' =  4
    f x   = error ("cost2: bad input " ++ show x)
