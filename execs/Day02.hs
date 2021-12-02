{-# Language QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/2>

Implement a simple submarine piloting/aiming command interpreter.

-}
module Main (main) where

import Advent.Format (format)

data C = Cforward | Cdown | Cup

pure [] -- puts C into view of format's reify below

main :: IO ()
main =
  do inp <- [format|2 (@C %u%n)*|]
     print (let (y, x   ) = foldl part1 (0,0  ) inp in y * x)
     print (let (y, x, _) = foldl part2 (0,0,0) inp in y * x)

part1 :: (Int, Int) -> (C, Int) -> (Int, Int)
part1 (y, x) (c, i) =
  case c of
    Cforward -> (y,   x+i)
    Cup      -> (y-i, x  )
    Cdown    -> (y+i, x  )

part2 :: (Int, Int, Int) -> (C, Int) -> (Int, Int, Int)
part2 (y, x, a) (c, i) =
  case c of
    Cforward -> (y+i*a, x+i, a  )
    Cup      -> (y,     x,   a-i)
    Cdown    -> (y,     x,   a+i)