{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/23>

Search for the cheapest way to rearrange lizards in a maze
to get all the lizards back into the correct rooms.

-}
module Main (main) where

import Advent.Coord
    ( Coord(..), coordCol, below, manhattan, cardinal )
import Advent.Input ( getInputMap )
import Advent.Search (AStep(AStep), dfs, astar)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map

main :: IO ()
main =
 do inp <- Map.filter (`elem` ".ABCD") <$> getInputMap 23
    print (head [cost | (w, cost) <- astar step inp, done w])

isRoom :: Coord -> Bool
isRoom (C _ c) = c == 3 || c == 5 || c == 7 || c == 9

step :: Map Coord Char -> [AStep (Map Coord Char)]
step w =
  [ AStep w' (manhattan c dest * tokCost tok) 0
  | (c, tok) <- Map.toList w
  , tok `elem` "ABCD"
  , dest <- route w c
    
  , if isRoom c
      then not (isRoom dest)
        && not (roomClean w (coordCol c) (target (coordCol c)))
      else isRoom dest
        && coordCol dest == roomCol tok
        && roomClean w (roomCol tok) tok
        && Map.findWithDefault tok (below dest) w == tok

  , let w' = Map.insert c '.'
           $ Map.insert dest tok w
  ]

target :: Int -> Char
target 3 = 'A'
target 5 = 'B'
target 7 = 'C'
target 9 = 'D'

roomClean :: Map Coord Char -> Int -> Char -> Bool
roomClean w c tok = roomCheck w c (`elem` [tok,'.'])

done :: Map Coord Char -> Bool
done w = all (\tok -> roomCheck w (roomCol tok) (tok ==)) "ABCD"

roomCheck :: Map Coord Char -> Int -> (Char -> Bool) -> Bool
roomCheck w c p = go 2
  where
    go r =
      case Map.lookup (C r c) w of
        Nothing -> True
        Just x -> p x && go (r+1)

roomCol :: Char -> Int
roomCol 'A' = 3
roomCol 'B' = 5
roomCol 'C' = 7
roomCol 'D' = 9

tokCost :: Char -> Int
tokCost 'A' = 1
tokCost 'B' = 10
tokCost 'C' = 100
tokCost 'D' = 1000

route :: Map Coord Char -> Coord -> [Coord]
route w = dfs move
  where
    move c = [c' | c' <- cardinal c, Map.lookup c' w == Just '.']
