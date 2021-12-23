{-# Language BlockArguments, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/23>

-}
module Main (main) where

import Advent.Input ( getInputMap )
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Advent.Search (AStep(AStep), dfs, astar)
import Advent.Coord

main :: IO ()
main =
 do inp <- getInputMap 23
    let Just (_, cost) = find (done . fst) (astar step inp)
    print cost

isHallway :: Coord -> Bool
isHallway c = coordRow c == 1 && not (isRoom c)

isRoom :: Coord -> Bool
isRoom (C _ c) = c == 3 || c == 5 || c == 7 || c == 9

step :: Map Coord Char -> [AStep (Map Coord Char)]
step w =
  [ AStep w' (manhattan c dest * tokCost tok) 0
  | c <- [c | (c, tok) <- Map.toList w, tok `elem` "ABCD."]
  , let tok = w Map.! c
  , tok /= '.'
  , dest <- route w c
    
  , if isRoom c
      then isHallway dest
      else isRoom dest
        && coordCol dest == roomCol tok
        && roomClean w tok
        && (w Map.! below dest) `elem` [tok, '#']

  , let w' = Map.insert c '.'
           $ Map.insert dest tok w
  ]

roomClean :: Map Coord Char -> Char -> Bool
roomClean w tok = all (`elem` [tok,'.'])
                $ takeWhile ('#' /=) [w Map.! C r (roomCol tok) | r <- [2..]]

done :: Map Coord Char -> Bool
done w = all (\tok ->
  all (tok ==)
  $ takeWhile ('#' /=) [w Map.! C r (roomCol tok) | r <- [2..]]
  ) "ABCD"

roomCol :: Char -> Int
roomCol 'A' = 3
roomCol 'B' = 5
roomCol 'C' = 7
roomCol 'D' = 9

rooms :: Char -> [Coord]
rooms 'A' = [C 2 3, C 3 3, C 4 3, C 5 3]
rooms 'B' = [C 2 5, C 3 5, C 4 5, C 5 5]
rooms 'C' = [C 2 7, C 3 7, C 4 7, C 5 7]
rooms 'D' = [C 2 9, C 3 9, C 4 9, C 5 9]

tokCost :: Char -> Int
tokCost 'A' = 1
tokCost 'B' = 10
tokCost 'C' = 100
tokCost 'D' = 1000

route :: Map Coord Char -> Coord -> [Coord]
route w = dfs move
  where
    move c = [c' | c' <- cardinal c, w Map.! c' == '.']
