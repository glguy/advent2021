{-# Language LambdaCase, BlockArguments, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/19>

-}
module Main (main) where

import Advent.Format (format)
import Control.Monad
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace

main :: IO ()
main =
 do inp <- Map.fromList <$> [format|19 (--- scanner %u ---%n(%d,%d,%d%n)*)&%n|]
    
    let (answer, offsets) =
           search (Map.delete 0 inp)
                  (Map.singleton 0 (Set.fromList (inp Map.! 0)))
                  (Map.singleton 0 (0,0,0))
    print answer
    print $ Set.size $ Set.unions $ Map.elems answer
    print $ maximum [abs (x-x') + abs (y-y') + abs (z-z') | (x,y,z) <- Map.elems offsets, (x' ,y',z') <- Map.elems offsets]

    -- print inp

-- search :: Map Int [(Int,Int,Int)] -> Map Int (Set (Int,Int,Int)) -> _
--search seen [] = seen



search remain known offset
 | Map.null remain = (known, offset)
 | otherwise =
  case zs of
    
    (i,j, (o,m)):_ -> traceShow j $
       search (Map.delete j remain) (Map.insert j m known )
        (Map.insert j (o) offset)

  where
  zs = [ (i,j, y2)
          | (i, region1) <- Map.toList known
          , (j, region2) <- Map.toList remain
          , region2' <- redirect region2
          , Just y2 <- [match region1 region2']
      ]

redirect :: [(Int,Int,Int)] -> [[(Int,Int,Int)]]
redirect = transpose . map (spins >=> orientations)

orientations :: (Int,Int,Int) -> [(Int,Int,Int)]
orientations (x,y,z) =
  [
    (x,y,z),
    (y,-x,z),
    (-x,-y,z),
    (-y,x,z),
    (y,z,x),
    (y,-z,-x)
  ]

spins :: (Int,Int,Int) -> [(Int,Int,Int)]
spins (x,y,z) =
  [
    (x,y,z),
    (x,-z,y),
    (x,-y,-z),
    (x,z,-y)
  ]

diff (x,y,z) (x',y',z') = (x-x',y-y',z-z')
add (x,y,z) (x',y',z') = (x+x',y+y',z+z')

-- match ::  Set (Int,Int,Int) ->  [(Int,Int,Int)] -> Maybe (Set (Int,Int,Int))
match xs ys = listToMaybe
 do offset <- [diff x y | x <- Set.toList xs, y <- ys]
    let ys' = Set.fromList (map (add offset) ys)
    let both = Set.intersection xs ys'
    if 12 <= Set.size both
      then [(offset, ys')]
      else []
