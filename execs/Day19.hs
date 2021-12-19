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

data P = P !Int !Int !Int deriving (Eq, Ord, Show)

manhattan :: P -> P -> Int
manhattan (P x1 y1 z1) (P x2 y2 z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

diff :: P -> P -> P
diff (P x y z) (P x' y' z') = P (x-x') (y-y') (z-z')

add :: P -> P -> P
add  (P x y z) (P x' y' z') = P (x+x') (y+y') (z+z')

redirect :: [P] -> [[P]]
redirect = transpose . map (spins >=> orientations)

orientations :: P -> [P]
orientations (P x y z) =
  [
    P x y z,
    P y (-x) z,
    P (-x) (-y) z,
    P (-y) x z,
    P y z x,
    P y (-z) (-x)
  ]

spins :: P -> [P]
spins (P x y z) =
  [
    P x y z,
    P x (-z) y,
    P x (-y) (-z),
    P x z (-y)
  ]

main :: IO ()
main =
 do (i0,ps0):inp <- [format|19 (--- scanner %u ---%n(%d,%d,%d%n)*)&%n|]
    let toP (x,y,z) = P x y z
    let scanners = Map.fromList [(i, map toP xs) | (i, xs) <- inp]
    
    let answer =
           search scanners
                  (Map.singleton i0 (P 0 0 0, Set.fromList (map toP ps0)))
                  [0]
    print $ Set.size $ Set.unions $ map snd $ Map.elems answer
    print $ maximum [manhattan p q | p <- map fst $ Map.elems answer, q <- map fst $ Map.elems answer]


search :: Ord a => Map a [P] -> Map a (P, Set P) -> [a] -> Map a (P, Set P)
search remain known _
 | Map.null remain = known
search remain known (i:cs) =
  case
    foldl (\(r,k,c) (j,(o,m)) ->
        (Map.delete j r,
        Map.insert j (o,m) k,
        j:c)
      ) (remain, known, cs) zs of
    (r,k,c) -> search r k c
  where
  region1 = snd (known Map.! i)
  zs = [ (j, align)
          | (j, region2) <- Map.toList remain
          , align <- match region1 region2
      ]

match :: Set P -> [P] -> [(P, Set P)]
match xs ys = take 1
 [(offset, yset')
   | ys1 <- redirect ys
   , let yset = Set.fromList ys1
   , offset <- [diff x y | x <- Set.toList xs, y <- ys1]    
   , let yset' = Set.mapMonotonic (add offset) yset
   , 12 <= Set.size (Set.intersection xs yset')
 ]
