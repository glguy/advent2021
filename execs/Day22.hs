{-# Language ParallelListComp, BlockArguments, TemplateHaskell, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/22>

-}
module Main (main) where

import Advent.Format (format)
import Data.List ( foldl', nub, sort )
import Data.Maybe

data C = Con | Coff
  deriving (Show, Eq, Ord)

mempty -- template haskell staging

data Cube = Cube !Seg !Seg !Seg deriving (Eq, Ord, Show)

data Seg = Seg !Int !Int deriving (Eq, Ord, Show)

-- | Determine if two cubes have some overlap
intersect :: Cube -> Cube -> Maybe Cube
intersect (Cube x1 y1 z1) (Cube x2 y2 z2) =
  Cube <$> intersectSeg x1 x2 <*> intersectSeg y1 y2 <*> intersectSeg z1 z2

-- | Determine if two segments have some overlap
intersectSeg :: Seg -> Seg -> Maybe Seg
intersectSeg (Seg alo ahi) (Seg blo bhi)
  | lo < hi = Just (Seg lo hi)
  | otherwise = Nothing
  where
    lo = max alo blo
    hi = min ahi bhi

-- | >>> :main
-- 606484
-- 1162571910364852
main :: IO ()
main =
 do inp <- [format|22 (@C x=%d..%d,y=%d..%d,z=%d..%d%n)*|]
    let steps = [(c, Cube (Seg x1 (x2+1)) (Seg y1 (y2+1)) (Seg z1 (z2+1))) | (c,x1,x2,y1,y2,z1,z2) <- inp]
        p1seg = Seg (-50) 51
        p1cube = Cube p1seg p1seg p1seg
    print $ solve [(cmd, cube') | (cmd, cube) <- steps, Just cube' <- [intersect p1cube cube]]
    print (solve steps)

-- | Figure out how many lights the given instructions turn on.
solve :: [(C, Cube)] -> Int
solve [] = 0
solve ((Coff,_):xs) = solve xs
solve ((Con,c):xs) = sum (map volume c') + solve xs
  where
    c' = foldl' (\acc y -> subcubes y =<< acc) [c] (map snd xs)

-- | Compute the volume of a cube
volume :: Cube -> Int
volume (Cube x y z) = len x * len y * len z

-- | Compute the length of a segment
len :: Seg -> Int
len (Seg lo hi) = hi - lo

-- | Return all the cubes that are in the second
-- argument and not in the first argument.
subcubes :: Cube -> Cube -> [Cube]
subcubes c1@(Cube x1 y1 z1) c2@(Cube x2 y2 z2)
  | isNothing (intersect c1 c2) = [c2]
  | otherwise =
    [ Cube x y z
      | (inx, x) <- segs x1 x2
      , (iny, y) <- segs y1 y2
      , (inz, z) <- segs z1 z2
      , not (inx && iny && inz)
    ]
  where
    segs (Seg a b) (Seg c d) =
      let xs = nub (sort ([a | c <= a, a < d] ++ [b | c <= b, b < d] ++ [c,d]))
      in [(a <= lo && lo < b, Seg lo hi) | lo <- xs | hi <- tail xs]
