{-# Language ParallelListComp, BlockArguments, TemplateHaskell, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/22>

This problem is made simple by processing commands
by subtracting away all future cuboids. Only the region
unique to the current command will affect the final output.

-}
module Main (main) where

import Advent.Format (format)
import Data.Maybe (isNothing)

-- | On and off commands from the input file
data C = Con | Coff
  deriving (Show, Eq, Ord)

mempty -- template haskell staging

-- | >>> :main
-- 606484
-- 1162571910364852
main :: IO ()
main =
 do inp <- [format|22 (@C x=%d..%d,y=%d..%d,z=%d..%d%n)*|]
    let seg lo hi = Seg lo (hi+1) -- make upper limit exclusive
        steps = [ (c, Cuboid (seg x1 x2) (seg y1 y2) (seg z1 z2))
                | (c,x1,x2,y1,y2,z1,z2) <- inp]
        p1seg = seg (-50) 50
        p1cube = Cuboid p1seg p1seg p1seg
    print $ solve [(cmd, cube') | (cmd, cube) <- steps, Just cube' <- [intersect p1cube cube]]
    print (solve steps)

-- | Figure out how many lights the given instructions turn on.
solve :: [(C, Cuboid)] -> Int
solve [] = 0
solve ((Coff,_):xs) = solve xs
solve ((Con,c):xs) = sum (map volume c') + solve xs
  where
    c' = foldl (\acc y -> subcubes y =<< acc) [c] (map snd xs)

-- * Cuboids

-- | A cuboid is defined by segments on the three axises.
data Cuboid = Cuboid !Seg !Seg !Seg deriving (Eq, Ord, Show)

-- | Find the overlap between two cuboids.
intersect :: Cuboid -> Cuboid -> Maybe Cuboid
intersect (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) =
  Cuboid <$> intersectSeg x1 x2 <*> intersectSeg y1 y2 <*> intersectSeg z1 z2

-- | Compute the volume of a cube
volume :: Cuboid -> Int
volume (Cuboid x y z) = len x * len y * len z

-- | Return all the cubes that are in the second
-- argument and not in the first argument.
subcubes :: Cuboid -> Cuboid -> [Cuboid]
subcubes c1@(Cuboid x1 y1 z1) c2@(Cuboid x2 y2 z2)
  | isNothing (intersect c1 c2) = [c2]
  | otherwise =
    [ Cuboid x y z
      | (inx, x) <- segs x1 x2
      , (iny, y) <- segs y1 y2
      , (inz, z) <- segs z1 z2
      , not (inx && iny && inz)
    ]
  where
    segs (Seg a b) (Seg c d) =
      let xs = [c] ++ [a | c < a, a < d] ++ [b | c < b, b < d] ++ [d]
      in [(a <= lo && lo < b, Seg lo hi) | lo <- xs | hi <- tail xs]

-- * Segments

-- | A segment defined by an inclusive lower-bound and an exclusive upper-bound.
data Seg = Seg !Int !Int deriving (Eq, Ord, Show)

-- | Compute the length of a segment
len :: Seg -> Int
len (Seg lo hi) = hi - lo

-- | Determine if two segments have some overlap
intersectSeg :: Seg -> Seg -> Maybe Seg
intersectSeg (Seg alo ahi) (Seg blo bhi)
  | lo < hi = Just (Seg lo hi)
  | otherwise = Nothing
  where
    lo = max alo blo
    hi = min ahi bhi
