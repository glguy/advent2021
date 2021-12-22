{-# Language LambdaCase, BlockArguments, TemplateHaskell, ImportQualifiedPost, QuasiQuotes #-}
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

data C = Con | Coff 
  deriving (Show, Eq, Ord)

mempty -- template haskell staging

data Cube = Cube !Seg !Seg !Seg deriving (Eq, Ord, Show)

data Seg = Seg !Int !Int deriving (Eq, Ord, Show)

-- | Determine if two cubes have some overlap
overlap :: Cube -> Cube -> Bool
overlap (Cube x1 y1 z1) (Cube x2 y2 z2) =
  overseg x1 x2 && overseg y1 y2 && overseg z1 z2

-- | Determine if two segments have some overlap
overseg :: Seg -> Seg -> Bool
overseg (Seg alo ahi) (Seg blo bhi) = max alo blo < min ahi bhi

-- | >>> :main
-- 606484
-- 1162571910364852
main :: IO ()
main =
 do inp <- [format|22 (@C x=%d..%d,y=%d..%d,z=%d..%d%n)*|]
    let steps = [(c, Cube (Seg x1 (x2+1)) (Seg y1 (y2+1)) (Seg z1 (z2+1))) | (c,x1,x2,y1,y2,z1,z2) <- inp]
    print $ solve [(cmd, cube') | (cmd, cube) <- steps, Just cube' <- [truncateCube cube]]
    print $ solve steps

-- | Truncate a cube to fit in part 1's narrow window
truncateCube :: Cube -> Maybe Cube
truncateCube (Cube x y z) =
  Cube <$> truncateSeg x <*> truncateSeg y <*> truncateSeg z

-- | Helper for 'truncateCube'
truncateSeg :: Seg -> Maybe Seg
truncateSeg (Seg lo hi)
  | a < b = Just (Seg a b)
  | otherwise = Nothing
  where
    a = max (-50) lo
    b = min 51 hi

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
  | not (overlap c1 c2) = [c2]
  | otherwise = 
    [ Cube x y z
      | x <- segs x1 x2
      , y <- segs y1 y2
      , z <- segs z1 z2
      , let c = Cube x y z
      , not (overlap c1 c)
      , overlap c2 c
    ]
  where    
    segs (Seg a b) (Seg c d) =
      let xs = nub (sort [a,b,c,d])
      in zipWith Seg xs (tail xs)
