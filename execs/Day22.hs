{-# Language KindSignatures, GADTs, DataKinds, ParallelListComp, BlockArguments, TemplateHaskell, ImportQualifiedPost, QuasiQuotes #-}
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
import Control.Monad.Trans.Writer.CPS (runWriterT, writerT, WriterT)
import Data.Kind (Type)
import Data.List (tails)
import Data.Maybe (isNothing, mapMaybe)
import Data.Monoid (All(All))
import Control.Monad

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
        steps = [ (c, seg x1 x2 :× seg y1 y2 :× seg z1 z2 :× Pt)
                | (c,x1,x2,y1,y2,z1,z2) <- inp]
        p1seg = seg (-50) 50
        p1cube = p1seg :× p1seg :× p1seg :× Pt
    print (solve (mapMaybe (traverse (intersectBox p1cube)) steps))
    print (solve steps)

-- | Figure out how many lights the given instructions turn on.
--
-- Each @on@ command has all future boxes subtracted from it before
-- being included in the sum.
solve :: [(C, Box n)] -> Int
solve xs = sum [size c | (Con,y):ys <- tails xs, c <- foldM subbox y (map snd ys)]

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

-- * N-dimensional boxes

-- | Natural numbers (used for type index)
data N = S N | Z

-- | An n-dimensional box.
data Box :: N -> Type where
  Pt   :: Box 'Z -- ^ A single point
  (:×) :: Seg -> Box n -> Box ('S n) -- ^ A box extended along an axis

infixr 6 :× -- a little higher than list cons

-- | Returns the number of points contained in a box.
size :: Box n -> Int
size Pt         = 1
size (s :× box) = len s * size box

-- | Find the intersection between two boxes.
intersectBox :: Box n -> Box n -> Maybe (Box n)
intersectBox Pt        Pt        = pure Pt
intersectBox (x :× xs) (y :× ys) = (:×) <$> intersectSeg x y <*> intersectBox xs ys

-- | Subtract the second box from the first box returning a list of boxes
-- that cover all the remaining area.
subbox ::
  Box n ->
  Box n {- ^ remove -} ->
  [Box n]
subbox b1 b2
  | isNothing (intersectBox b1 b2) = [b1]
  | otherwise = [b | (b, All False) <- runWriterT (go b1 b2)]
  where
    segs (Seg a b) (Seg c d) =
      let xs = [a] ++ [c | a < c, c < b] ++ [d | a < d, d < b] ++ [b]
      in writerT [(Seg lo hi, All (c <= lo && lo < d)) | lo <- xs | hi <- tail xs]

    go :: Box n -> Box n -> WriterT All [] (Box n)
    go Pt        Pt        = pure Pt
    go (x :× xs) (y :× ys) = (:×) <$> segs x y <*> go xs ys
