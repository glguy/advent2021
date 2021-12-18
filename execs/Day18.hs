{-# Language BlockArguments, ImportQualifiedPost, QuasiQuotes, ViewPatterns #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/18>

Today's problem had us perform manipulations on a tree-based
term language. It was made tricky because the problem asked
us to do things to the nearest left and right neighbors
of elements of our tree.

-}
module Main (main) where

import Advent (getInputLines)
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List (tails)
import Text.ParserCombinators.ReadP

-- | >>> :main
-- 3551
-- 4555
main :: IO ()
main =
 do inp <- map parse <$> getInputLines 18
    print (magnitude (foldl1 add inp))
    print (maximum [magnitude (add x y) `max` magnitude (add y x)
                   | x:ys <- tails inp, y <- ys])

-- | A snailfish expression
data X
  = X :+ X -- ^ pair
  | N Int  -- ^ regular number
  deriving Show

-- | Add two expressions and reduce them
add :: X -> X -> X
add x y = reduce (x :+ y)

-- | Reduce an expression until it won't reduce
reduce :: X -> X
reduce x = maybe x reduce (explode <$> unstable x <|> split x)

-- | Find the first pair of numbers at depth 4.
unstable :: X -> Maybe (Int, Int, Zip)
unstable = go (4::Int) Top
  where
    go 0 z (N l :+ N r) = Just (l, r, z)
    go 0 _ _ = Nothing
    go d z (l :+ r) = go (d-1) (ZL r z) l <|> go (d-1) (ZR l z) r
    go _ _ _ = Nothing

-- Add the left and right components to the nearest number
-- on the left and right respectively. Replace the hole with
-- a zero.
explode :: (Int, Int, Zip) -> X
explode (l, r, z) = fromZip (N 0) (addUp l r z)

-- | Replace the first number with value 10 or more with a pair
-- of it divided in half rounding first down then up.
split :: X -> Maybe X
split (N x)
  | x >= 10 = Just (N (x`div`2) :+ N ((x+1)`div`2))
  | otherwise = Nothing
split (l :+ r) = (:+ r) <$> split l <|> (l :+) <$> split r

-- * Expression zippers

-- | The type of a hole in an expression tree. Values of
-- this type describe a location in an 'X' that's missing
-- a subterm. This term can be replaced with 'fromZip' giving
-- you the complete 'X' back.
data Zip
  = ZR X Zip -- ^ The hole is on the right side of a pair
  | ZL X Zip -- ^ The hole is on the left side of a pair
  | Top      -- ^ the topmost hole
  deriving Show

-- | Rebuild an expression given a zipper and the value to put in the hole.
fromZip :: X -> Zip -> X
fromZip r (ZR l z) = fromZip (l :+ r) z
fromZip l (ZL r z) = fromZip (l :+ r) z
fromZip l Top = l

addUp :: Int -> Int -> Zip -> Zip
addUp ln rn (ZR l z) = ZR (addDownR ln l) (addUpR rn z)
addUp ln rn (ZL r z) = ZL (addDownL rn r) (addUpL ln z)
addUp _ _ Top = Top

addUpL :: Int -> Zip -> Zip
addUpL _ Top = Top
addUpL n (ZR l z) = ZR (addDownR n l) z
addUpL n (ZL r z) = ZL r (addUpL n z)

addUpR :: Int -> Zip -> Zip
addUpR _ Top = Top
addUpR n (ZR l z) = ZR l (addUpR n z)
addUpR n (ZL r z) = ZL (addDownL n r) z

addDownL :: Int -> X -> X
addDownL n (l :+ r) = addDownL n l :+ r
addDownL n (N m) = N (n+m)

addDownR :: Int -> X -> X
addDownR n (l :+ r) = l :+ addDownR n r
addDownR n (N m) = N (n+m)

-- | Compute the /magnitude/ of an expression
--
-- >>> magnitude (parse "[9,1]")
-- 29
--
-- >>> magnitude (parse "[[1,2],[[3,4],5]]")
-- 143
magnitude :: X -> Int
magnitude (N x) = x
magnitude (l :+ r) = 3 * magnitude l + 2 * magnitude r

-- * Parsing

-- | Parse an expression
--
-- >>> parse "[[[[0,9],2],3],4]"
-- (((N 0 :+ N 9) :+ N 2) :+ N 3) :+ N 4
parse :: String -> X
parse (readP_to_S pX -> [(x,_)]) = x
parse _ = error "bad input"

-- | ReadP expression parser
pX :: ReadP X
pX = pair +++ number
  where
    pair = (:+) <$ char '[' <*> pX <* char ',' <*> pX <* char ']'
    number = N . read <$> munch1 isDigit
