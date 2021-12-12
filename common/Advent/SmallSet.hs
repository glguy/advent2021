{-# Language TypeFamilies, TypeOperators #-}
module Advent.SmallSet where

import Data.Bits
import Data.List (foldl')
import Data.Word (Word64)
import Data.MemoTrie (HasTrie(..))
import Data.Coerce (coerce)

newtype SmallSet = SmallSet Word64
  deriving (Eq, Ord)

fromList :: [Int] -> SmallSet
fromList xs = foldl' (flip insert) empty xs

toList :: SmallSet -> [Int]
toList (SmallSet x) = go 0 x
  where
    go offset n
      | 0 == n     = []
      | next == 63 = [63] -- avoid shift overflow
      | seq x True = x : go (1+x) (n `unsafeShiftR` (next+1))
      where
        next = countTrailingZeros n
        x    = offset + next

empty :: SmallSet
empty = SmallSet 0

union :: SmallSet -> SmallSet -> SmallSet
union (SmallSet x) (SmallSet y) = SmallSet (x .|. y)

unions :: [SmallSet] -> SmallSet
unions = foldl' union empty

intersection :: SmallSet -> SmallSet -> SmallSet
intersection (SmallSet x) (SmallSet y) = SmallSet (x .&. y)

difference :: SmallSet -> SmallSet -> SmallSet
difference (SmallSet x) (SmallSet y) = SmallSet (x .&. complement y)

insert :: Int -> SmallSet -> SmallSet
insert x (SmallSet y)
  | 0 <= x && x < 64 = SmallSet (setBit y x)
  | otherwise        = error ("Advent.SmallSet.insert: bad argument " ++ show x)

member :: Int -> SmallSet -> Bool
member x (SmallSet y)
  | 0 <= x && x < 64 = testBit y x
  | otherwise        = error ("Advent.SmallSet.member: bad argument " ++ show x)

instance Show SmallSet where
  showsPrec p x = showParen (p > 10) (showString "fromList " . shows (toList x))

instance HasTrie SmallSet where
  newtype SmallSet :->: a = T (Word64 :->: a)
  trie f = T (trie (coerce f))
  untrie (T x) = coerce (untrie x)
  enumerate (T x) = coerce (enumerate x)
