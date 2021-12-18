{-# Language BlockArguments, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/18>

-}
module Main (main) where

import Advent ( getInputLines )
import Control.Applicative
import Data.Char (isDigit)
import Data.List (tails)
import Text.ParserCombinators.ReadP

-- | >>> :main
-- 3551
-- 4555
main :: IO ()
main =
 do inp <- map parse <$> getInputLines 18
    print $ mag $ foldl1 (\a b -> reduce (add a b)) inp
    print $ maximum [mag (reduce (add x y)) `max`
                     mag (reduce (add y x))
                    | x:ys <- tails inp, y <- ys]

data Tok = Int Int | Open | Close  deriving Show

add :: [Tok] -> [Tok] -> [Tok]
add x y = reduce (Open : x ++ y ++ [Close])

reduce :: [Tok] -> [Tok]
reduce xs = maybe xs reduce (explode 0 [] xs <|> split [] xs)

explode :: Int -> [Tok] -> [Tok] -> Maybe [Tok]
explode d ys (Open:xs) = explode (d+1) (Open:ys) xs
explode d ys (Close:xs) = explode (d-1) (Close:ys) xs
explode 5 (Open:ys) (Int x:Int y:Close:xs) = Just (reverse (send x ys) ++ [Int 0] ++ send y xs)
explode d ys (Int x:xs) = explode d (Int x:ys) xs
explode _ _ [] = Nothing

split :: [Tok] -> [Tok] -> Maybe [Tok]
split l (Int x:r)
  | x >= 10 = Just (reverse l ++ [Open, Int (x`div`2), Int ((x+1)`div`2), Close] ++ r)
split l (x:xs) = split (x:l) xs
split _ [] = Nothing

send :: Int -> [Tok] -> [Tok]
send x (Int y:xs) = Int (x+y) : xs
send x (y:    xs) = y : send x xs
send _ []         = []

mag :: [Tok] -> Int
mag = fst . mag'

mag' :: [Tok] -> (Int, [Tok])
mag' (Int x:xs) = (x,xs)
mag' (Open:xs)
  | (l,xs1      ) <- mag' xs
  , (r,Close:xs2) <- mag' xs1
  = (3*l+2*r, xs2)
mag' _ = error "mag' called on malformed token list"

-- * Parsing

parse :: String -> [Tok]
parse str =
  case readP_to_S (pTok <* eof) str of
    [(toks,_)] -> toks
    _ -> undefined

pTok :: ReadP [Tok]
pTok = ppair +++ (pure . Int . read <$> munch1 isDigit)
  where
    ppair = do x <- char '[' *> pTok
               y <- char ',' *> pTok <* char ']'
               pure (Open : x ++ y ++ [Close])

{-

Zipper version that feels better but I haven't finished

tree (Open:xs)
  | (l,xs) <- tree xs
  , (r,Close:xs) <- tree xs
  = (L l r,xs)
tree (Int x:xs) = (N x, xs)
  

mag (N x) = x
mag (L l r) = 3 * mag l + 2 * mag r


data Zip = ZL X Zip | ZR X Zip | Top  deriving Show

unz :: X -> Zip -> X
unz r (ZL l z) = unz (L l r) z
unz l (ZR r z) = unz (L l r) z
unz l Top = l


findExplode :: X -> Maybe (Int, Int, Zip)
findExplode = go 0 Top
  where
    go 4 z (L (N x) (N y)) = Just (x,y,z)
    go 5 _ _ = Nothing
    go d z (L l r) = go (d+1) (ZR r z) l <|> go (d+1) (ZL l z) r
    go _ _ _ = Nothing

explode :: X -> Maybe X
explode x =
 do (l,r,z) <- findExplode x
    Just (unz (N 0) (addUpL l (addUpR r z)))

addUpL :: Int -> Zip -> Zip
addUpL n Top = Top
addUpL n (ZL l z) = ZL (addDownR n l) z
addUpL n (ZR r z) = ZR r (addUpL n z)

addUpR :: Int -> Zip -> Zip
addUpR n Top = Top
addUpR n (ZL l z) = ZL l (addUpR n z)
addUpR n (ZR r z) = ZR (addDownL n r) z

addDownL :: Int -> X -> X
addDownL n (L l r) = L (addDownL n l) r
addDownL n (N m) = N (n+m)

addDownR :: Int -> X -> X
addDownR n (L l r) = L l (addDownR n r)
addDownR n (N m) = N (n+m)



parseX = (\i -> let [(b,_)] = readP_to_S (pList <* eof ) i in b)


data X = L X X | N Int deriving Show
parse = (\i -> let [(b,_)] = readP_to_S (pTok <* eof ) i in b)
add x y = L x y

pList :: ReadP X
pList = (between (char '[') (char ']') (L <$> pList <* char ',' <*> pList)) +++ (N . read <$> munch1 isDigit)



-}