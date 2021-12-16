{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/16>

Decode an expression from a bitstream.

-}
module Main (main) where

import Advent (fromDigits, format)
import Control.Applicative (many)
import Data.Char (digitToInt)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as ReadP

-- | >>> :main
-- 843
-- 5390807940351
main :: IO ()
main =
 do inp <- [format|16 %s%n|]
    let [p] = parsePackets (decodeHex =<< inp)
    print (vers p)
    print (eval p)

data Packet = Lit Int Int | Op Int Int [Packet]
  deriving Show

-- | Compute the sum of the versions
vers :: Packet -> Int
vers (Lit v _   ) = v
vers (Op  v _ xs) = v + sum (map vers xs)

-- | Evaluate the expression
eval :: Packet -> Int
eval (Lit _ n) = n
eval (Op _ 0 xs) = sum     (eval <$> xs)
eval (Op _ 1 xs) = product (eval <$> xs)
eval (Op _ 2 xs) = minimum (eval <$> xs)
eval (Op _ 3 xs) = maximum (eval <$> xs)
eval (Op _ 5 [x,y]) = if eval x >  eval y then 1 else 0
eval (Op _ 6 [x,y]) = if eval x <  eval y then 1 else 0
eval (Op _ 7 [x,y]) = if eval x == eval y then 1 else 0
eval o = error ("bad expression: " ++ show o)

parsePackets :: String -> [Packet]
parsePackets xs =
  case ReadP.readP_to_S (many pPacket <* skipPad 3 <* ReadP.eof) xs of
    [(ps,_)] -> ps
    _ -> error ("Failed parsing: " ++ show xs)

-- | Parse a single packet
pPacket :: ReadP Packet
pPacket =
 do v <- field 3
    t <- field 3
    if t == 4
      then Lit v   <$> pLiteral
      else Op  v t <$> pArguments

-- | Skip up to @n@ padding zeros
skipPad :: Int -> ReadP ()
skipPad 0 = pure ()
skipPad n = ReadP.optional (ReadP.char '0' >> skipPad (n-1))

-- | Parse a fixed-width big-endian, binary number
field :: Int -> ReadP Int
field n = fromDigits 2 . map digitToInt <$> ReadP.count n ReadP.get

-- | Parse a variable-sized number chunk
pLiteral :: ReadP Int
pLiteral = fromDigits 16 <$> go
  where
    go =
     do more <- (1==) <$> field 1
        chunk <- field 4
        if more then (chunk:) <$> go else pure [chunk]

-- | Parse a list of sub-packets
pArguments :: ReadP [Packet]
pArguments =
 do byCount <- (1==) <$> field 1
    if byCount
      then do n <- field 11
              ReadP.count n pPacket
      else do size <- field 15
              parsePackets <$> ReadP.count size ReadP.get

-- | Decode a hex character into 4 bits
decodeHex :: Char -> String
decodeHex '0' = "0000"
decodeHex '1' = "0001"
decodeHex '2' = "0010"
decodeHex '3' = "0011"
decodeHex '4' = "0100"
decodeHex '5' = "0101"
decodeHex '6' = "0110"
decodeHex '7' = "0111"
decodeHex '8' = "1000"
decodeHex '9' = "1001"
decodeHex 'A' = "1010"
decodeHex 'B' = "1011"
decodeHex 'C' = "1100"
decodeHex 'D' = "1101"
decodeHex 'E' = "1110"
decodeHex 'F' = "1111"
decodeHex x = error ("bad hex: " ++ show x)