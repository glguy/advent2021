{-# Language BlockArguments, TemplateHaskell #-}
module Advent.Format (format) where

import Advent (count, getRawInput)
import Advent.Format.Lexer ( alexScanTokens, AlexPosn(..) )
import Advent.Format.Parser (parseFormat, ParseError(..) )
import Advent.Format.Types ( interesting, Format(..), acceptsEmpty, showFormat, showToken )
import Control.Applicative ((<|>), some)
import Control.Monad ( (<=<) )
import Data.Char ( isDigit, isSpace, isUpper )
import Data.Maybe ( listToMaybe )
import Data.Traversable ( for )
import Data.List (stripPrefix)
import Data.Foldable (asum)
import Language.Haskell.TH
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

parse :: String -> Q Format
parse txt =
  case parseFormat (alexScanTokens txt) of
    Right fmt                -> pure fmt
    Left (Unclosed p)        -> failAt p "Unclosed parenthesis"
    Left (UnexpectedToken p t) -> failAt p ("Unexpected token " ++ showToken t)
    Left UnexpectedEOF       -> fail "Format parse error, unexpected end-of-input"

failAt :: AlexPosn -> String -> Q a
failAt (AlexPn _ line col) msg = fail ("Format parse error at " ++ show line ++ ":" ++ show col ++ ", " ++ msg)

format :: QuasiQuoter
format = QuasiQuoter
  { quoteExp  = uncurry makeParser <=< prepare
  , quotePat  = \_ -> fail "format: patterns not supported"
  , quoteType = \_ -> fail "format: types not supported"
  , quoteDec  = \_ -> fail "format: declarations not supported"
  }

prepare :: String -> Q (Int,String)
prepare str =
  case lines str of
    []   -> fail "Empty input format"
    [x]  -> case reads x of
              [(n,rest)] -> pure (n, dropWhile (' '==) rest)
              _ -> fail "Failed to parse single-line input pattern"
    x:xs ->
      do n <- case readMaybe x of
                Nothing -> fail "Failed to parse format day number"
                Just n  -> pure n
         pure (n, concatMap (drop indent) xs1)
      where
        xs1    = filter (any (' ' /=)) xs
        indent = minimum (map (length . takeWhile (' '==)) xs1)

makeParser :: Int -> String -> ExpQ
makeParser n str =
  do fmt <- parse str
     let formats = [| readP_to_S ($(toReadP fmt) <* eof) |]
     let qf = [| maybe (error "bad input parse") fst . listToMaybe . $formats |]
     if n == 0 then
       qf
     else
       [| $qf <$> getRawInput n |]

toReadP :: Format -> ExpQ
toReadP s =
  case s of
    Literal xs -> [| () <$ string xs |]

    Gather p -> [| fst <$> gather $(toReadP p) |]

    Named n
      | isUpper (head n) -> enumParser n
      | otherwise -> varE (mkName n)

    UnsignedInteger -> [| (read :: String -> Integer) <$>                                      munch1 isDigit  |]
    SignedInteger   -> [| (read :: String -> Integer) <$> ((++) <$> option "" (string "-") <*> munch1 isDigit) |]
    UnsignedInt     -> [| (read :: String -> Int    ) <$>                                      munch1 isDigit  |]
    SignedInt       -> [| (read :: String -> Int    ) <$> ((++) <$> option "" (string "-") <*> munch1 isDigit) |]

    Char      -> [| satisfy ('\n' /=) |]
    Letter    -> [| satisfy (\x -> 'a' <= x && x <= 'z' || 'A' <= x && x <= 'Z') |]
    Word      -> [| some (satisfy (not . isSpace)) |]

    Many x
      | acceptsEmpty x -> fail ("Argument to * accepts ε: " ++ showFormat 0 s "")
      | interesting x -> [|       many $(toReadP x) |]
      | otherwise     -> [| () <$ many $(toReadP x) |]

    Some x
      | acceptsEmpty x -> fail ("Argument to + accepts ε: " ++ showFormat 0 s "")
      | interesting x -> [|       some $(toReadP x) |]
      | otherwise     -> [| () <$ some $(toReadP x) |]

    SepBy x y
      | acceptsEmpty x, acceptsEmpty y -> fail ("Both arguments to & accept ε: " ++ showFormat 0 s "")
      | interesting x -> [|       sepBy $(toReadP x) $(toReadP y) |]
      | otherwise     -> [| () <$ sepBy $(toReadP x) $(toReadP y) |]

    Alt x y
      | xi, yi    -> [| Left    <$> $xp <|> Right   <$> $yp |]
      | xi        -> [| Just    <$> $xp <|> Nothing <$  $yp |]
      |     yi    -> [| Nothing <$  $xp <|> Just    <$> $yp |]
      | otherwise -> [|             $xp <|>             $yp |]
      where
        xi = interesting x
        yi = interesting y
        xp = toReadP x
        yp = toReadP y

    Empty -> [| pure () |]
    Follow x1 x2 ->
      case [(interesting x, toReadP x) | x <- follows x1 (follows x2 [])] of
        [] -> [| pure () |]
        (ix,x):xs
          | n <= 1    -> foldl follow1 x xs
          | ix        -> foldl follow [| $(conE (tupleDataName n)) <$> $x |] xs
          | otherwise -> foldl follow [| $(conE (tupleDataName n)) <$  $x |] xs
          where
            n               = Advent.count fst ((ix,x):xs)
            follow1 l (i,r) = if i then [| $l  *> $r |] else [| $l <* $r |]
            follow  l (i,r) = if i then [| $l <*> $r |] else [| $l <* $r |]


-- | Prefix a list of format strings with a format string.
-- If the given list has all the topmost 'Follow' constructors
-- removed, the output list will as well. Any consecutive literals found
-- while flattening will be combined.
follows :: Format -> [Format] -> [Format]
follows (Follow x y) zs               = follows x (follows y zs)
follows Empty        zs               = zs
follows (Literal x)  (Literal y : zs) = follows (Literal (x ++ y)) zs
follows x            zs               = x : zs

enumParser :: String -> ExpQ
enumParser nameStr =
  do tyName <- maybe (fail ("Failed to find type named " ++ show nameStr)) pure
           =<< lookupTypeName nameStr

     info <- reify tyName
     cons <-
       case info of
         TyConI (DataD _ _ _ _ cons _) -> pure cons
         _ -> fail ("Failed to find data declaration for " ++ show nameStr)

     entries <-
       for cons \con ->
         case con of
           NormalC name []
             | Just str <- stripPrefix nameStr (nameBase name) ->
                pure (name, str)
           _ -> fail ("Unsupported constructor: " ++ show con)

     let parsers = [[| $(conE name) <$ string str |] | (name, str) <- entries]

     [| asum $(listE parsers) |]
