# Advent of Code 2021

These are my Advent of Code 2021 solutions. I generally try to focus on making clean and documented Haskell solutions to each puzzle.

We'll be chatting about AoC on IRC all December. You can find AoC discussion on [Libera Chat](https://libera.chat)'s `##adventofcode` and `#haskell` ([webchat](https://web.libera.chat/#adventofcode))

## Building

I recommend installing `ghc` with [ghcup](https://www.haskell.org/ghcup/).

```
$ curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

I'm using `GHC 9.0.1` this year

```
$ ghcup install ghc 9.0.1
$ ghcup set     ghc 9.0.1
```

`cabal-install` is the best way to get things built, and is provided by ghcup

```
$ cabal update
$ cabal build
```

## Running solutions

All the solutions take an optional command line argument that can be an input file name or `-` to read the input from `stdin`.
The default input filename is `inputs/input##.txt`

```
$ cabal run Day01
...
1681
1704
```

## Common libraries used

I try to avoid using too many dependencies for these problems.

* **containers** - Almost all of the solutions will benefit from having access to `Map` and `Set` types from this package.
* **array** - Standard mutable and immutable array library
* **doctest** - Having checked examples in the documentation makes it easier to understand what code does and easy to add unit tests.
* **alex** - Lexer generator package I use for my input format DSL
* **happy** - Parser generator package I use for my input format DSL

## Input file DSL

To speed up the process of writing input parsers, I use a quasiquoter that generates [ReadP](https://hackage.haskell.org/package/base-4.16.0.0/docs/Text-ParserCombinators-ReadP.html)
parsers.

[Day 13](https://adventofcode.com/2021/day/13) used in input format of pairs of numbers and fold instructions as seen below.

```
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
```

This can be parsed using the following pattern string

```haskell
data A = Ax | Ay
[format|13 (%u,%u%n)*%n(fold along @A=%u%n)*|]
```

which processes an output of

```haskell
([(6,10),(0,14),(9,10),(0,3),(10,4),(4,11),(6,0),(6,12),(4,1),(0,13),(10,12),(3,4),(3,0),(8,4),(1,10),(2,14),(8,10),(9,0)],[(Ay,7),(Ax,5)])
```

Getting the data into Haskell datatypes quickly allows me to focus on the problem
and not the input file, and it keeps the solution files focused, as well.
