---
title: "Day 2: Dive!"
date: 2021-12-02
summary: "Parsing and our first state machine."
weight: 2
---

We are given our first mini instruction set! We need to steer the submarine using an instruction set `forward`, `down` or `up` with a number attached. We get to do our first parsing of this year (yay!).

``` {.haskell file=app/Day02.hs}
module Day02 where

import RIO
import RIO.List (foldl)
import qualified RIO.Text as Text
import Parsing (readInputParsing, Parser, lexeme, string, integer, sepEndBy1, eol)

<<solution-day-2>>
<<run-solutions>>
```

We start by defining a datatype and the associated parser:

``` {.haskell #solution-day-2}
data Instruction
    = GoForward Int
    | GoUp Int
    | GoDown Int
    deriving (Show)

instructions :: Parser [Instruction]
instructions = sepEndBy1 (lexeme direction <*> integer) eol
    where direction =   (string "forward" $> GoForward)
                    <|> (string "up"      $> GoUp)
                    <|> (string "down"    $> GoDown)

readInput :: (MonadIO m, MonadReader env m, HasLogFunc env) => m [Instruction]
readInput = readInputParsing "data/day02.txt" instructions
```

In the first part, we are asked to do some Turtle movement. We can reduce the set of instructions in a `foldl` if we define a function that updates the position for each move:

``` {.haskell #solution-day-2}
type Pos = (Int, Int)

moveA :: Pos -> Instruction -> Pos
moveA (x, y) (GoForward dx) = (x + dx, y)
moveA (x, y) (GoUp dy)      = (x, y - dy)
moveA (x, y) (GoDown dy)    = (x, y + dy)

solutionA :: [Instruction] -> Int
solutionA inst = x * y
    where (x, y) = foldl moveA (0, 0) inst
```

In the second part, the interpretation of the instructions changes slightly, but the only thing we have to change is the `moveA` function and the corresponding accumulator data type (I'm using `NamedFieldPuns` and `RecordWildCards`, which I prefer over lenses in these simple cases):

``` {.haskell #solution-day-2}
data Navigation = Navigation
    { navDepth :: Int
    , navAim   :: Int
    , navPos   :: Int
    } deriving (Show)

moveB :: Navigation -> Instruction -> Navigation
moveB n@Navigation{..} (GoForward x) = n{ navPos = navPos + x
                                        , navDepth = navDepth + navAim * x }
moveB n@Navigation{..} (GoUp x)      = n{ navAim = navAim - x }
moveB n@Navigation{..} (GoDown x)    = n{ navAim = navAim + x }

solutionB :: [Instruction] -> Int
solutionB inst = navPos * navDepth
    where Navigation{..} = foldl moveB (Navigation 0 0 0) inst
```

