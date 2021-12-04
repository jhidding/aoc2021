# Day 4: Giant Squid
We're playing Bingo with a giant squid. This is why I love advent of Code!

Doing contrived array arithmetic is not seen as the strong suit of Haskell. Solving this in Python with NumPy would seem so much easier. I will use the nice `Massiv` library, that implements multi-dimensional arrays, fancy indexing, stencil operations etc.

``` {.haskell file=app/Day04.hs}
module Day04 where

import RIO hiding (try)
import RIO.List (partition, headMaybe, lastMaybe)
import Parsing ( Parser, sepEndBy1, sepBy1, char, hspace, eol
               , integer, lexeme, readInputParsing
               , failOnException )
import qualified Data.Massiv.Array as A

<<data-types-day-4>>
<<parser-day-4>>
<<solution-day-4>>

<<run-solutions>>
```

We need to have integers that we can mark when we play Bingo. I'll make a generic `Mark` container, that contains an extra boolean flag.

``` {.haskell #data-types-day-4}
data Mark a = Mark
    { marked :: Bool
    , markValue :: a
    }

markEq :: (Eq a) => a -> Mark a -> Mark a
markEq v Mark{ .. }
    | v == markValue = Mark True   markValue
    | otherwise      = Mark marked markValue

unmarked :: Mark a -> Bool
unmarked = not . marked

type Board = A.Array A.B A.Ix2 (Mark Int)

data Bingo = Bingo
    { draws :: [Int]
    , boards :: [Board]
    }
```

Next, we need to parse the input data.

``` {.haskell #parser-day-4}
drawsP :: Parser [Int]
drawsP = sepBy1 integer (lexeme $ char ',')

boardP :: Parser Board
boardP = sepEndBy1 row eol >>= toBoard
    where whitespace = lexeme (return ())
          row = whitespace >> some (Mark False <$> integer)
          toBoard = failOnException . A.fromListsM A.Seq

bingoP :: Parser Bingo
bingoP = Bingo <$> drawsP <* eol <* eol <*> sepEndBy1 boardP eol

readInput :: (HasLogFunc env) => RIO env Bingo
readInput = readInputParsing "data/day04.txt" bingoP
```

We win at Bingo if a row of column on a board is fully marked. The `Massiv` library provides the nice functions `outerSlices` and `innerSlices`, allowing us to traverse all rows and columns:

``` {.haskell #solution-day-4}
win :: Board -> Bool
win b = rows || columns
    where rows    = any (all marked) (A.outerSlices b)
          columns = any (all marked) (A.innerSlices b)
```

Each time a number is called we mark all matching values:

``` {.haskell #solution-day-4}
markBoard :: Int -> Board -> Board
markBoard n = A.compute . A.map (markEq n)
```

For part A we need to figure out, the first board to win and the last number that was called:

``` {.haskell #solution-day-4}
winners :: [Int] -> [Board] -> [(Int, Board)]
winners [] _  = []
winners _  [] = []
winners (d:draws) boards = map (d,) won <> winners draws lost
    where (won, lost) = partition win 
                      $ map (markBoard d) boards

score :: (Int, Board) -> Int
score (n, b) = n * sum (unmarkedValues $ A.toList b)
    where unmarkedValues = map markValue . filter unmarked

solutionA :: Bingo -> Maybe Int
solutionA Bingo{..} = score <$> headMaybe (winners draws boards)
```

For part B we need to know the last board to win:

``` {.haskell #solution-day-4}
solutionB :: Bingo -> Maybe Int
solutionB Bingo{..} = score <$> lastMaybe (winners draws boards)
```

