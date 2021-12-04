# Day 4: Giant Squid
We're playing Bingo with a giant squid. This is why I love advent of Code!

Doing contrived array arithmetic is not seen as the strong suit of Haskell. Solving this in Python with NumPy would be so much easier. I will use the nice `Massiv` library, that implements multi-dimensional arrays, fancy indexing, stencil operations etc.

``` {.haskell file=app/Day04.hs}
module Day04 where

import RIO hiding (try)
import RIO.List (find)
import qualified RIO.Set as Set
import Control.Monad.Except (throwError)
import Parsing (Parser, sepEndBy1, sepBy1, char, hspace, eol, integer, lexeme, readInputParsing)
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Mutable as MA

import Text.Megaparsec (try, fancyFailure)
import Text.Megaparsec.Error (ErrorFancy(..))

failOnException :: (Exception e) => Either e a -> Parser a
failOnException = either convertError return
    where convertError = fancyFailure . Set.singleton . ErrorFail . displayException

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

instance Show a => Show (Mark a) where
    show Mark{..} = show markValue <> if marked then "*" else " "

mark :: Mark a -> Mark a
mark Mark{ markValue } = Mark True markValue

unmarked :: Mark a -> Bool
unmarked = not . marked

type Board = A.Array A.B A.Ix2 (Mark Int)
type MutableBoard s = MA.MArray s A.B A.Ix2 (Mark Int)

data Bingo = Bingo
    { draws :: [Int]
    , boards :: [Board]
    } deriving (Show)
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
markOnBoard :: Int -> Board -> Board
markOnBoard n = A.compute . A.map mayMark
    where mayMark Mark{ .. }
            | markValue == n = Mark True markValue
            | otherwise      = Mark marked markValue
```

For part A we need to figure out, the first board to win and the last number that was called:

``` {.haskell #solution-day-4}
firstToWin :: [Int] -> [Board] -> Maybe (Int, Board)
firstToWin []        _      = Nothing
firstToWin (d:draws) boards = case (find win nextBoards) of
        Nothing -> firstToWin draws nextBoards
        Just b  -> Just (d, b)
    where nextBoards = map (markOnBoard d) boards

score :: (Int, Board) -> Int
score (n, b) = n * sum (unmarkedValues $ A.toList b)
    where unmarkedValues = map markValue . filter unmarked

solutionA :: Bingo -> Maybe Int
solutionA Bingo{..} = score <$> firstToWin draws boards
```

For part B we need to know the last board to win:

``` {.haskell #solution-day-4}
lastToWin :: [Int] -> [Board] -> Maybe (Int, Board)
lastToWin [] _ = Nothing
lastToWin (d:draws) [b]
        | win nextBoard = Just (d, nextBoard)
        | otherwise     = lastToWin draws [nextBoard]
        where nextBoard = markOnBoard d b
lastToWin (d:draws) boards = lastToWin draws (filter (not . win) nextBoards)
        where nextBoards = map (markOnBoard d) boards

solutionB :: Bingo -> Maybe Int
solutionB Bingo{..} = score <$> lastToWin draws boards
```

