-- ~\~ language=Haskell filename=app/Day04.hs
-- ~\~ begin <<lit/day04.md|app/Day04.hs>>[0]
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

-- ~\~ begin <<lit/day04.md|data-types-day-4>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/day04.md|parser-day-4>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/day04.md|solution-day-4>>[0]
win :: Board -> Bool
win b = rows || columns
    where rows    = any (all marked) (A.outerSlices b)
          columns = any (all marked) (A.innerSlices b)
-- ~\~ end
-- ~\~ begin <<lit/day04.md|solution-day-4>>[1]
markOnBoard :: Int -> Board -> Board
markOnBoard n = A.compute . A.map mayMark
    where mayMark Mark{ .. }
            | markValue == n = Mark True markValue
            | otherwise      = Mark marked markValue
-- ~\~ end
-- ~\~ begin <<lit/day04.md|solution-day-4>>[2]
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
-- ~\~ end
-- ~\~ begin <<lit/day04.md|solution-day-4>>[3]
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
-- ~\~ end

-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
