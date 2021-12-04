-- ~\~ language=Haskell filename=app/Day04.hs
-- ~\~ begin <<lit/day04.md|app/Day04.hs>>[0]
module Day04 where

import RIO hiding (try)
import RIO.List (partition, headMaybe, lastMaybe)
import Parsing ( Parser, sepEndBy1, sepBy1, char, hspace, eol
               , integer, lexeme, readInputParsing
               , failOnException )
import qualified Data.Massiv.Array as A

-- ~\~ begin <<lit/day04.md|data-types-day-4>>[0]
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
markBoard :: Int -> Board -> Board
markBoard n = A.compute . A.map (markEq n)
-- ~\~ end
-- ~\~ begin <<lit/day04.md|solution-day-4>>[2]
winSeq :: [Int] -> [Board] -> [(Int, Board)]
winSeq []       _       = []
winSeq _        []      = []
winSeq (d:draws) boards = map (d,) winners <> winSeq draws losers
    where (winners, losers) = partition win $ markBoard d <$> boards
-- ~\~ end
-- ~\~ begin <<lit/day04.md|solution-day-4>>[3]
score :: (Int, Board) -> Int
score (n, b) = n * sum (unmarkedValues $ A.toList b)
    where unmarkedValues = map markValue . filter unmarked

solutionA :: Bingo -> Maybe Int
solutionA Bingo{..} = score <$> headMaybe (winSeq draws boards)
-- ~\~ end
-- ~\~ begin <<lit/day04.md|solution-day-4>>[4]
solutionB :: Bingo -> Maybe Int
solutionB Bingo{..} = score <$> lastMaybe (winSeq draws boards)
-- ~\~ end

-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
