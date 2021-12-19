-- ~\~ language=Haskell filename=app/Day07.hs
-- ~\~ begin <<lit/day07.md|app/Day07.hs>>[0]
module Day07 where

import RIO
import RIO.List (sort)
import RIO.List.Partial ((!!))
import Parsing ( Parser, readInputParsing, sepEndBy1
               , lexeme, char, integer)

-- ~\~ begin <<lit/day07.md|parser-day-7>>[0]
csvInts :: Parser [Int]
csvInts = sepEndBy1 integer (lexeme (char ','))

readInput :: (HasLogFunc env) => RIO env [Int]
readInput = readInputParsing "data/day07.txt" csvInts
-- ~\~ end
-- ~\~ begin <<lit/day07.md|solution-day-7>>[0]
costFuncA :: [Int] -> Int -> Int
costFuncA cs x = sum (map (abs . (x -)) cs)

median :: [Int] -> Int
median x = sort x !! (length x `div` 2)

solutionA :: [Int] -> (Int, Int)
solutionA as = (loc, costFuncA as loc)
    where loc = sort as !! (length as `div` 2)
-- ~\~ end
-- ~\~ begin <<lit/day07.md|solution-day-7>>[1]
costFuncB :: [Int] -> Int -> Int
costFuncB cs x = sum (map f cs)
    where f c = abs (x - c) * (abs (x - c) + 1) `div` 2

mean :: [Int] -> Int
mean x = sum x `div` length x

solutionB :: [Int] -> (Int, Int)
solutionB x = (loc, costFuncB x loc)
    where loc = mean x
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
