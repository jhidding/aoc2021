-- ~\~ language=Haskell filename=app/Day06.hs
-- ~\~ begin <<lit/day06.md|app/Day06.hs>>[0]
module Day06 where

import RIO
import RIO.List (foldl)
import RIO.List.Partial (last)
import Parsing ( Parser, readInputParsing, sepEndBy1
               , lexeme, char, integer)
import RIO.Map (Map, (!?))
import qualified RIO.Map as Map
-- ~\~ begin <<lit/day06.md|imports-day-6>>[0]
import Tally (Tally, CFunctor(..), CApplicative(..), CMonad(..), ElemCt)
import qualified Tally
-- ~\~ end

-- ~\~ begin <<lit/day06.md|parser-day-6>>[0]
csvInts :: Parser [Int]
csvInts = sepEndBy1 integer (lexeme (char ','))

readInput :: (HasLogFunc env) => RIO env [Int]
readInput = readInputParsing "data/day06.txt" csvInts
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[0]
iterate :: Int -> (a -> a) -> a -> [a]
iterate n f x
    | n == 0    = [x]
    | otherwise = x : iterate (n - 1) f (f x)

solutionA :: [Int] -> Int
solutionA = length . last . iterate 80 step
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[1]
rules :: (CApplicative f, ElemCt f Int, Semigroup (f Int)) => Int -> f Int
rules fish
    | fish == 0 = cpure 8 <> cpure 6
    | otherwise = cpure (fish - 1)

step :: (CMonad f, ElemCt f Int, Semigroup (f Int)) => f Int -> f Int
step = cbind rules
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[2]
solutionB :: [Int] -> Integer
solutionB = Tally.size . last . iterate 256 step . Tally.fromList
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
