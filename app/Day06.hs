-- ~\~ language=Haskell filename=app/Day06.hs
-- ~\~ begin <<lit/day06.md|app/Day06.hs>>[0]
module Day06 where

import RIO
import Parsing ( Parser, readInputParsing, sepEndBy1
               , lexeme, char, integer)
import RIO.Map (Map, (!?))
import qualified RIO.Map as Map
-- ~\~ begin <<lit/day06.md|parser-day-6>>[0]
csvInts :: Parser [Int]
csvInts = sepEndBy1 integer (lexeme (char ','))

readInput :: (HasLogFunc env) => RIO env [Int]
readInput = readInputParsing "data/day06.txt" csvInts
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[0]
type Tally = Map Int Int

init :: [Int] -> Tally
init = foldl count
--    where count m i = 

step :: Tally -> Tally
step (Tally x0 x1 x2 x3 x4 x5 x6 x7 x8) = 
    Tally x1 x2 x3 x4 x5 x6 (x7 + x0) x8 x0

solutionA :: [Int] -> Int
solutionA = undefined

solutionB :: [Int] -> Int
solutionB = undefined
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
