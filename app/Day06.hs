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

-- ~\~ begin <<lit/day06.md|parser-day-6>>[0]
csvInts :: Parser [Int]
csvInts = sepEndBy1 integer (lexeme (char ','))

readInput :: (HasLogFunc env) => RIO env [Int]
readInput = readInputParsing "data/day06.txt" csvInts
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[0]
type Tally = Map Int Int

fishCount :: Tally -> Int -> Int
fishCount t s = Map.findWithDefault 0 s t

addFish :: Int -> Int -> Tally -> Tally
addFish k v = Map.alter (Just . maybe v (+ v)) k

init :: [Int] -> Tally
init = foldl (\t k -> addFish k 1 t) Map.empty
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[1]
step :: Tally -> Tally
step t = create $ cycle $ age t
    where age    = Map.delete (-1) . Map.mapKeys (\k -> k - 1)
          cycle  = addFish 6 births
          create = addFish 8 births
          births = fishCount t 0

iterate :: Int -> (a -> a) -> a -> [a]
iterate n f x
    | n == 0    = [x]
    | otherwise = x : iterate (n - 1) f (f x)

totalCount :: Tally -> Int
totalCount = sum . Map.elems

solutionA :: [Int] -> Int
solutionA = totalCount . last . iterate 80 step . init

solutionB :: [Int] -> Int
solutionB = totalCount . last . iterate 256 step . init
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
