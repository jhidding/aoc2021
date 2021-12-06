# Day 6: Lanternfish
We need to simulate the number of lanternfish, each with a timer, spawning new lanternfish etc. Since we have an exponential growth process, to simulate this naively would be stupid, which is kind of the point of the exercise. We only have nine possible states for each fish, so instead we can tally **how many** lanternfish exist in each state.

``` {.haskell file=app/Day06.hs}
module Day06 where

import RIO
import RIO.List (foldl)
import RIO.List.Partial (last)
import Parsing ( Parser, readInputParsing, sepEndBy1
               , lexeme, char, integer)
import RIO.Map (Map, (!?))
import qualified RIO.Map as Map

<<parser-day-6>>
<<solution-day-6>>
<<run-solutions>>
```

As always, we first parse the input:

``` {.haskell #parser-day-6}
csvInts :: Parser [Int]
csvInts = sepEndBy1 integer (lexeme (char ','))

readInput :: (HasLogFunc env) => RIO env [Int]
readInput = readInputParsing "data/day06.txt" csvInts
```

For tallying the amount of lanternfish in each state, I use a `Map Int Int`. I've considered `List` and `Vector`, but this is the least amount of hassle.

``` {.haskell #solution-day-6}
type Tally = Map Int Int

fishCount :: Tally -> Int -> Int
fishCount t s = Map.findWithDefault 0 s t

addFish :: Int -> Int -> Tally -> Tally
addFish k v = Map.alter (Just . maybe v (+ v)) k

init :: [Int] -> Tally
init = foldl (\t k -> addFish k 1 t) Map.empty
```

We can age the fish, by mapping the keys, and deleting the -1 entry.

``` {.haskell #solution-day-6}
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
```

