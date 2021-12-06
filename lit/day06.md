# Day 6: Lanternfish
We need to simulate the number of lanternfish, each with a timer, spawning new lanternfish etc. Since we have an exponential growth process, to simulate this naively would be stupid, which is kind of the point of the exercise. We only have nine possible states for each fish, so instead we can tally **how many** lanternfish exist in each state.

``` {.haskell file=app/Day06.hs}
module Day06 where

import RIO
import Parsing ( Parser, readInputParsing, sepEndBy1
               , lexeme, char, integer)
import RIO.Map (Map, (!?))
import qualified RIO.Map as Map
<<parser-day-6>>
<<solution-day-6>>
<<run-solutions>>
```

``` {.haskell #parser-day-6}
csvInts :: Parser [Int]
csvInts = sepEndBy1 integer (lexeme (char ','))

readInput :: (HasLogFunc env) => RIO env [Int]
readInput = readInputParsing "data/day06.txt" csvInts
```

``` {.haskell #solution-day-6}
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
```


