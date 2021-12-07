# Day 7: The Treachery of Whales
But I like whales! We need to minimize a cost function. We have a list of integers, so we can reuse the parser from Day 6.

``` {.haskell file=app/Day07.hs}
module Day07 where

import RIO
import RIO.List (sort)
import RIO.List.Partial ((!!))
import Parsing ( Parser, readInputParsing, sepEndBy1
               , lexeme, char, integer)

<<parser-day-7>>
<<solution-day-7>>
<<run-solutions>>
```

``` {.haskell #parser-day-7}
csvInts :: Parser [Int]
csvInts = sepEndBy1 integer (lexeme (char ','))

readInput :: (HasLogFunc env) => RIO env [Int]
readInput = readInputParsing "data/day07.txt" csvInts
```

We minimize the function,

$$f_a(x) = \sum_{i=1}^n |c_i - x|.$$

We know that the solution should be translation invariant. For $n=2$ the cost function is equal at any point in between, only for $n=3$ do we start to get a minimum, at the center most point. That would suggest a median. If we remove the outer two most points, the answer stays the same, repeat and we arrive at the center most point. Proven! Since we're only interested in the value attained at the minimum, it doesn't matter if we take the upper or lower median for even length sequences.

``` {.haskell #solution-day-7}
costFuncA :: [Int] -> Int -> Int
costFuncA cs x = sum (map (abs . (x -)) cs)

median :: [Int] -> Int
median x = sort x !! (length x `div` 2)

solutionA :: [Int] -> (Int, Int)
solutionA as = (loc, costFuncA as loc)
    where loc = sort as !! (length as `div` 2)
```

For part B, we get a distance function that goes like $\sum_{i=1}^d d = d (d + 1) / 2$, where $d = c_i - x$. Since the cost function now goes with distance square, we arrive at a minimum at the mean $x = \langle c_i \rangle$. I don't have a proof for this, but it gives me the right answer. We would need to show that,

$$f_b(x) = \sum_{i=1}^n |c_i - x| (|c_i -x| + 1) / 2 \sim (x - \langle c_i \rangle)^2 + {\rm const},$$

or something of that form. To finish later today.

``` {.haskell #solution-day-7}
costFuncB :: [Int] -> Int -> Int
costFuncB cs x = sum (map f cs)
    where f c = abs (x - c) * (abs (x - c) + 1) `div` 2

mean :: [Int] -> Int
mean x = sum x `div` length x

solutionB :: [Int] -> (Int, Int)
solutionB x = (loc, costFuncB x loc)
    where loc = mean x
```
