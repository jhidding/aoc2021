# Day 1: Sonar Sweep
It seems we are going on a deep dive this year! We are given the height data of a sonar sweep of the bottom of the sea.

``` {.haskell file=app/Day01.hs}
module Day01 where

import RIO
import qualified RIO.Text as Text

readInput :: MonadIO m => m [Int]
readInput = do
    text <- Text.lines <$> readFileUtf8 "data/day01.txt"
    return $ mapMaybe (readMaybe . Text.unpack) text

<<solution-day-1>>
<<run-solutions>>
```

``` {.gnuplot action=plot output=fig/day01.svg}
My input data
---
plot "data/day01.txt" w l ls 1
```

The question is: how often do we find this sequence ascending? We may solve this by computing the difference between each consecutive element in the input list. Then we need to know the number of possitive numbers in the difference list:

``` {.haskell #solution-day-1}
solutionA :: [Int] -> Int
solutionA = length . filter (> 0) . diff
    where diff (a1:a2:as) = a2 - a1 : diff (a2:as)
          diff _          = []
```

In the second part we need to do a sliding sum over the list of input numbers, reducing the problem to that of part A:

``` {.haskell #solution-day-1}
solutionB :: [Int] -> Int
solutionB = solutionA . slidingSum
    where slidingSum (a1:a2:a3:as) = a1 + a2 + a3 : slidingSum (a2:a3:as)
          slidingSum _             = []
```

In hindsight, a more efficient solution would be:

``` {.haskell}
solutionB = length . filter (> 0) . diff3
    where diff3 (a1:a2:a3:a4:as) = a4 - a1 : diff3 (a2:a3:a4:as)
          diff3 _                = []
```

The middle terms in the finite difference drop out.
