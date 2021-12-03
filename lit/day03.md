# Day 3: Binary Diagnostic

``` {.haskell file=app/Day03.hs}
module Day03 where

import RIO
import RIO.List.Partial (foldl1)
import Parsing (Parser, sepEndBy1, char, eol, readInputParsing)

import qualified Data.Vector as Vector
import Data.Vector (Vector)

<<solution-day-3>>

<<run-solutions>>
```

Because of part 2 of this puzzle, I chose to put the bit sequence in a `Vector`.

``` {.haskell #solution-day-3}
type Bits = Vector Int

bitSequence :: Parser [Bits]
bitSequence = sepEndBy1 bits eol
    where bits :: Parser Bits
          bits = Vector.fromList
               <$> some (   (char '0' $> 0)
                        <|> (char '1' $> 1))

readInput :: (HasLogFunc env) => RIO env [Bits]
readInput = readInputParsing "data/day03.txt" bitSequence
```

We need to compute the most common digit for each bit position. I solve this by rounding of the mean bit value.

``` {.haskell #solution-day-3}
fromBinary :: Bits -> Int
fromBinary = go 0 . Vector.toList
    where go n (b:bs) = go (2*n + b) bs
          go n []     = n

invertBinary :: Bits -> Bits
invertBinary = Vector.map (1 -)

mostCommon :: [Bits] -> Bits
mostCommon b = Vector.map ((`div` length b) . (* 2))
             $ foldl1 (Vector.zipWith (+)) b

leastCommon :: [Bits] -> Bits
leastCommon = invertBinary . mostCommon

solutionA :: [Bits] -> Int
solutionA input = gammaRate * epsilonRate
    where gammaRate   = fromBinary mc
          epsilonRate = fromBinary $ invertBinary mc
          mc = mostCommon input
```

In the second part we need to filter down on a single bit in each iteration. The most or least common bit value needs to be computed every time, as it changes when bit sequences are filtered out.

``` {.haskell #solution-day-3}
findRating :: ([Bits] -> Bits) -> Int -> [Bits] -> Bits
findRating _ _   [b]  = b
findRating f idx bits =
    findRating f (idx + 1)
    $ filter (\b -> b Vector.!? idx == mc Vector.!? idx) bits
    where mc = f bits

oxygenGeneratorRating :: [Bits] -> Int
oxygenGeneratorRating = fromBinary . findRating mostCommon 0

co2ScrubberRating :: [Bits] -> Int
co2ScrubberRating = fromBinary . findRating leastCommon 0

solutionB :: [Bits] -> Int
solutionB bits = oxygenGeneratorRating bits * co2ScrubberRating bits
```
 
