-- ~\~ language=Haskell filename=app/Day03.hs
-- ~\~ begin <<lit/day03.md|app/Day03.hs>>[0]
module Day03 where

import RIO
import RIO.List.Partial (foldl1)
import Parsing (Parser, sepEndBy1, char, eol, readInputParsing)

import qualified Data.Vector as Vector
import Data.Vector (Vector)

-- ~\~ begin <<lit/day03.md|solution-day-3>>[0]
type Bits = Vector Int

bitSequence :: Parser [Bits]
bitSequence = sepEndBy1 bits eol
    where bits :: Parser Bits
          bits = Vector.fromList
               <$> some (   (char '0' $> 0)
                        <|> (char '1' $> 1))

readInput :: (HasLogFunc env) => RIO env [Bits]
readInput = readInputParsing "data/day03.txt" bitSequence
-- ~\~ end
-- ~\~ begin <<lit/day03.md|solution-day-3>>[1]
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
-- ~\~ end
-- ~\~ begin <<lit/day03.md|solution-day-3>>[2]
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
-- ~\~ end

-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
