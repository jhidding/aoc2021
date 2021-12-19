# Day 19: Beacon Scanner

``` {.haskell file=app/Day19.hs}
module Day19 where

import RIO hiding (try)
import Parsing (Parser, readInputParsing, string, integer, char, eol, sepEndBy1, dropUntilEol)
import Text.Megaparsec (try)

import Linear.V3 ( V3(..) )

type Scan = [V3 Int]

inputP :: Parser Scan
inputP = string "---" >> dropUntilEol
       >> (V3 <$> integer <* char ',' <*> integer <* char ',' <*> integer) `sepEndBy1` eol

readInput :: (HasLogFunc env) => RIO env [Scan]
readInput = readInputParsing "data/day19.txt" (inputP `sepEndBy1` eol)

type Transform = M33

allTransforms :: [Transform]
allTransforms = 

class Similarity a where
    (<~>) :: a -> a -> Bool

instance Similarity Int where
    a <~> b = abs a == abs b

solutionA :: [Scan] -> [Scan]
solutionA = id

solutionB :: [Scan] -> Int
solutionB = length


<<run-solutions>>
```
