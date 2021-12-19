-- ~\~ language=Haskell filename=app/Day19.hs
-- ~\~ begin <<lit/day19.md|app/Day19.hs>>[0]
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


-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
