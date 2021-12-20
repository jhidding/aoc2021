# Day 20: Trench Map
It's game of life time!

``` {.haskell file=app/Day20.hs}
module Day20 where

import RIO
import RIO.List (iterate)
import RIO.List.Partial ((!!))
import Data.Massiv.Array (Ix1, Ix2(..), U, Sz(..))
import qualified Data.Massiv.Array as A

import Parsing (Parser, readInputParsing, char, failOnException, sepEndBy1, eol)

<<parser-day20>>
<<solution-day20>>
<<run-solutions>>
```

``` {.haskell #parser-day20}
type Array1 a = A.Array U Ix1 a
type Array2 a = A.Array U Ix2 a
type Input = (Array1 Int, Array2 Int)

lineP :: Parser [Int]
lineP = some ((char '.' $> 0) <|> (char '#' $> 1))

rulesP :: Parser (Array1 Int)
rulesP = A.fromList A.Seq . join <$> (lineP `sepEndBy1` eol)

gridP :: Parser (Array2 Int)
gridP = failOnException . A.fromListsM A.Seq =<< (lineP `sepEndBy1` eol)

readInput :: (HasLogFunc env) => RIO env Input
readInput = readInputParsing "data/day20.txt"
            ((,) <$> rulesP <* eol <*> gridP)
```

Little comment needed. Take care with the value at infinity!

``` {.haskell #solution-day20}
patch :: [Ix2]
patch = [i :. j | i <- [(-1)..1], j <- [(-1)..1]]

fromBinary :: [Int] -> Int
fromBinary = go 0
    where go n (b:bs) = go (2*n + b) bs
          go n []     = n

ruleStencil :: Array1 Int -> A.Stencil Ix2 Int Int
ruleStencil rules = A.makeStencil (Sz $ 3 :. 3) (1 :. 1) go
    where go get = rules A.! idx
            where idx = fromBinary (map get patch)

growBorder :: Array2 Int -> Int -> Array2 Int
growBorder src inf = A.makeArrayR A.U A.Seq (A.liftSz (+ 2) (A.size src))
                    (\ix -> fromMaybe inf $ src A.!? (ix - (1 :. 1)))

step :: Array1 Int -> (Array2 Int, Int) -> (Array2 Int, Int)
step rules (src, inf) = (tgt, inf')
    where tgt  = A.compute $ A.mapStencil (A.Fill inf) (ruleStencil rules) (growBorder src inf)
          inf' = if inf == 0 then rules A.! 0 else rules A.! 511

solutionA :: Input -> Int
solutionA (rules, src) = A.sum $ fst $ step' $ step' (src, 0)
    where step' = step rules

solutionB :: Input -> Int
solutionB (rules, src) = A.sum $ fst $ (!! 50) $ iterate (step rules) (src, 0)
```


