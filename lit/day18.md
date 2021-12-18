# Day 18: Snailfish

``` {.haskell file=app/Day18.hs}
module Day18 where

import RIO
import RIO.List.Partial (foldl1', maximum)
import Parsing (Parser, readInputParsing, char, integer, eol, sepEndBy1)
import Text.Megaparsec (parse)

<<parser-day18>>
<<solution-day18>>
<<run-solutions>>
```

``` {.haskell #parser-day18}
data Number a
    = Regular a
    | Snailfish (Number a) (Number a)
    deriving (Eq)

instance (Show a) => Show (Number a) where
    show (Regular a) = show a
    show (Snailfish a b) = "[" <> show a <> "," <> show b <> "]"

(<+>) :: Number Int -> Number Int -> Number Int
a <+> b = reduce $ Snailfish a b

split :: Int -> Number Int
split a = Snailfish (Regular $ floor (fromIntegral a / 2))
                    (Regular $ ceiling (fromIntegral a / 2))

leftMost :: Number Int -> Int
leftMost (Regular a) = a
leftMost (Snailfish a b) = leftMost b

rightMost :: Number Int -> Int
rightMost (Regular a) = a
rightMost (Snailfish a b) = rightMost a

addToLeftMost :: Number Int -> Int -> Number Int
addToLeftMost a x = go a
    where go (Snailfish a b) = Snailfish (go a) b
          go (Regular y) = Regular (x + y)

addToRightMost :: Number Int -> Int -> Number Int
addToRightMost a x = go a
    where go (Snailfish a b) = Snailfish a (go b)
          go (Regular y) = Regular (x + y)

reduce :: Number Int -> Number Int
reduce a = maybe a reduce (   (walkExplode 0 (\_ x _ -> Just x) a)
                          <|> (walkSplit Just a))
    where walkExplode 4 explode (Snailfish (Regular a) (Regular b))
            = explode a (Regular 0) b
          walkExplode depth explode (Snailfish a b)
            =   walkExplode (depth+1) explodeLeft a
            <|> walkExplode (depth+1) explodeRight b
            where explodeLeft x n y = 
                    explode x (Snailfish n (addToLeftMost b y)) 0
                  explodeRight x n y =
                    explode 0 (Snailfish (addToRightMost a x) n) y
          walkExplode _ _ (Regular _) = Nothing

          walkSplit cc (Snailfish a b)
            = walkSplit (\n -> cc $ Snailfish n b) a
            <|> walkSplit (\n -> cc $ Snailfish a n) b
          walkSplit cc (Regular x)
            | x >= 10    = cc $ split x
            | otherwise  = Nothing

magnitude :: Number Int -> Int
magnitude (Regular x) = x
magnitude (Snailfish a b) = magnitude a * 3 + magnitude b * 2

snailfishP :: Parser (Number Int)
snailfishP = Snailfish <$ char '[' <*> exprP <* char ',' <*> exprP <* char ']'

exprP :: Parser (Number Int)
exprP = (Regular <$> integer) <|> snailfishP

readInput :: (HasLogFunc env) => RIO env [Number Int]
readInput = readInputParsing "data/day18.txt" (snailfishP `sepEndBy1` eol)

read :: Text -> Number Int
read s = fromRight (Regular (-1)) $ parse snailfishP "-" s
```

``` {.haskell #solution-day18}
solutionA :: [Number Int] -> Int
solutionA = magnitude . foldl1' (<+>)

solutionB :: [Number Int] -> Int
solutionB inp = maximum [magnitude (a <+> b) | a <- inp, b <- inp, a /= b]
```
