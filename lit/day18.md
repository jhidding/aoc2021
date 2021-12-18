# Day 18: Snailfish
Today we're walking trees. I spent most of my time reading the instructions. My solution is based on clever (if I can say so myself) combination of `Alternative` and continuation passing style.

``` {.haskell file=app/Day18.hs}
module Day18 where

import RIO
import RIO.List.Partial (foldl1', maximum)
import Parsing (Parser, readInputParsing, char, integer, eol, sepEndBy1)

<<parser-day18>>
<<solution-day18>>
<<run-solutions>>
```

We have snailfish "numbers" that are represented by pairs of snailfish "numbers".

``` {.haskell #parser-day18}
data Number a
    = Regular a
    | Snailfish (Number a) (Number a)
    deriving (Eq)

instance (Show a) => Show (Number a) where
    show (Regular a) = show a
    show (Snailfish a b) = "[" <> show a <> "," <> show b <> "]"

snailfishP :: Parser (Number Int)
snailfishP = Snailfish <$ char '[' <*> exprP <* char ',' <*> exprP <* char ']'

exprP :: Parser (Number Int)
exprP = (Regular <$> integer) <|> snailfishP

readInput :: (HasLogFunc env) => RIO env [Number Int]
readInput = readInputParsing "data/day18.txt" (snailfishP `sepEndBy1` eol)
```

We are told that numbers are added by creating a new pair and then reducing.

``` {.haskell #solution-day18}
(<+>) :: Number Int -> Number Int -> Number Int
a <+> b = reduce $ Snailfish a b
```

To reduce a number, we either explode or split. It took me a long time to understand that we don't split unless there's nothing to explode.

``` {.haskell #solution-day18}
reduce :: Number Int -> Number Int
reduce a = maybe a reduce (reduceExplode a <|> reduceSplit a)
```

The idea of `Alternative Maybe` and CPS is best explained on `reduceSplit`. Once we found a number to split, we should stop scanning for other numbers to split. This means we have to represent upper levels of the tree in terms of what happens somewhere down in the walk. Instead of waiting for a function to return, I pass it a continuation (think a template) that we may use **if and only if** we want to change something in the tree. The continuation should be a function that, when given a node, reconstructs the entire tree.

Calling the continuation will result in a `Just` that already contains the entire tree. If the continuation is not called, the result is a `Nothing`, telling the `Alternative` class that we're not done yet.

``` {.haskell #solution-day18}
reduceSplit :: Number Int -> Maybe (Number Int)
reduceSplit = walk Just
  where walk cc (Snailfish a b)
          =   walk (\n -> cc $ Snailfish n b) a
          <|> walk (\n -> cc $ Snailfish a n) b
        walk cc (Regular x)
          | x >= 10    = cc $ split x
          | otherwise  = Nothing
          where split x = Snailfish (Regular $ floor (fromIntegral x / 2))
                                    (Regular $ ceiling (fromIntegral x / 2))
```

The same principle applies on the `reduceExplode` function. However, now it is more complicated. Next to replacing the current node with `Regular 0`, we need to add numbers to the left and right. If a sub-tree is unmodified, we may decide to add `0` after all.

``` {.haskell #solution-day18}
reduceExplode :: Number Int -> Maybe (Number Int)
reduceExplode = walk 0 (\_ x _  -> Just x)
  where walk 4 explode (Snailfish (Regular a) (Regular b))
            = explode a (Regular 0) b
        walk depth explode (Snailfish a b)
            =   walk (depth+1) explodeLeft a
            <|> walk (depth+1) explodeRight b
            where explodeLeft x n y = 
                    explode x (Snailfish n (addToLeftMost b y)) 0
                  explodeRight x n y =
                    explode 0 (Snailfish (addToRightMost a x) n) y
        walk _ _ (Regular _) = Nothing
```

The `addToLeftMost` and `addToRightMost` functions to a normal recursive decent, optimising for the common case of adding `0`.

``` {.haskell #solution-day18}
addToLeftMost :: Number Int -> Int -> Number Int
addToLeftMost a 0 = a
addToLeftMost a x = go a
    where go (Snailfish a b) = Snailfish (go a) b
          go (Regular y) = Regular (x + y)

addToRightMost :: Number Int -> Int -> Number Int
addToRightMost a 0 = a
addToRightMost a x = go a
    where go (Snailfish a b) = Snailfish a (go b)
          go (Regular y) = Regular (x + y)
```

With all that in place, the rest of the exercise is not too hard.

``` {.haskell #solution-day18}
magnitude :: Number Int -> Int
magnitude (Regular x) = x
magnitude (Snailfish a b) = magnitude a * 3 + magnitude b * 2

solutionA :: [Number Int] -> Int
solutionA = magnitude . foldl1' (<+>)

solutionB :: [Number Int] -> Int
solutionB inp = maximum [ magnitude (a <+> b) 
                        | a <- inp, b <- inp, a /= b]
```
