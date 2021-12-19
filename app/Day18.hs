-- ~\~ language=Haskell filename=app/Day18.hs
-- ~\~ begin <<lit/day18.md|app/Day18.hs>>[0]
module Day18 where

import RIO
import RIO.List.Partial (foldl1', maximum)
import Parsing (Parser, readInputParsing, char, integer, eol, sepEndBy1)

-- ~\~ begin <<lit/day18.md|parser-day18>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/day18.md|solution-day18>>[0]
instance Semigroup (Number Int) where
    Regular 0 <> b = b
    a <> Regular 0 = a
    a <> b = reduce $ Snailfish a b

instance Monoid (Number Int) where
    mempty = Regular 0
-- ~\~ end
-- ~\~ begin <<lit/day18.md|solution-day18>>[1]
reduce :: Number Int -> Number Int
reduce a = maybe a reduce (reduceExplode a <|> reduceSplit a)
-- ~\~ end
-- ~\~ begin <<lit/day18.md|solution-day18>>[2]
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
-- ~\~ end
-- ~\~ begin <<lit/day18.md|solution-day18>>[3]
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
-- ~\~ end
-- ~\~ begin <<lit/day18.md|solution-day18>>[4]
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
-- ~\~ end
-- ~\~ begin <<lit/day18.md|solution-day18>>[5]
magnitude :: Number Int -> Int
magnitude (Regular x) = x
magnitude (Snailfish a b) = magnitude a * 3 + magnitude b * 2

solutionA :: [Number Int] -> Int
solutionA = magnitude . fold

solutionB :: [Number Int] -> Int
solutionB inp = maximum [ magnitude (a <> b)
                        | a <- inp, b <- inp, a /= b]
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
