-- ~\~ language=Haskell filename=app/Day14.hs
-- ~\~ begin <<lit/day14.md|app/Day14.hs>>[0]
module Day14 where

import RIO
import RIO.List (iterate, sortOn)
import RIO.List.Partial (tail, init, head, last, (!!))
import RIO.Map ((!?))
import qualified RIO.Map as Map
import qualified Data.Map.Lazy as LazyMap
import Parsing (Parser, readInputParsing, sepEndBy1, eol, string)
import Text.Megaparsec.Char (upperChar)
import Data.MultiSet (MultiSet, occur, findMin, findMax)
import qualified Data.MultiSet as MultiSet

type LazyMap = LazyMap.Map

-- ~\~ begin <<lit/day14.md|parser-day14>>[0]
data Input = Input
    { axiom :: [Char]
    , rules :: Map (Char, Char) Char
    } deriving (Show)

axiomP :: Parser [Char]
axiomP = some upperChar <* eol

ruleP :: Parser ((Char, Char), Char)
ruleP = (,) <$> ((,) <$> upperChar <*> upperChar)
            <*  string " -> " <*> upperChar

inputP :: Parser Input
inputP = Input <$> axiomP <* eol <*> (Map.fromList <$> sepEndBy1 ruleP eol)

readInput :: (HasLogFunc env) => RIO env Input
readInput = readInputParsing "data/day14.txt" inputP
-- ~\~ end
-- ~\~ begin <<lit/day14.md|solution-day14>>[0]
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

newtype GlueList a = GlueList { fromGlueList :: [a] }

instance Semigroup (GlueList a) where
    GlueList (x:xs) <> GlueList (_:y)  = GlueList $ (x:xs) <> y
    GlueList x <> GlueList [] = GlueList x
    GlueList [] <> GlueList y = GlueList y

instance Monoid (GlueList a) where
    mempty = GlueList mempty

step :: Map (Char, Char) Char -> [Char] -> [Char]
step rules = fromGlueList . foldMap insertChar . pairs
    where insertChar (a, b) = case rules !? (a, b) of
                                Nothing -> GlueList [a, b]
                                Just c  -> GlueList [a, c, b]

countDiff :: [Char] -> Int
countDiff [] = 0
countDiff cs = snd (last counts) - snd (head counts)
    where counts = sortOn snd $ MultiSet.toOccurList $ MultiSet.fromList cs

solutionA :: Input -> Int
solutionA Input {..} = countDiff $ (!! 10) $ iterate (step rules) axiom

countMap :: Map (Char, Char) Char -> LazyMap ((Char, Char), Int) (MultiSet Char)
countMap rules = m
    where m = LazyMap.fromList [ (((c1, c2), d), f c1 c2 d) 
                               | c1 <- ['A'..'Z']
                               , c2 <- ['A'..'Z']
                               , d <- [0..40]]
          f :: Char -> Char -> Int -> MultiSet Char
          f c1 _ 0  = MultiSet.singleton c1
          f c1 c3 d = case rules !? (c1, c3) of
                        Nothing -> MultiSet.singleton c1
                        Just c2 -> m LazyMap.! ((c1, c2), d - 1)
                                <> m LazyMap.! ((c2, c3), d - 1)

solutionB :: Input -> Int
solutionB Input{..} = snd (last counts) - snd (head counts)
    where counts = sortOn snd $ MultiSet.toOccurList 
                 $ MultiSet.singleton (last axiom)
                 <> foldMap (\p -> m LazyMap.! (p, 40)) (pairs axiom)
          m = countMap rules
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
