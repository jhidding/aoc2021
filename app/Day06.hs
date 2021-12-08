-- ~\~ language=Haskell filename=app/Day06.hs
-- ~\~ begin <<lit/day06.md|app/Day06.hs>>[0]
module Day06 where

import RIO
import RIO.List (foldl)
import RIO.List.Partial (last)
import Parsing ( Parser, readInputParsing, sepEndBy1
               , lexeme, char, integer)
import RIO.Map (Map, (!?))
import qualified RIO.Map as Map
import Data.Constraint (Constraint)

-- ~\~ begin <<lit/day06.md|parser-day-6>>[0]
csvInts :: Parser [Int]
csvInts = sepEndBy1 integer (lexeme (char ','))

readInput :: (HasLogFunc env) => RIO env [Int]
readInput = readInputParsing "data/day06.txt" csvInts
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[0]
iterate :: Int -> (a -> a) -> a -> [a]
iterate n f x
    | n == 0    = [x]
    | otherwise = x : iterate (n - 1) f (f x)

solutionA :: [Int] -> Int
solutionA = length . last . iterate 80 step
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[1]
newtype Tally a = Tally { tallyMap :: Map a Int }
    deriving (Show)
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[2]
instance (Ord a) => Semigroup (Tally a) where
    Tally a <> Tally b = Tally $ Map.unionWith (+) a b

instance (Ord a) => Monoid (Tally a) where
    mempty = Tally mempty
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[3]
class CFunctor f where
    type ElemCt f a :: Constraint
    cmap :: (ElemCt f a, ElemCt f b) => (a -> b) -> f a -> f b

class CFunctor f => CApplicative f where
    cpure :: (ElemCt f a) => a -> f a
    cliftA2 :: (ElemCt f a, ElemCt f b, ElemCt f c)
            => (a -> b -> c) -> f a -> f b -> f c
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[4]
class CApplicative f => CMonad f where
    cbind :: (ElemCt f a, ElemCt f b) => (a -> f b) -> f a -> f b
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[5]
rules :: (CApplicative f, ElemCt f Int, Semigroup (f Int)) => Int -> f Int
rules fish
    | fish == 0 = cpure 8 <> cpure 6
    | otherwise = cpure (fish - 1)

step :: (CMonad f, ElemCt f Int, Semigroup (f Int)) => f Int -> f Int
step = cbind rules
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[6]
class EmptyCt a
instance EmptyCt a
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[7]
instance CFunctor [] where
    type ElemCt [] a = EmptyCt a
    cmap = fmap

instance CApplicative [] where
    cpure = pure
    cliftA2 = liftA2

instance CMonad [] where
    cbind = (=<<)
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[8]
instance CFunctor Tally where
    type ElemCt Tally a = Ord a
    cmap f (Tally a) = Tally (Map.mapKeys f a)

multiply :: Tally a -> Int -> Tally a
multiply (Tally a) n = Tally (Map.map (* n) a)

instance CApplicative Tally where
    cpure a = Tally $ Map.singleton a 1
    cliftA2 f (Tally a) b = Map.foldMapWithKey
            (\k v -> multiply (cmap (f k) b) v) a

instance CMonad Tally where
    cbind f (Tally a) = Map.foldMapWithKey (multiply . f) a
-- ~\~ end
-- ~\~ begin <<lit/day06.md|solution-day-6>>[9]
tallyLength :: Tally a -> Int
tallyLength (Tally a) = sum $ Map.elems a

tallyFromList :: Ord a => [a] -> Tally a
tallyFromList = foldMap cpure

solutionB :: [Int] -> Int
solutionB = tallyLength . last . iterate 256 step . tallyFromList
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
