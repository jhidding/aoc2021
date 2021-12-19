-- ~\~ language=Haskell filename=app/Day19.hs
-- ~\~ begin <<lit/day19.md|app/Day19.hs>>[0]
module Day19 where

import RIO hiding (try)
import RIO.List (sortBy, find, sort, headMaybe)
import RIO.List.Partial (head, last, maximum)
import qualified RIO.Map as Map
import qualified Data.Set as Set
import qualified Data.Map.Lazy as LazyMap
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Parsing (Parser, readInputParsing, string, integer, char, eol, sepEndBy1, dropUntilEol)
import Text.Megaparsec (try)

import Linear.Matrix ( M33, (!*), (!!*), (!*!), transpose, det33 )
import Linear.V3 ( V3(..), _x, _y, _z )
import Linear.Vector ( negated )

-- ~\~ begin <<lit/day19.md|data-types-day19>>[0]
type Pt = V3 Int
type Scan = [Pt]

type Transform = M33 Int
data Affine = Affine Transform Pt
    deriving (Show)

instance Semigroup Affine where
    Affine t p <> Affine u q = Affine (t !*! u) (t !* q + p)

instance Monoid Affine where
    mempty = Affine (V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1))
                    (V3 0 0 0)

invert :: Affine -> Affine
invert (Affine t p) = Affine (transpose t) (negated p)

applyAffine :: Affine -> Pt -> Pt
applyAffine (Affine t p) q = t !* q + p
-- ~\~ end
-- ~\~ begin <<lit/day19.md|parser-day19>>[0]
inputP :: Parser Scan
inputP = string "---" >> dropUntilEol
       >> (V3 <$> integer <* char ',' <*> integer <* char ',' <*> integer) `sepEndBy1` eol

readInput :: (HasLogFunc env) => RIO env (Vector Scan)
readInput = readInputParsing "data/day19.txt" (Vector.fromList <$> inputP `sepEndBy1` eol)
-- ~\~ end
-- ~\~ begin <<lit/day19.md|solution-day19>>[0]
allTransforms :: [Transform]
allTransforms = [ p * s | p <- permutations (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
                        , s <- signatures
                        , det33 (p * s) == 1 ]
    where
          permutations a b c = [ V3 a b c, V3 a c b, V3 b a c
                               , V3 b c a, V3 c a b, V3 c b a ]
          signatures = [ V3 1 1 1,       V3 1 1 (-1)
                       , V3 1 (-1) 1,    V3 1 (-1) (-1)
                       , V3 (-1) 1 1,    V3 (-1) 1 (-1)
                       , V3 (-1) (-1) 1, V3 (-1) (-1) (-1) ]
-- ~\~ end
-- ~\~ begin <<lit/day19.md|solution-day19>>[1]
matchScans :: [Pt] -> [Pt] -> Maybe Pt
matchScans a b = headMaybe $ Map.keys $ Map.filter (>= 12) $ count diffs
    where diffs = (-) <$> a <*> b
          count = Map.fromListWith (+) . map (,1)
-- ~\~ end
-- ~\~ begin <<lit/day19.md|solution-day19>>[2]
match :: [Pt] -> [Pt] -> Maybe Affine
match a b = asum (go <$> allTransforms)
    where go t = Affine t <$> matchScans a (map (t !*) b)
-- ~\~ end
-- ~\~ begin <<lit/day19.md|solution-day19>>[3]
memoizeMatch :: Vector Scan -> Int -> Int -> Maybe Affine
memoizeMatch s = lookup
    where lookup i j = join $ LazyMap.lookup (i, j) cache
          cache = LazyMap.fromList [ ((i, j), match (s Vector.! i) (s Vector.! j))
                                   | i <- [0 .. Vector.length s - 1]
                                   , j <- [0 .. Vector.length s - 1]
                                   , i /= j ]

buildMap :: (Int -> Int -> Maybe Affine) -> Int -> Map Int Affine -> Maybe (Map Int Affine)
buildMap f n m
    | Map.size m == n = Just m
    | otherwise = asum matches >>= insert >>= buildMap f n
    where insert (i, j, aff) = do { a <- m Map.!? i;
                                    return $ Map.insert j (a <> aff) m }
          matches = [ (i, j,) <$> f i j
                    | i <- Map.keys m
                    , j <- [0..(n - 1)]
                    , j `Map.notMember` m ]
-- ~\~ end
-- ~\~ begin <<lit/day19.md|solution-day19>>[4]
mergeScans :: Vector Scan -> Map Int Affine -> Set Pt
mergeScans s = Map.foldMapWithKey (\i a -> Set.fromList $ map (applyAffine a) (s Vector.! i))

solutionA :: Vector Scan -> Maybe Int
solutionA inp = Set.size . mergeScans inp
            <$> buildMap (memoizeMatch inp) (Vector.length inp)
                         (Map.singleton 0 mempty)

maxDist :: Map Int Affine -> Int
maxDist m = maximum [dist a b | a <- Map.elems m, b <- Map.elems m]
    where dist (Affine _ a) (Affine _ b) = sum (abs (a - b))

solutionB :: Vector Scan -> Maybe Int
solutionB inp = maxDist
            <$> buildMap (memoizeMatch inp) (Vector.length inp)
                         (Map.singleton 0 mempty)
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
