# Day 19: Beacon Scanner
Boy, this was a hard one.

``` {.haskell file=app/Day19.hs}
module Day19 where

import RIO hiding (try)
import RIO.List (sortBy, find, sort)
import RIO.List.Partial (head, last, maximum)
import qualified RIO.Map as Map
import qualified Data.Set as Set
import qualified Data.Map.Lazy as LazyMap
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Parsing (Parser, readInputParsing, string, integer, char, eol, sepEndBy1, dropUntilEol)
import Text.Megaparsec (try)

import Linear.Matrix ( M33, (!*), (!!*), (!*!), transpose )
import Linear.V3 ( V3(..), _x, _y, _z )
import Linear.Vector ( negated )

<<data-types-day19>>
<<parser-day19>>
<<solution-day19>>
<<run-solutions>>
```

For this problem I use the `Linear` module quite a lot: `V3 Int` for coordinates, `V3 (V3 Int)` for rotating and reflecting the coordinates. I defined my own `Affine` type here, I know `Linear` also has one, but this kind of grew and I don't know if it is used in a similar way. The `Affine` type combines a coordinate transformation and an offset. I implemented `Monoid` to get the `<>` operator to combine `Affine` transformations.

``` {.haskell #data-types-day19}
type Pt = V3 Int
type Scan = Set Pt

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
```

``` {.haskell #parser-day19}
inputP :: Parser Scan
inputP = Set.fromList <$> (string "---" >> dropUntilEol
       >> (V3 <$> integer <* char ',' <*> integer <* char ',' <*> integer) `sepEndBy1` eol)

readInput :: (HasLogFunc env) => RIO env (Vector Scan)
readInput = readInputParsing "data/day19.txt" (Vector.fromList <$> inputP `sepEndBy1` eol)
```

To find if two scans have matching points, I try all transpositions and reflections of coordinates. This may not be the most compact way of writing these down, but it works.

``` {.haskell #solution-day19}
allTransforms :: [Transform]
allTransforms = [ p * s | p <- permutations (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
                        , s <- signatures ]
    where
          permutations a b c = [ V3 a b c, V3 a c b, V3 b a c
                               , V3 b c a, V3 c a b, V3 c b a ]
          signatures = [ V3 1 1 1,       V3 1 1 (-1)
                       , V3 1 (-1) 1,    V3 1 (-1) (-1)
                       , V3 (-1) 1 1,    V3 (-1) 1 (-1)
                       , V3 (-1) (-1) 1, V3 (-1) (-1) (-1) ]
```

To see if two scans match (in their current orientation), I have to translate one by some coordinate and see if more than 12 points line up. To get this with decent speed, I do this first along every axis, and then try combinations of those. I tried doing this with `IntSet` first, but the problem is then, that values can appear multiple times. I've thought about using `MultiSet` from our previous problems. We are looking at lists of 25 numbers each, so I expect that a normal `[Int]` is fine. Still need an `intersection` function though. This assumes that input lists are sorted.

``` {.haskell #solution-day19}
intersect :: (Ord a) => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (a:as) (b:bs)
    | a == b    = a : intersect as bs
    | a < b     = intersect as (b:bs)
    | otherwise = intersect (a:as) bs
```

Now, I can implement a function that takes two sorted lists, and returns a list of `(offset, overlap)` pairs, sorted such that the offset with largest overlap is returned first.

``` {.haskell #solution-day19}
matchInts :: [Int] -> [Int] -> [(Int, Int)]
matchInts a b = sortBy (compare `on` (negate . snd))
              $ map overlap range
    where range = [(head a - last b) .. (last a - head b)]
          overlap i = (i, length $ intersect a (map (+ i) b))
```

The trick is now to combine offsets found for each axis. I make a list of candidates by taking the cartesian product of matches from each axis. The first candidate to yield an intersection larger or equal than 12 points gets through.

``` {.haskell #solution-day19}
matchScans :: Set Pt -> Set Pt -> Maybe Pt
matchScans a b = find approve candidates
    where candidates = V3 <$> matchOn _x <*> matchOn _y <*> matchOn _z
          approve d = Set.size (Set.intersection a (Set.map (+ d) b)) >= 12
          matchOn coord = map fst
                        $ takeWhile (\(i, s) -> s >= 12)
                        $ matchInts (sort $ map (view coord) $ Set.toList a)
                                    (sort $ map (view coord) $ Set.toList b)
```

We still have to try this for every rotation and reflection of one of the scans.

``` {.haskell #solution-day19}
match :: Set Pt -> Set Pt -> Maybe Affine
match a b = asum (go <$> allTransforms)
    where go t = Affine t <$> matchScans a (Set.map (t !*) b)
```

I build an index of `Affine` transformations. Starting with scan 0, I find the first remaining scan that produces a match, add that to the map and repeat. This could be sped up by memoizing matches we already know to fail; for me this gives a factor 60 speedup.

``` {.haskell #solution-day19}
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
```

That was the hard bit. This code runs in about 30 seconds on my laptop.

``` {.haskell #solution-day19}
mergeScans :: Vector Scan -> Map Int Affine -> Scan
mergeScans s = Map.foldMapWithKey (\i a -> Set.map (applyAffine a) (s Vector.! i))

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
```
