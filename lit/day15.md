# Day 15: Chiton
We are given a map and need to compute the shortest path from the top left to the right bottom. The algorithm to use here is [Dijkstra's algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm).

``` {.haskell file=app/Dijkstra.hs}
module Dijkstra (minDist) where

import RIO
import RIO.List.Partial (foldl1')
import qualified RIO.Set as Set
import RIO.Map ((!?))
import qualified RIO.Map as Map

minOn :: Ord b => (a -> b) -> a -> a -> a
minOn f x y
    | f x < f y = x
    | otherwise = y

minDist :: forall n a. (Ord n, Ord a, Num a, Bounded a)
        => [n] -> (n -> [n]) -> (n -> n -> Maybe a) -> n -> n -> a
minDist nodes neighbours distance start end
  = go (Set.fromList nodes) (Set.singleton start) (Map.singleton start 0) start
  where go :: Set n -> Set n -> Map n a -> n -> a
        go unvisited marked dists pos
          | pos == end         = currentDist pos
          | Set.null unvisited = maxBound
          | otherwise          = go (pos `Set.delete` unvisited) newMarked newDists closestUnvisited
          where unvisitedNeighbours = filter (`Set.member` unvisited) (neighbours pos)
                currentDist node = fromMaybe maxBound (dists !? node)

                updateDist :: n -> Maybe a -> Maybe a
                updateDist node Nothing  = (+) <$> (dists !? pos) <*> distance pos node
                updateDist node x        = min <$> updateDist node Nothing <*> x

                newDists = foldr (\k -> Map.alter (updateDist k) k) dists unvisitedNeighbours
                newMarked = (pos `Set.delete` marked) <> Set.fromList unvisitedNeighbours
                closestUnvisited = foldl1' (minOn currentDist) $ Set.toList newMarked
```

``` {.haskell file=app/Day15.hs}
module Day15 where

import RIO
import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
import Parsing (readInputParsing, digitArray)
import Dijkstra (minDist)

<<parser-day15>>

listNodes :: (A.Unbox a) => Array2 a -> [Ix2]
listNodes = A.toList . A.imap const

neighbours :: (A.Unbox a) => Array2 a -> Ix2 -> [Ix2]
neighbours x i = filter (isJust . (x A.!?)) $ map (+ i) n
    where n = [-1 :. 0, 1 :. 0, 0 :. -1, 0 :. 1]

distance :: Array2 Int -> Ix2 -> Ix2 -> Maybe Int
distance x _ i = x A.!? i

endPoint :: Array2 Int -> Ix2
endPoint x = A.unSz (A.size x) - (1 :. 1)

solutionA :: Array2 Int -> Int
solutionA inp = minDist (listNodes inp) (neighbours inp) (distance inp) (0 :. 0) (endPoint inp)

scaleUp :: Array2 Int -> Array2 Int
scaleUp x = stack rowM
    where rowM = foldl' (\r t -> A.compute $ A.append' 1 r t) x (map (\h -> A.map (inc h) x) [1..4])
          stack row = foldl' (\r t -> A.compute $ A.append' 2 r t) row (map (\h -> A.map (inc h) row) [1..4])
          inc h x = (x - 1 + h) `mod` 9 + 1

solutionB :: Array2 Int -> Int
solutionB inp' = minDist (listNodes inp) (neighbours inp) (distance inp) (0 :. 0) (endPoint inp)
    where inp = scaleUp inp'

<<run-solutions>>
```

``` {.haskell #parser-day15}
type Array2' r a = A.Array r A.Ix2 a
type Array2 a = Array2' A.U a

readInput :: (HasLogFunc env) => RIO env (Array2 Int)
readInput = readInputParsing "data/day15.txt" digitArray
```

