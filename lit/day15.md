# Day 15: Chiton
We are given a map and need to compute the shortest path from the top left to the right bottom. The algorithm to use here is [Dijkstra's algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm).

I implemented two versions: one for abstract cases, and one on a grid.

``` {.haskell file=app/Dijkstra.hs}
{-# LANGUAGE TypeApplications #-}

module Dijkstra (minDist, minDistArray2) where

import RIO
import RIO.List.Partial (foldl1')
import qualified RIO.Set as Set
import RIO.Map ((!?))
import qualified RIO.Map as Map
import qualified Data.PQueue.Min as Q

<<dijkstra-imports>>
<<dijkstra-generic>>
<<dijkstra-array>>
```

To put elements on a priority-queue, I defined a `newtype` that sorts on the second element of a tuple.

``` {.haskell #dijkstra-generic}
newtype DistLoc i a = DistLoc (i, a)
    deriving (Eq)

instance (Ord a, Eq i) => Ord (DistLoc i a) where
    compare (DistLoc (_, x)) (DistLoc (_, y)) = compare x y

toLoc :: DistLoc i a -> i
toLoc (DistLoc (l, _)) = l
```

The generic algorithm looks rather horrible, I won't bother you with it.

``` {.haskell #dijkstra-generic .hide}
minDist :: forall n a. (Ord n, Ord a, Num a, Bounded a)
        => (n -> [n]) -> (n -> n -> Maybe a) -> n -> n -> a
minDist neighbours distance start end
  = go Set.empty (Q.singleton $ DistLoc (start, 0)) (Map.singleton start 0) start
  where go :: Set n -> Q.MinQueue (DistLoc n a) -> Map n a -> n -> a
        go visited marked dists pos
          | pos == end         = currentDist pos
          | pos `Set.member` visited = go visited marked'' dists (toLoc $ Q.findMin marked'')
          | otherwise          = go (pos `Set.insert` visited) marked' dists' (toLoc $ Q.findMin marked')
          where unvisitedNeighbours = filter (`Set.notMember` visited) (neighbours pos)
                currentDist node = fromMaybe maxBound (dists !? node)

                distLoc :: n -> Maybe (DistLoc n a)
                distLoc node = do
                    let v = currentDist node
                    x <- dists !? pos
                    dx <- distance pos node
                    return $ DistLoc (node, min v (x + dx))

                newDists = mapMaybe distLoc unvisitedNeighbours
                dists' = foldl' (\m (DistLoc (i, x)) -> Map.insert i x m) dists newDists

                marked' :: Q.MinQueue (DistLoc n a)
                marked' = Q.deleteMin marked <> Q.fromList (mapMaybe distLoc unvisitedNeighbours)
                marked'' = Q.deleteMin marked
```

### Array version
For our case with path-finding on a grid. We can have a much more efficient implementation than the generic one.

``` {.haskell #dijkstra-imports}
import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
```

The entire algorithm now runs in the `ST` monad, so that we can do array mutation.

``` {.haskell #dijkstra-array}
type Array2' r a = A.Array r Ix2 a
type Array2 a = Array2' A.U a

<<condM>>

minDistArray2 :: forall a. (Ord a, Num a, Bounded a, A.Unbox a, A.Manifest A.U a)
              => Array2 a -> (Ix2 -> [Ix2]) -> Ix2 -> Ix2 -> a
minDistArray2 cost neighbours start end = runST go
  where go :: forall s. ST s a
        go = do
          <<dijkstra-array-init>>

          let
              <<dijkstra-array-distloc>>
              <<dijkstra-array-recur>>

          recur Q.empty start
          where size = A.size cost
```

#### Init
The algorithm is initialised with the `unvisited` set, encoded as an array of bools, and the tentative distances, stored in another array (of int).

``` {.haskell #dijkstra-array-init}
unvisited <- A.newMArray @A.U @Bool size True
dist      <- A.newMArray @A.U @a size maxBound
A.write_ dist start 0
```

#### Estimating distance
Distance is estimated as the minimum of the last known estimate and the distance from the current node plus the confirmed total distance to the current node.

``` {.haskell #dijkstra-array-distloc}
distLoc :: Ix2 -> Ix2 -> ST s (DistLoc Ix2 a)
distLoc i j = do
    v <- A.readM dist j
    x <- A.readM dist i
    return $ DistLoc (j, min v (x + (cost A.! j)))
```

#### Recursion
The recursion keeps the priority-queue of nodes to visit. There are three cases:

* The node is the end node: we're done.
* The node was already visited: we can skip it.
* Otherwise, set the current node to visited, check the neighbours, compute distances for them, update the priority queue and recurse.

``` {.haskell #dijkstra-array-recur}
recur :: Q.MinQueue (DistLoc Ix2 a) -> Ix2 -> ST s a
recur q pos = condM
  [ (pure $ pos == end, A.readM dist end)
  , (A.readM unvisited pos, do
       A.write_ unvisited pos False
       unvisitedNeighbours <- filterM (A.readM unvisited) (neighbours pos)
       newDists <- mapM (distLoc pos) unvisitedNeighbours
       mapM_ (\(DistLoc (i, x)) -> A.write_ dist i x) newDists
       let q' = foldl' (flip Q.insert) (Q.deleteMin q) newDists
       recur q' (toLoc $ Q.findMin q'))
  , (otherwiseM, do
       let q' = Q.deleteMin q
       recur q' (toLoc $ Q.findMin q')) ]
```

Here `condM` is a little helper function to write monadic conditions.

``` {.haskell #condM}
condM :: (Monad m) => [(m Bool, m a)] -> m a
condM ((pred, action): cs) = do
    c <- pred
    if c then action else condM cs
condM [] = error "no matching conditions"

otherwiseM :: (Monad m) => m Bool
otherwiseM = pure True
```

### Solution

``` {.haskell file=app/Day15.hs}
module Day15 where

import RIO
import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
import Parsing (readInputParsing, digitArray)
import Dijkstra (minDistArray2, minDist)

<<parser-day15>>
<<solution-day15>>
<<run-solutions>>
```

We again reuse the parser from day 9.

``` {.haskell #parser-day15}
type Array2' r a = A.Array r A.Ix2 a
type Array2 a = Array2' A.U a

readInput :: (HasLogFunc env) => RIO env (Array2 Int)
readInput = readInputParsing "data/day15.txt" digitArray
```

With Dijkstra's algorithm in place, the solution is not too hard.

``` {.haskell #solution-day15}
neighbours :: (A.Unbox a) => Array2 a -> Ix2 -> [Ix2]
neighbours x i = filter (isJust . (x A.!?)) $ map (+ i) n
    where n = [-1 :. 0, 1 :. 0, 0 :. -1, 0 :. 1]

distance :: Array2 Int -> Ix2 -> Ix2 -> Maybe Int
distance x _ i = x A.!? i

endPoint :: Array2 Int -> Ix2
endPoint x = A.unSz (A.size x) - (1 :. 1)

solutionA :: Array2 Int -> Int
solutionA inp = minDistArray2 inp (neighbours inp) (0 :. 0) (endPoint inp)

scaleUp :: Array2 Int -> Array2 Int
scaleUp x = stack 2 (stack 1 x)
    where stack axis row = foldl' (\r t -> A.compute $ A.append' axis r t) row
                         $ map (\h -> A.map (inc h) row) [1..4]
          inc h x = (x - 1 + h) `mod` 9 + 1

solutionB :: Array2 Int -> Int
-- solutionB inp' = minDist (neighbours inp) (distance inp) (0 :. 0) (endPoint inp)
solutionB inp' = minDistArray2 inp (neighbours inp) (0 :. 0) (endPoint inp)
    where inp = scaleUp inp'
```

