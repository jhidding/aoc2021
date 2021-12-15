# Day 15: Chiton
We are given a map and need to compute the shortest path from the top left to the right bottom. The algorithm to use here is [Dijkstra's algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm).

``` {.haskell file=app/Dijkstra.hs}
{-# LANGUAGE TypeApplications #-}

module Dijkstra (minDist, minDistArray2) where

import RIO
import RIO.List.Partial (foldl1')
import qualified RIO.Set as Set
import RIO.Map ((!?))
import qualified RIO.Map as Map
import qualified Data.PQueue.Min as Q

import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Mutable as M

type Array2' r a = A.Array r Ix2 a
type Array2 a = Array2' A.U a

minOn :: Ord b => (a -> b) -> a -> a -> a
minOn f x y
    | f x < f y = x
    | otherwise = y

newtype DistLoc i a = DistLoc (i, a)
    deriving (Eq)

instance (Ord a, Eq i) => Ord (DistLoc i a) where
    compare (DistLoc (_, x)) (DistLoc (_, y)) = compare x y

toLoc :: DistLoc i a -> i
toLoc (DistLoc (l, _)) = l

condM :: (Monad m) => [(m Bool, m a)] -> m a
condM ((pred, action): cs) = do
    c <- pred
    if c then action else condM cs
condM [] = error "no matching conditions"

otherwiseM :: (Monad m) => m Bool
otherwiseM = pure True

minDistArray2 :: forall a. (Ord a, Num a, Bounded a, A.Unbox a, M.Manifest A.U a)
              => Array2 a -> (Ix2 -> [Ix2]) -> Ix2 -> Ix2 -> a
minDistArray2 cost neighbours start end = runST go
  where go :: forall s. ST s a
        go = do
          unvisited <- M.newMArray @A.U @Bool size True
          dist      <- M.newMArray @A.U @a size maxBound
          M.write_ dist start 0

          let
              distLoc :: Ix2 -> Ix2 -> ST s (DistLoc Ix2 a)
              distLoc i j = do
                  v <- A.readM dist j
                  x <- A.readM dist i
                  return $ DistLoc (j, min v (x + (cost A.! j)))

              recur :: Q.MinQueue (DistLoc Ix2 a) -> Ix2 -> ST s a
              recur q pos = condM
                [ (pure $ pos == end, A.readM dist end)
                , (A.readM unvisited pos, do
                     M.write_ unvisited pos False
                     unvisitedNeighbours <- filterM (A.readM unvisited) (neighbours pos)
                     newDists <- mapM (distLoc pos) unvisitedNeighbours
                     mapM_ (\(DistLoc (i, x)) -> M.write_ dist i x) newDists
                     let q' = foldl' (flip Q.insert) (Q.deleteMin q) newDists
                     recur q' (toLoc $ Q.findMin q'))
                , (otherwiseM, do
                     let q' = Q.deleteMin q
                     recur q' (toLoc $ Q.findMin q')) ]

          recur Q.empty start
          where size = A.size cost

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

                updateDist :: n -> Maybe a -> Maybe a
                updateDist node Nothing  = (+) <$> (dists !? pos) <*> distance pos node
                updateDist node x        = min <$> updateDist node Nothing <*> x

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

``` {.haskell file=app/Day15.hs}
module Day15 where

import RIO
import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
import Parsing (readInputParsing, digitArray)
import Dijkstra (minDistArray2, minDist)

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
solutionA inp = minDistArray2 inp (neighbours inp) (0 :. 0) (endPoint inp)

scaleUp :: Array2 Int -> Array2 Int
scaleUp x = stack rowM
    where rowM = foldl' (\r t -> A.compute $ A.append' 1 r t) x (map (\h -> A.map (inc h) x) [1..4])
          stack row = foldl' (\r t -> A.compute $ A.append' 2 r t) row (map (\h -> A.map (inc h) row) [1..4])
          inc h x = (x - 1 + h) `mod` 9 + 1

solutionB :: Array2 Int -> Int
-- solutionB inp' = minDist (neighbours inp) (distance inp) (0 :. 0) (endPoint inp)
solutionB inp' = minDistArray2 inp (neighbours inp) (0 :. 0) (endPoint inp)
    where inp = scaleUp inp'

<<run-solutions>>
```

``` {.haskell #parser-day15}
type Array2' r a = A.Array r A.Ix2 a
type Array2 a = Array2' A.U a

readInput :: (HasLogFunc env) => RIO env (Array2 Int)
readInput = readInputParsing "data/day15.txt" digitArray
```

