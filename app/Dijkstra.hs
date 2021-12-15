-- ~\~ language=Haskell filename=app/Dijkstra.hs
-- ~\~ begin <<lit/day15.md|app/Dijkstra.hs>>[0]
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
-- ~\~ end
